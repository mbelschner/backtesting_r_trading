#Mit diesem Script werden wir Feature Engineering machen

rm(list=ls())
gc()


# Packages ----------------------------------------------------------------

pacman::p_load(tidyverse,
               data.table,
               lubridate,
               foreach,
               doParallel,
               xts,
               TTR,
               Boruta,
               ranger,
               readr)


# Load File ---------------------------------------------------------------

input_path = file.path("C:", "Users", "maxib",
                       "OneDrive", "Dokumente", "Finance",
                       "capitalcom_backtesting", "api-data")

output_path = file.path("C:", "Users", "maxib",
                        "OneDrive", "Dokumente", "Finance",
                        "capitalcom_backtesting", "r_output")

#Daten laden und vorbereiten
df_raw <- read_csv(file.path(input_path, "GOLD_MINUTE_15_testdata.csv")) %>%
  # Stelle sicher, dass die Spaltennamen korrekt sind und die 'time' Spalte als Datum erkannt wird
  mutate(time = as.POSIXct(time, format="%Y-%m-%d %H:%M:%S")) %>%
  select(time, open, high, low, close, volume)


# Create Target Variable --------------------------------------------------

#Hier machen wir die Target Variable. Sie ist -1 wenn der Kurs um 3x den ATR sinkt und 1 wenn er um 3x den ATR steigt
#Sie ist 0 wenn der Kurs sich nicht genug verändert.

df_temp = df_raw
atr_n = 14
atr_multiplier = 3

df_temp$atr_val = ATR(df_temp[, c("high", "low", "close")], n = atr_n)[, "atr"]

# Setup parallel processing
n_cores <- parallel::detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Vorbereitungen
dt <- as.data.table(df_temp)
dt[, `:=`(
  hour = hour(time),
  minute = minute(time),
  date = as.Date(time)
)]
dt <- dt[!(hour >= 22 & minute >= 30)]
dt[, `:=`(
  target_up = close + atr_multiplier * atr_val,
  target_down = close - atr_multiplier * atr_val
)]

# Parallel processing nach Tagen
unique_dates <- unique(dt$date)

results <- foreach(
  current_date = unique_dates,
  .packages = c('data.table', 'lubridate'),
  .combine = rbind
) %dopar% {
  
  day_data <- dt[date == current_date]
  
  if (nrow(day_data) <= 1) return(NULL)
  
  end_of_day <- ymd(current_date) + hm("22:30")
  
  # Ergebnis-Vektoren
  y_vals <- rep(NA_real_, nrow(day_data))
  target_times <- rep(as.POSIXct(NA), nrow(day_data))
  indices <- day_data[, which = TRUE]
  
  for (i in seq_len(nrow(day_data) - 1)) {
    
    target_up <- day_data$target_up[i]
    target_down <- day_data$target_down[i]
    
    future_subset <- day_data[(i+1):.N][time <= end_of_day]
    
    if (nrow(future_subset) == 0) next
    
    hit_up <- which(future_subset$high >= target_up)[1]
    hit_down <- which(future_subset$low <= target_down)[1]
    
    if (!is.na(hit_up) && (is.na(hit_down) || hit_up < hit_down)) {
      y_vals[i] <- 1
      target_times[i] <- future_subset$time[hit_up]
    } else if (!is.na(hit_down) && (is.na(hit_up) || hit_down < hit_up)) {
      y_vals[i] <- -1
      target_times[i] <- future_subset$time[hit_down]
    } else {
      y_vals[i] <- 0
    }
  }
  
  data.table(
    index = indices,
    y_target_atr = y_vals,
    target_reached = target_times
  )
}

# Merge results zurück
dt[results$index, `:=`(
  y_target_atr = results$y_target_atr,
  target_reached = results$target_reached
)]

# Cleanup
stopCluster(cl)

df_temp <- as.data.frame(dt)


# Feature Engineering -----------------------------------------------------

cat("Starting Feature Engineering...\n")

# Konvertiere zu xts für technische Indikatoren
df_xts <- xts(df_temp[, c("open", "high", "low", "close", "volume")],
              order.by = df_temp$time)

# Initialisiere Feature DataFrame
df_features <- df_temp

# 1. Moving Averages (5, 10, 25, 50, 200)
cat("Calculating Moving Averages...\n")
df_features$ma_5 <- SMA(df_xts$close, n = 5)
df_features$ma_10 <- SMA(df_xts$close, n = 10)
df_features$ma_25 <- SMA(df_xts$close, n = 25)
df_features$ma_50 <- SMA(df_xts$close, n = 50)
df_features$ma_200 <- SMA(df_xts$close, n = 200)

# MA Crossovers
df_features$ma_5_10_cross <- df_features$ma_5 - df_features$ma_10
df_features$ma_10_25_cross <- df_features$ma_10 - df_features$ma_25
df_features$ma_50_200_cross <- df_features$ma_50 - df_features$ma_200

# Price relative to MAs
df_features$price_ma_5_ratio <- df_xts$close / df_features$ma_5
df_features$price_ma_10_ratio <- df_xts$close / df_features$ma_10
df_features$price_ma_50_ratio <- df_xts$close / df_features$ma_50
df_features$price_ma_200_ratio <- df_xts$close / df_features$ma_200

# 2. ADX (Average Directional Index)
cat("Calculating ADX...\n")
adx_vals <- ADX(df_xts[, c("high", "low", "close")], n = 14)
df_features$adx <- adx_vals[, "ADX"]
df_features$di_plus <- adx_vals[, "DIp"]
df_features$di_minus <- adx_vals[, "DIn"]

# 3. Parabolic SAR
cat("Calculating Parabolic SAR...\n")
sar_vals <- SAR(df_xts[, c("high", "low")], accel = c(0.02, 0.2))
df_features$sar <- sar_vals
df_features$price_sar_diff <- df_xts$close - sar_vals

# 4. MACD
cat("Calculating MACD...\n")
macd_vals <- MACD(df_xts$close, nFast = 12, nSlow = 26, nSig = 9, maType = "EMA")
df_features$macd <- macd_vals[, "macd"]
df_features$macd_signal <- macd_vals[, "signal"]
df_features$macd_histogram <- df_features$macd - df_features$macd_signal

# 5. RSI (Relative Strength Index)
cat("Calculating RSI...\n")
df_features$rsi_14 <- RSI(df_xts$close, n = 14)
df_features$rsi_7 <- RSI(df_xts$close, n = 7)
df_features$rsi_21 <- RSI(df_xts$close, n = 21)

# 6. Stochastic Oscillator
cat("Calculating Stochastic Oscillator...\n")
stoch_vals <- stoch(df_xts[, c("high", "low", "close")],
                    nFastK = 14, nFastD = 3, nSlowD = 3)
df_features$stoch_k <- stoch_vals[, "fastK"]
df_features$stoch_d <- stoch_vals[, "fastD"]
df_features$stoch_slowd <- stoch_vals[, "slowD"]

# 7. Bollinger Bands
cat("Calculating Bollinger Bands...\n")
bb_vals <- BBands(df_xts$close, n = 20, sd = 2)
df_features$bb_upper <- bb_vals[, "up"]
df_features$bb_middle <- bb_vals[, "mavg"]
df_features$bb_lower <- bb_vals[, "dn"]
df_features$bb_pctB <- bb_vals[, "pctB"]  # Position within bands

# 8. Bollinger Bandwidth
df_features$bb_bandwidth <- (df_features$bb_upper - df_features$bb_lower) / df_features$bb_middle

# 9. Ichimoku Cloud
cat("Calculating Ichimoku Cloud...\n")
# Tenkan-sen (Conversion Line): (9-period high + 9-period low)/2
tenkan_high <- runMax(df_xts$high, n = 9)
tenkan_low <- runMin(df_xts$low, n = 9)
df_features$ichimoku_tenkan <- (tenkan_high + tenkan_low) / 2

# Kijun-sen (Base Line): (26-period high + 26-period low)/2
kijun_high <- runMax(df_xts$high, n = 26)
kijun_low <- runMin(df_xts$low, n = 26)
df_features$ichimoku_kijun <- (kijun_high + kijun_low) / 2

# Senkou Span A (Leading Span A): (Tenkan-sen + Kijun-sen)/2
df_features$ichimoku_senkou_a <- (df_features$ichimoku_tenkan + df_features$ichimoku_kijun) / 2

# Senkou Span B (Leading Span B): (52-period high + 52-period low)/2
senkou_high <- runMax(df_xts$high, n = 52)
senkou_low <- runMin(df_xts$low, n = 52)
df_features$ichimoku_senkou_b <- (senkou_high + senkou_low) / 2

# Chikou Span (Lagging Span): Current close shifted back 26 periods
df_features$ichimoku_chikou <- c(rep(NA, 26), df_xts$close[1:(nrow(df_xts)-26)])

# Cloud thickness
df_features$ichimoku_cloud_thickness <- abs(df_features$ichimoku_senkou_a - df_features$ichimoku_senkou_b)

# Price relative to cloud
df_features$price_above_cloud <- ifelse(
  df_xts$close > pmax(df_features$ichimoku_senkou_a, df_features$ichimoku_senkou_b), 1,
  ifelse(df_xts$close < pmin(df_features$ichimoku_senkou_a, df_features$ichimoku_senkou_b), -1, 0)
)

# 10. CCI (Commodity Channel Index)
cat("Calculating CCI...\n")
df_features$cci_20 <- CCI(df_xts[, c("high", "low", "close")], n = 20)
df_features$cci_14 <- CCI(df_xts[, c("high", "low", "close")], n = 14)

# 11. Aroon Indicator
cat("Calculating Aroon Indicator...\n")
aroon_vals <- aroon(df_xts[, c("high", "low")], n = 25)
df_features$aroon_up <- aroon_vals[, "aroonUp"]
df_features$aroon_down <- aroon_vals[, "aroonDn"]
df_features$aroon_oscillator <- aroon_vals[, "oscillator"]

# Additional useful features
cat("Calculating additional features...\n")

# Volume features
df_features$volume_sma_20 <- SMA(df_xts$volume, n = 20)
df_features$volume_ratio <- df_xts$volume / df_features$volume_sma_20

# Price momentum
df_features$roc_10 <- ROC(df_xts$close, n = 10)  # Rate of Change
df_features$roc_20 <- ROC(df_xts$close, n = 20)
df_features$momentum_14 <- momentum(df_xts$close, n = 14)

# Volatility
df_features$atr_14 <- ATR(df_xts[, c("high", "low", "close")], n = 14)[, "atr"]
df_features$atr_ratio <- df_features$atr_14 / df_xts$close

# Price range features
df_features$high_low_range <- df_xts$high - df_xts$low
df_features$close_open_diff <- df_xts$close - df_xts$open

cat("Feature Engineering completed. Total features:", ncol(df_features) - ncol(df_temp), "\n")


# Data Preparation for Modeling -------------------------------------------

cat("\nPreparing data for modeling...\n")

# Entferne Zeilen mit NA in der Zielvariable
df_model <- df_features %>%
  filter(!is.na(y_target_atr))

cat("Rows after removing NA targets:", nrow(df_model), "\n")

# Wähle relevante Spalten aus (alle Features außer Metadaten)
# Entferne: time, date, hour, minute, target_up, target_down, target_reached
feature_cols <- setdiff(
  names(df_model),
  c("time", "date", "hour", "minute", "target_up", "target_down",
    "target_reached", "y_target_atr")
)

# Erstelle finalen Dataset mit Features und Target
df_final <- df_model %>%
  select(all_of(c(feature_cols, "y_target_atr"))) %>%
  # Entferne Zeilen mit NA in Features (wichtig für frühe Beobachtungen)
  na.omit()

cat("Rows after removing NA features:", nrow(df_final), "\n")
cat("Total features:", length(feature_cols), "\n")


# Train/Test Split (85% Train, 15% Test) ---------------------------------

cat("\nCreating Train/Test Split...\n")

# Zeitbasierter Split - letzte 15% als Test
n_total <- nrow(df_final)
split_point <- floor(n_total * 0.85)

df_train <- df_final[1:split_point, ]
df_test <- df_final[(split_point + 1):n_total, ]

cat("Training set size:", nrow(df_train), "\n")
cat("Test set size:", nrow(df_test), "\n")
cat("Class distribution in training set:\n")
print(table(df_train$y_target_atr))
cat("Class distribution in test set:\n")
print(table(df_test$y_target_atr))


# Boruta Feature Selection ------------------------------------------------

cat("\nStarting Boruta Feature Selection...\n")
cat("This may take a while...\n")

# Konvertiere Target zu Factor für Boruta
df_train_boruta <- df_train
df_train_boruta$y_target_atr <- as.factor(df_train_boruta$y_target_atr)

# Führe Boruta aus
# maxRuns limitieren um Overfitting zu vermeiden
set.seed(123)  # Für Reproduzierbarkeit
boruta_output <- Boruta(
  y_target_atr ~ .,
  data = df_train_boruta,
  doTrace = 2,
  maxRuns = 100,  # Limitiere Iterationen
  num.trees = 500  # Für ranger
)

cat("\nBoruta Feature Selection Results:\n")
print(boruta_output)

# Plot Boruta Ergebnisse
pdf(file.path(getwd(), "boruta_feature_importance.pdf"), width = 12, height = 8)
plot(boruta_output, las = 2, cex.axis = 0.7,
     xlab = "", main = "Boruta Feature Importance")
dev.off()

cat("\nBoruta plot saved to: boruta_feature_importance.pdf\n")

# Hole bestätigte und tentative Features
boruta_confirmed <- getSelectedAttributes(boruta_output, withTentative = FALSE)
boruta_tentative <- getSelectedAttributes(boruta_output, withTentative = TRUE)

cat("\nConfirmed important features:", length(boruta_confirmed), "\n")
cat(boruta_confirmed, sep = "\n")

if (length(boruta_tentative) > length(boruta_confirmed)) {
  cat("\nTentative features (zusätzlich):",
      length(boruta_tentative) - length(boruta_confirmed), "\n")
}

# TentativeRoughFix für bessere Entscheidung
boruta_final <- TentativeRoughFix(boruta_output)
boruta_selected <- getSelectedAttributes(boruta_final, withTentative = FALSE)

cat("\nFinal selected features:", length(boruta_selected), "\n")


# Create Final Datasets with Selected Features ---------------------------

cat("\nCreating final datasets with selected features...\n")

# Finaler Train und Test mit ausgewählten Features
df_train_final <- df_train %>%
  select(all_of(c(boruta_selected, "y_target_atr")))

df_test_final <- df_test %>%
  select(all_of(c(boruta_selected, "y_target_atr")))

cat("Final training set dimensions:", dim(df_train_final), "\n")
cat("Final test set dimensions:", dim(df_test_final), "\n")


# Save Results ------------------------------------------------------------

cat("\nSaving results...\n")

# Speichere Boruta Objekt
saveRDS(boruta_final, file.path(getwd(), "boruta_model.rds"))

# Speichere Train/Test Datasets
write_csv(df_train_final, file.path(getwd(), "train_data.csv"))
write_csv(df_test_final, file.path(getwd(), "test_data.csv"))

# Speichere auch die Feature Namen
writeLines(boruta_selected, file.path(getwd(), "selected_features.txt"))

# Speichere Feature Importance
feature_importance <- data.frame(
  feature = names(boruta_final$finalDecision),
  decision = as.character(boruta_final$finalDecision),
  importance = boruta_final$ImpHistory[nrow(boruta_final$ImpHistory),
                                        names(boruta_final$finalDecision)]
) %>%
  arrange(desc(importance))

#write_csv(feature_importance, file.path(getwd(), "feature_importance.csv"))

cat("\nResults saved:\n")
cat("- boruta_model.rds (Boruta model object)\n")
cat("- train_data.csv (Training data with selected features)\n")
cat("- test_data.csv (Test data with selected features)\n")
cat("- selected_features.txt (List of selected features)\n")
cat("- feature_importance.csv (Feature importance scores)\n")
cat("- boruta_feature_importance.pdf (Feature importance plot)\n")

cat("\n=== Feature Engineering Pipeline Completed Successfully ===\n")

