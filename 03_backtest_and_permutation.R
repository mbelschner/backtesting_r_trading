################################################################################
# ICHIMOKU TRADING STRATEGIE - LINEARES SCRIPT (OHNE FUNKTIONEN)
# Angepasst für 15-MINUTEN GOLD DATEN
################################################################################
# Vollständig sequenzielles Script - kein function() Aufruf
# Alles läuft von oben nach unten durch
################################################################################

rm(list=ls())
gc()

cat("\n")
cat("================================================================================\n")
cat("         ICHIMOKU TRADING STRATEGIE - BACKTEST & OPTIMIERUNG\n")
cat("         15-Minuten GOLD Daten\n")
cat("================================================================================\n\n")

# =============================================================================
# PAKETE LADEN
# =============================================================================
cat("Lade Pakete...\n")

suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
  library(lubridate)
  library(foreach)
  library(doParallel)
  library(TTR)
  library(PerformanceAnalytics)
})

cat("✓ Pakete geladen\n\n")

# =============================================================================
# KONFIGURATION
# =============================================================================
cat("Setze Konfiguration...\n")

# Reproduzierbarkeit
set.seed(123)

# Datenpfade
input_path <- file.path("C:/Users/maxib/OneDrive/Dokumente/Finance/capitalcom_backtesting", "api-data")
output_path <- file.path("C:/Users/maxib/OneDrive/Dokumente/Finance/R ML Trading Complete", "labelled_data")
EPIC <- "GOLD"
INTERVAL <- "MINUTE_15"
filename <- paste0(EPIC, "_", INTERVAL, ".csv")

# Backtesting Parameter (angepasst für 15min)
initial_capital <- 2000
position_size <- 0.5  # 10% des Kapitals pro Trade
exit_hour <- 22        # 22:00 Uhr Time Exit
ATR_Length <- 14       # Länge ATR

# Train/Test Split
train_split <- 0.70

# Permutationstest
n_permutations_train <- 200
n_permutations_test <- 200
block_size <- 96  # 96 Bars = 24 Stunden bei 15min Daten

# Parallele Verarbeitung
n_cores <- detectCores() - 1

cat("✓ Konfiguration gesetzt\n")
cat("  Symbol:", EPIC, "\n")
cat("  Interval:", INTERVAL, "\n")
cat("  Block Size:", block_size, "Bars (24h bei 15min)\n\n")

# =============================================================================
# DATEN LADEN
# =============================================================================
cat("Lade Daten...\n")
cat("  Pfad:", file.path(input_path, filename), "\n")

# Daten laden
data_raw <- read_csv(file.path(input_path, filename), show_col_types = FALSE)

cat("✓ Rohdaten geladen:", nrow(data_raw), "Zeilen\n")
cat("  Spalten:", paste(names(data_raw), collapse = ", "), "\n\n")

# Daten vorbereiten - flexible Spaltenerkennung
cat("Bereite Daten vor...\n")

# Spaltennamen standardisieren (verschiedene Formate unterstützen)
col_names_lower <- tolower(names(data_raw))

# Mapping für verschiedene Spaltenbezeichnungen
if("snapshottime" %in% col_names_lower) {
  time_col <- names(data_raw)[col_names_lower == "snapshottime"]
} else if("timestamp" %in% col_names_lower) {
  time_col <- names(data_raw)[col_names_lower == "timestamp"]
} else if("datetime" %in% col_names_lower) {
  time_col <- names(data_raw)[col_names_lower == "datetime"]
} else if("time" %in% col_names_lower) {
  time_col <- names(data_raw)[col_names_lower == "time"]
} else {
  time_col <- names(data_raw)[1]  # Erste Spalte als Fallback
}

# OHLC Spalten finden
if("openprice" %in% col_names_lower) {
  open_col <- names(data_raw)[col_names_lower == "openprice"]
  high_col <- names(data_raw)[col_names_lower == "highprice"]
  low_col <- names(data_raw)[col_names_lower == "lowprice"]
  close_col <- names(data_raw)[col_names_lower == "closeprice"]
} else if("open" %in% col_names_lower) {
  open_col <- names(data_raw)[col_names_lower == "open"]
  high_col <- names(data_raw)[col_names_lower == "high"]
  low_col <- names(data_raw)[col_names_lower == "low"]
  close_col <- names(data_raw)[col_names_lower == "close"]
} else {
  # Fallback: Annahme Standard-Reihenfolge
  open_col <- names(data_raw)[2]
  high_col <- names(data_raw)[3]
  low_col <- names(data_raw)[4]
  close_col <- names(data_raw)[5]
}

cat("  Erkannte Spalten:\n")
cat("    Zeit:", time_col, "\n")
cat("    Open:", open_col, "\n")
cat("    High:", high_col, "\n")
cat("    Low:", low_col, "\n")
cat("    Close:", close_col, "\n\n")

# Daten standardisieren
data <- data_raw %>%
  select(
    datetime = all_of(time_col),
    open = all_of(open_col),
    high = all_of(high_col),
    low = all_of(low_col),
    close = all_of(close_col)
  ) %>%
  mutate(
    # Datetime konvertieren (verschiedene Formate unterstützen)
    datetime = case_when(
      is.character(datetime) ~ as.POSIXct(datetime, format = "%Y-%m-%dT%H:%M:%S"),
      is.POSIXct(datetime) ~ datetime,
      TRUE ~ as.POSIXct(as.character(datetime))
    ),
    # Datum extrahieren
    date = as.Date(datetime)
  ) %>%
  arrange(datetime) %>%
  filter(!is.na(datetime), !is.na(close))

cat("✓ Daten vorbereitet:", nrow(data), "Bars\n")
cat("  Zeitraum:", min(data$datetime), "bis", max(data$datetime), "\n")
cat("  Von:", min(data$date), "bis", max(data$date), "\n\n")

# Prüfe auf fehlende Daten
if(any(is.na(data$open)) || any(is.na(data$high)) || 
   any(is.na(data$low)) || any(is.na(data$close))) {
  cat("⚠ WARNUNG: Fehlende Werte in OHLC gefunden - entferne diese Zeilen\n")
  data <- data %>% filter(!is.na(open), !is.na(high), !is.na(low), !is.na(close))
  cat("  Verbleibende Bars:", nrow(data), "\n\n")
}

# =============================================================================
# TRAIN/TEST SPLIT
# =============================================================================
cat("Teile Daten in Train/Test...\n")

split_idx <- floor(nrow(data) * train_split)
data_train <- data[1:split_idx, ]
data_test <- data[(split_idx + 1):nrow(data), ]

cat("✓ Train Periode:", min(data_train$datetime), "bis", max(data_train$datetime), "\n")
cat("  (", nrow(data_train), "Bars = ca.", round(nrow(data_train)/96, 1), "Tage )\n")
cat("✓ Test Periode: ", min(data_test$datetime), "bis", max(data_test$datetime), "\n")
cat("  (", nrow(data_test), "Bars = ca.", round(nrow(data_test)/96, 1), "Tage )\n\n")

# =============================================================================
# PARAMETER-OPTIMIERUNG AUF TRAINING DATA
# =============================================================================
cat("========================================\n")
cat("PARAMETER-OPTIMIERUNG (15min Daten)\n")
cat("========================================\n\n")

# Parameter Grid für 15-Minuten Daten angepasst
# Bei 15min: 4 Bars = 1 Stunde, 96 Bars = 24 Stunden
param_grid <- expand.grid(
  tenkan = c(8,16),          
  kijun = c(20,26),          
  senkou_b = c(54,60,66),      
  displacement = c(12,15,18),    
  sl_atr = c(2.0, 3.0),     
  tp_atr = c(3.0, 4.0),    
  spread = 0.3           
)

# Filter: Kijun > Tenkan, Senkou_B > Kijun
param_grid <- param_grid %>%
  filter(kijun > tenkan, senkou_b > kijun, tp_atr > sl_atr)

cat("Anzahl Parameter-Kombinationen:", nrow(param_grid), "\n")
cat("Nutze", n_cores, "CPU-Kerne...\n")
cat("Angepasst für 15-Minuten Intraday Trading\n\n")

# Parallele Verarbeitung Setup
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Parallele Optimierung
cat("Starte parallele Optimierung...\n")
start_time = Sys.time()

optimization_results <- foreach(idx = 1:nrow(param_grid),
                                .combine = 'rbind',
                                .packages = c('tidyverse', 'TTR', 'lubridate', 'data.table'),
                                .errorhandling = 'remove') %dopar% {
                                  
                                  # Parameter
                                  p_tenkan <- param_grid$tenkan[idx]
                                  p_kijun <- param_grid$kijun[idx]
                                  p_senkou_b <- param_grid$senkou_b[idx]
                                  p_displacement <- param_grid$displacement[idx]
                                  p_sl_atr <- param_grid$sl_atr[idx]
                                  p_tp_atr <- param_grid$tp_atr[idx]
                                  p_spread <- param_grid$spread[idx]
                                  
                                  # Arbeite direkt mit data.table für Geschwindigkeit
                                  train_data <- as.data.table(data_train)
                                  
                                  # === ICHIMOKU BERECHNEN (VECTORIZED) ===
                                  
                                  train_data[, `:=`(
                                    tenkan = (runMax(high, p_tenkan) + runMin(low, p_tenkan)) / 2,
                                    kijun = (runMax(high, p_kijun) + runMin(low, p_kijun)) / 2
                                  )]
                                  
                                  senkou_a <- (train_data$tenkan + train_data$kijun) / 2
                                  senkou_b <- (runMax(train_data$high, p_senkou_b) + runMin(train_data$low, p_senkou_b)) / 2
                                  
                                  train_data[, `:=`(
                                    senkou_a_displaced = shift(senkou_a, p_displacement - 1, type = "lag"),
                                    senkou_b_displaced = shift(senkou_b, p_displacement - 1, type = "lag"),
                                    atr = ATR(cbind(high, low, close), n = ATR_Length)[, "atr"]
                                  )]
                                  
                                  train_data[, `:=`(
                                    kumo_high = pmax(senkou_a_displaced, senkou_b_displaced, na.rm = TRUE),
                                    kumo_low = pmin(senkou_a_displaced, senkou_b_displaced, na.rm = TRUE)
                                  )]
                                  
                                  train_data[, `:=`(
                                    price_above_kumo = close > kumo_high,
                                    price_below_kumo = close < kumo_low,
                                    chikou_free_long = close > shift(high, p_displacement - 1, type = "lag"),
                                    chikou_free_short = close < shift(low, p_displacement - 1, type = "lag")
                                  )]
                                  
                                  # === SIGNALE GENERIEREN (VECTORIZED) ===
                                  
                                  train_data[, `:=`(
                                    tk_cross_up = tenkan > kijun & shift(tenkan, 1) <= shift(kijun, 1),
                                    tk_cross_down = tenkan < kijun & shift(tenkan, 1) >= shift(kijun, 1)
                                  )]
                                  
                                  train_data[, `:=`(
                                    signal_long = tk_cross_up & price_above_kumo & chikou_free_long,
                                    signal_short = tk_cross_down & price_below_kumo & chikou_free_short
                                  )]
                                  
                                  train_data[is.na(signal_long), signal_long := FALSE]
                                  train_data[is.na(signal_short), signal_short := FALSE]
                                  
                                  train_data[, hour := hour(datetime)]
                                  train_data[, time_exit := hour >= exit_hour]
                                  
                                  # === BACKTEST VECTORIZED ===
                                  
                                  n <- nrow(train_data)
                                  spread_cost <- p_spread
                                  
                                  # Finde alle Entry-Signale
                                  long_entries <- which(train_data$signal_long & !train_data$time_exit)
                                  short_entries <- which(train_data$signal_short & !train_data$time_exit)
                                  
                                  # Pre-allocate trade results
                                  trades_list <- vector("list", length(long_entries) + length(short_entries))
                                  trade_count <- 0
                                  
                                  # Verarbeite Long Trades
                                  for(entry_idx in long_entries) {
                                    entry_price <- train_data$close[entry_idx]
                                    sl <- entry_price - (train_data$atr[entry_idx] * p_sl_atr + spread_cost)
                                    tp <- entry_price + (train_data$atr[entry_idx] * p_tp_atr + spread_cost)
                                    
                                    # Finde Exit (vectorized)
                                    future_bars <- train_data[(entry_idx + 1):min(entry_idx + 500, n)]
                                    
                                    sl_hit <- which(future_bars$low <= sl)[1]
                                    tp_hit <- which(future_bars$high >= tp)[1]
                                    time_hit <- which(future_bars$time_exit)[1]
                                    
                                    exit_bar <- min(sl_hit, tp_hit, time_hit, na.rm = TRUE)
                                    
                                    if(!is.infinite(exit_bar)) {
                                      exit_idx <- entry_idx + exit_bar
                                      
                                      if(!is.na(sl_hit) && sl_hit == exit_bar) {
                                        exit_price <- sl
                                      } else if(!is.na(tp_hit) && tp_hit == exit_bar) {
                                        exit_price <- tp
                                      } else {
                                        exit_price <- train_data$close[exit_idx]
                                      }
                                      
                                      pnl <- (exit_price - entry_price) - spread_cost
                                      
                                      trade_count <- trade_count + 1
                                      trades_list[[trade_count]] <- list(
                                        pnl = pnl,
                                        entry_idx = entry_idx,
                                        exit_idx = exit_idx
                                      )
                                    }
                                  }
                                  
                                  # Verarbeite Short Trades (analog)
                                  for(entry_idx in short_entries) {
                                    entry_price <- train_data$close[entry_idx]
                                    sl <- entry_price + (train_data$atr[entry_idx] * p_sl_atr + spread_cost)
                                    tp <- entry_price - (train_data$atr[entry_idx] * p_tp_atr + spread_cost)
                                    
                                    future_bars <- train_data[(entry_idx + 1):min(entry_idx + 500, n)]
                                    
                                    sl_hit <- which(future_bars$high >= sl)[1]
                                    tp_hit <- which(future_bars$low <= tp)[1]
                                    time_hit <- which(future_bars$time_exit)[1]
                                    
                                    exit_bar <- min(sl_hit, tp_hit, time_hit, na.rm = TRUE)
                                    
                                    if(!is.infinite(exit_bar)) {
                                      exit_idx <- entry_idx + exit_bar
                                      
                                      if(!is.na(sl_hit) && sl_hit == exit_bar) {
                                        exit_price <- sl
                                      } else if(!is.na(tp_hit) && tp_hit == exit_bar) {
                                        exit_price <- tp
                                      } else {
                                        exit_price <- train_data$close[exit_idx]
                                      }
                                      
                                      pnl <- (entry_price - exit_price) - spread_cost
                                      
                                      trade_count <- trade_count + 1
                                      trades_list[[trade_count]] <- list(
                                        pnl = pnl,
                                        entry_idx = entry_idx,
                                        exit_idx = exit_idx
                                      )
                                    }
                                  }
                                  
                                  # === EQUITY CURVE BERECHNEN (VECTORIZED) ===
                                  
                                  if(trade_count > 0) {
                                    trades_list <- trades_list[1:trade_count]
                                    
                                    # Berechne PnL in Dollar
                                    equity_curve <- rep(initial_capital, n)
                                    
                                    for(t in trades_list) {
                                      trade_size <- equity_curve[t$entry_idx] * position_size
                                      entry_price_val <- train_data$close[t$entry_idx]
                                      contracts <- trade_size / entry_price_val
                                      pnl_dollar <- t$pnl * contracts
                                      
                                      # Update equity ab exit
                                      if(t$exit_idx <= n) {
                                        equity_curve[(t$exit_idx):n] <- equity_curve[(t$exit_idx):n] + pnl_dollar
                                      }
                                    }
                                    
                                    # Performance Metriken
                                    returns <- c(0, diff(equity_curve) / equity_curve[-n])
                                    returns[is.infinite(returns) | is.nan(returns)] <- 0
                                    
                                    total_return <- (equity_curve[n] / initial_capital - 1) * 100
                                    n_trades <- trade_count
                                    pnl_values <- sapply(trades_list, function(x) x$pnl)
                                    win_rate <- sum(pnl_values > 0) / n_trades * 100
                                    
                                    sharpe_ratio <- mean(returns, na.rm = TRUE) / sd(returns, na.rm = TRUE) * sqrt(96 * 252)
                                    
                                    cum_returns <- cumprod(1 + returns)
                                    running_max <- cummax(cum_returns)
                                    drawdown <- (cum_returns - running_max) / running_max
                                    max_drawdown <- min(drawdown, na.rm = TRUE) * 100
                                    
                                  } else {
                                    total_return <- 0
                                    n_trades <- 0
                                    win_rate <- 0
                                    sharpe_ratio <- 0
                                    max_drawdown <- 0
                                  }
                                  
                                  data.frame(
                                    tenkan = p_tenkan,
                                    kijun = p_kijun,
                                    senkou_b = p_senkou_b,
                                    displacement = p_displacement,
                                    sl_atr = p_sl_atr,
                                    tp_atr = p_tp_atr,
                                    spread = p_spread,
                                    total_return = total_return,
                                    n_trades = n_trades,
                                    win_rate = win_rate,
                                    sharpe_ratio = sharpe_ratio,
                                    max_drawdown = max_drawdown
                                  )
                                }

stopCluster(cl)

exit_time = Sys.time()
elapsed_features <- as.numeric(difftime(exit_time, start_time, units = "secs"))

cat("✓ Feature Engineering abgeschlossen in", round(elapsed_features, 2), "Sekunden\n")
cat("\n✓ Optimierung abgeschlossen!\n\n")

# Beste Parameter finden
optimization_results <- optimization_results %>%
  arrange(desc(sharpe_ratio)) %>%
  mutate(rank = row_number())

optis = optimization_results %>%
  filter(rank <= 30) %>%
  summarise(across(c(tenkan, kijun, senkou_b, displacement), ~ {
    tab <- table(.x)
    names(tab)[which.max(tab)]
  }))

cat("=== TOP 5 PARAMETER-KOMBINATIONEN ===\n")
cat("(Perioden in 15-Minuten Bars)\n")
print(head(optimization_results %>% 
             select(rank, tenkan, kijun, senkou_b, displacement, sl_atr, tp_atr, 
                    sharpe_ratio, total_return, win_rate), 5))

best_params = optimization_results %>%
  filter(rank == 1)

# Speichern
#write_csv(best_params, file.path(output_path, "best_parameters.csv"))
#write_csv(optimization_results, file.path(output_path, "optimization_results.csv"))

#cat("✓ Ergebnisse gespeichert in:", output_path, "\n\n")

# =============================================================================
# BACKTEST AUF TEST DATA MIT OPTIMALEN PARAMETERN
# =============================================================================
cat("========================================\n")
cat("TEST AUF OUT-OF-SAMPLE DATEN\n")
cat("========================================\n\n")

opt_tenkan <- best_params$tenkan
opt_kijun <- best_params$kijun
opt_senkou_b <- best_params$senkou_b
opt_displacement <- best_params$displacement
opt_sl_atr <- best_params$sl_atr
opt_tp_atr <- best_params$tp_atr
opt_spread <- best_params$spread

# === ICHIMOKU AUF TEST DATA ===

test_data <- data_test

test_data$tenkan <- (runMax(test_data$high, opt_tenkan) + 
                       runMin(test_data$low, opt_tenkan)) / 2

test_data$kijun <- (runMax(test_data$high, opt_kijun) + 
                      runMin(test_data$low, opt_kijun)) / 2

senkou_a_test <- (test_data$tenkan + test_data$kijun) / 2

senkou_b_test <- (runMax(test_data$high, opt_senkou_b) + 
                    runMin(test_data$low, opt_senkou_b)) / 2

test_data$senkou_a_displaced <- lag(senkou_a_test, opt_displacement - 1)
test_data$senkou_b_displaced <- lag(senkou_b_test, opt_displacement - 1)

test_data$kumo_high <- pmax(test_data$senkou_a_displaced, 
                            test_data$senkou_b_displaced, na.rm = TRUE)
test_data$kumo_low <- pmin(test_data$senkou_a_displaced, 
                           test_data$senkou_b_displaced, na.rm = TRUE)

test_data$price_above_kumo <- test_data$close > test_data$kumo_high
test_data$price_below_kumo <- test_data$close < test_data$kumo_low

test_data$chikou_free_long <- test_data$close > lag(test_data$high, opt_displacement - 1)
test_data$chikou_free_short <- test_data$close < lag(test_data$low, opt_displacement - 1)

test_data$atr <- ATR(test_data[, c("high", "low", "close")], n = ATR_Length)[, "atr"]

# === SIGNALE ===

test_data$tk_cross_up <- test_data$tenkan > test_data$kijun & 
  lag(test_data$tenkan) <= lag(test_data$kijun)
test_data$tk_cross_down <- test_data$tenkan < test_data$kijun & 
  lag(test_data$tenkan) >= lag(test_data$kijun)

test_data$signal_long <- test_data$tk_cross_up & 
  test_data$price_above_kumo & 
  test_data$chikou_free_long

test_data$signal_short <- test_data$tk_cross_down & 
  test_data$price_below_kumo & 
  test_data$chikou_free_short

test_data$signal_long[is.na(test_data$signal_long)] <- FALSE
test_data$signal_short[is.na(test_data$signal_short)] <- FALSE

# === BACKTEST ===

n_test <- nrow(test_data)
position_test <- rep(0, n_test)
entry_price_test <- rep(NA_real_, n_test)
entry_bar_test <- rep(NA_integer_, n_test)  # Neue Variable für Entry-Bar
stop_loss_test <- rep(NA_real_, n_test)
take_profit_test <- rep(NA_real_, n_test)
equity_test <- rep(initial_capital, n_test)

spread_cost_test <- opt_spread

test_data$hour <- hour(test_data$datetime)
time_exit_trigger_test <- test_data$hour >= exit_hour

trades_test <- list()

cat("Führe Backtest durch...\n")

for(i in 2:n_test) {
  equity_test[i] <- equity_test[i-1]
  
  # ENTRY LOGIC
  if(position_test[i-1] == 0) {
    if(test_data$signal_long[i] && !time_exit_trigger_test[i]) {
      position_test[i] <- 1
      entry_price_test[i] <- test_data$close[i]
      entry_bar_test[i] <- i  # Merke Entry-Bar
      sl_offset <- test_data$atr[i] * opt_sl_atr + spread_cost_test
      tp_offset <- test_data$atr[i] * opt_tp_atr + spread_cost_test
      stop_loss_test[i] <- entry_price_test[i] - sl_offset
      take_profit_test[i] <- entry_price_test[i] + tp_offset
      
      # WICHTIG: Nach Entry NICHT in gleicher Bar Exit prüfen!
      next  # Springe zur nächsten Iteration
      
    } 
    else if(test_data$signal_short[i] && !time_exit_trigger_test[i]) {
      position_test[i] <- -1
      entry_price_test[i] <- test_data$close[i]
      entry_bar_test[i] <- i  # Merke Entry-Bar
      sl_offset <- test_data$atr[i] * opt_sl_atr + spread_cost_test
      tp_offset <- test_data$atr[i] * opt_tp_atr + spread_cost_test
      stop_loss_test[i] <- entry_price_test[i] + sl_offset
      take_profit_test[i] <- entry_price_test[i] - tp_offset
      
      # WICHTIG: Nach Entry NICHT in gleicher Bar Exit prüfen!
      next  # Springe zur nächsten Iteration
    }
  }
  
  # EXIT LOGIC (nur wenn Position bereits offen war)
  if(position_test[i-1] != 0) {
    
    # Position fortführen
    position_test[i] <- position_test[i-1]
    entry_price_test[i] <- entry_price_test[i-1]
    entry_bar_test[i] <- entry_bar_test[i-1]  # Entry-Bar fortführen
    stop_loss_test[i] <- stop_loss_test[i-1]
    take_profit_test[i] <- take_profit_test[i-1]
    
    exit_triggered <- FALSE
    exit_price <- test_data$close[i]
    exit_reason <- NA_character_
    
    # LONG Exit Checks
    if(position_test[i] == 1) {
      if(test_data$low[i] <= stop_loss_test[i]) {
        exit_triggered <- TRUE
        exit_price <- stop_loss_test[i]
        exit_reason <- "Stop Loss"
      } else if(test_data$high[i] >= take_profit_test[i]) {
        exit_triggered <- TRUE
        exit_price <- take_profit_test[i]
        exit_reason <- "Take Profit"
      } else if(time_exit_trigger_test[i]) {
        exit_triggered <- TRUE
        exit_reason <- "Time Exit"
      }
    }
    
    # SHORT Exit Checks
    if(position_test[i] == -1) {
      if(test_data$high[i] >= stop_loss_test[i]) {
        exit_triggered <- TRUE
        exit_price <- stop_loss_test[i]
        exit_reason <- "Stop Loss"
      } else if(test_data$low[i] <= take_profit_test[i]) {
        exit_triggered <- TRUE
        exit_price <- take_profit_test[i]
        exit_reason <- "Take Profit"
      } else if(time_exit_trigger_test[i]) {
        exit_triggered <- TRUE
        exit_reason <- "Time Exit"
      }
    }
    
    # Exit ausführen
    if(exit_triggered) {
      if(position_test[i] == 1) {
        pnl <- (exit_price - entry_price_test[i]) - spread_cost_test
      } else {
        pnl <- (entry_price_test[i] - exit_price) - spread_cost_test
      }
      
      trade_size <- equity_test[i-1] * position_size
      contracts <- trade_size / entry_price_test[i]
      pnl_dollar <- pnl * contracts
      
      equity_test[i] <- equity_test[i-1] + pnl_dollar
      
      trades_test[[length(trades_test) + 1]] <- data.frame(
        entry_date = test_data$datetime[entry_bar_test[i]],  # Nutze gespeicherte Entry-Bar
        exit_date = test_data$datetime[i],
        direction = ifelse(position_test[i-1] == 1, "Long", "Short"),
        entry_price = entry_price_test[i],
        exit_price = exit_price,
        pnl = pnl_dollar,
        exit_reason = exit_reason
      )
      
      position_test[i] <- 0
      entry_price_test[i] <- NA
      entry_bar_test[i] <- NA  # Zurücksetzen
      stop_loss_test[i] <- NA
      take_profit_test[i] <- NA
    }
  }
}

cat("✓ Backtest abgeschlossen\n\n")

# === PERFORMANCE ===

returns_test <- c(0, diff(equity_test) / equity_test[-n_test])

if(length(trades_test) > 0) {
  trades_test_df <- bind_rows(trades_test)
  total_return_test <- (equity_test[n_test] / initial_capital - 1) * 100
  n_trades_test <- nrow(trades_test_df)
  win_rate_test <- sum(trades_test_df$pnl > 0) / n_trades_test * 100
  avg_win_test <- mean(trades_test_df$pnl[trades_test_df$pnl > 0])
  avg_loss_test <- mean(trades_test_df$pnl[trades_test_df$pnl < 0])
  profit_factor_test <- abs(sum(trades_test_df$pnl[trades_test_df$pnl > 0]) / 
                              sum(trades_test_df$pnl[trades_test_df$pnl < 0]))
  sharpe_ratio_test <- mean(returns_test, na.rm = TRUE) / sd(returns_test, na.rm = TRUE) * sqrt(96 * 252)
  
  cum_returns_test <- cumprod(1 + returns_test)
  running_max_test <- cummax(cum_returns_test)
  drawdown_test <- (cum_returns_test - running_max_test) / running_max_test
  max_drawdown_test <- min(drawdown_test, na.rm = TRUE) * 100
} else {
  total_return_test <- 0
  n_trades_test <- 0
  win_rate_test <- 0
  avg_win_test <- 0
  avg_loss_test <- 0
  profit_factor_test <- 0
  sharpe_ratio_test <- 0
  max_drawdown_test <- 0
}

test_metrics <- data.frame(
  total_return = total_return_test,
  n_trades = n_trades_test,
  win_rate = win_rate_test,
  avg_win = avg_win_test,
  avg_loss = avg_loss_test,
  profit_factor = profit_factor_test,
  sharpe_ratio = sharpe_ratio_test,
  max_drawdown = max_drawdown_test
)

cat("=== OUT-OF-SAMPLE PERFORMANCE ===\n")
print(test_metrics)
cat("\n")

# write_csv(test_metrics, file.path(output_path, "performance_metrics.csv"))
# if(length(trades_test) > 0) {
#   write_csv(trades_test_df, file.path(output_path, "trades.csv"))
# }

# =============================================================================
# PERMUTATIONSTEST AUF TRAINING DATA
# =============================================================================
cat("========================================\n")
cat("PERMUTATIONSTEST (TRAIN)\n")
cat("========================================\n\n")

cat("Starte", n_permutations_train, "Permutationen auf Training Data...\n")
cat("Block Size:", block_size, "Bars (24 Stunden bei 15min)\n\n")
cat("Metrik: PROFIT FACTOR\n\n")

cl_perm <- makeCluster(n_cores)
registerDoParallel(cl_perm)

permuted_pf_train <- foreach(perm_idx = 1:n_permutations_train,
                             .combine = 'c',
                             .packages = c('tidyverse', 'TTR')) %dopar% {
                               
                               returns_original <- c(0, diff(log(data_train$close)))
                               
                               n_blocks <- ceiling(length(returns_original) / block_size)
                               block_indices <- sample(1:(length(returns_original) - block_size), 
                                                       n_blocks, replace = TRUE)
                               
                               synthetic_returns <- numeric(0)
                               for(b_idx in block_indices) {
                                 synthetic_returns <- c(synthetic_returns, 
                                                        returns_original[b_idx:(b_idx + block_size - 1)])
                               }
                               
                               synthetic_returns <- synthetic_returns[1:length(returns_original)]
                               synthetic_prices <- exp(cumsum(synthetic_returns)) * data_train$close[1]
                               
                               perm_data <- data_train
                               perm_data$close <- synthetic_prices
                               perm_data$open <- lag(synthetic_prices, 1)
                               perm_data$open[1] <- data_train$close[1]
                               perm_data$high <- pmax(perm_data$open, perm_data$close, na.rm = TRUE) * 1.001
                               perm_data$low <- pmin(perm_data$open, perm_data$close, na.rm = TRUE) * 0.999
                               
                               perm_data$tenkan <- (runMax(perm_data$high, opt_tenkan) + 
                                                      runMin(perm_data$low, opt_tenkan)) / 2
                               perm_data$kijun <- (runMax(perm_data$high, opt_kijun) + 
                                                     runMin(perm_data$low, opt_kijun)) / 2
                               
                               senkou_a_p <- (perm_data$tenkan + perm_data$kijun) / 2
                               senkou_b_p <- (runMax(perm_data$high, opt_senkou_b) + 
                                                runMin(perm_data$low, opt_senkou_b)) / 2
                               
                               perm_data$senkou_a_displaced <- lag(senkou_a_p, opt_displacement - 1)
                               perm_data$senkou_b_displaced <- lag(senkou_b_p, opt_displacement - 1)
                               
                               perm_data$kumo_high <- pmax(perm_data$senkou_a_displaced, 
                                                           perm_data$senkou_b_displaced, na.rm = TRUE)
                               perm_data$kumo_low <- pmin(perm_data$senkou_a_displaced, 
                                                          perm_data$senkou_b_displaced, na.rm = TRUE)
                               
                               perm_data$price_above_kumo <- perm_data$close > perm_data$kumo_high
                               perm_data$price_below_kumo <- perm_data$close < perm_data$kumo_low
                               
                               perm_data$chikou_free_long <- perm_data$close > lag(perm_data$high, opt_displacement - 1)
                               perm_data$chikou_free_short <- perm_data$close < lag(perm_data$low, opt_displacement - 1)
                               
                               perm_data$atr <- ATR(perm_data[, c("high", "low", "close")], n = 56)[, "atr"]
                               
                               perm_data$tk_cross_up <- perm_data$tenkan > perm_data$kijun & 
                                 lag(perm_data$tenkan) <= lag(perm_data$kijun)
                               perm_data$tk_cross_down <- perm_data$tenkan < perm_data$kijun & 
                                 lag(perm_data$tenkan) >= lag(perm_data$kijun)
                               
                               perm_data$signal_long <- perm_data$tk_cross_up & 
                                 perm_data$price_above_kumo & 
                                 perm_data$chikou_free_long
                               perm_data$signal_short <- perm_data$tk_cross_down & 
                                 perm_data$price_below_kumo & 
                                 perm_data$chikou_free_short
                               
                               perm_data$signal_long[is.na(perm_data$signal_long)] <- FALSE
                               perm_data$signal_short[is.na(perm_data$signal_short)] <- FALSE
                               
                               n_p <- nrow(perm_data)
                               position_p <- rep(0, n_p)
                               entry_price_p <- rep(NA_real_, n_p)
                               stop_loss_p <- rep(NA_real_, n_p)
                               take_profit_p <- rep(NA_real_, n_p)
                               equity_p <- rep(initial_capital, n_p)
                               
                               spread_cost_p <- opt_spread
                               
                               for(i in 2:n_p) {
                                 equity_p[i] <- equity_p[i-1]
                                 
                                 if(position_p[i-1] == 0) {
                                   if(perm_data$signal_long[i]) {
                                     position_p[i] <- 1
                                     entry_price_p[i] <- perm_data$close[i]
                                     sl_offset <- perm_data$atr[i] * opt_sl_atr + spread_cost_p
                                     tp_offset <- perm_data$atr[i] * opt_tp_atr + spread_cost_p
                                     stop_loss_p[i] <- entry_price_p[i] - sl_offset
                                     take_profit_p[i] <- entry_price_p[i] + tp_offset
                                   } else if(perm_data$signal_short[i]) {
                                     position_p[i] <- -1
                                     entry_price_p[i] <- perm_data$close[i]
                                     sl_offset <- perm_data$atr[i] * opt_sl_atr + spread_cost_p
                                     tp_offset <- perm_data$atr[i] * opt_tp_atr + spread_cost_p
                                     stop_loss_p[i] <- entry_price_p[i] + sl_offset
                                     take_profit_p[i] <- entry_price_p[i] - tp_offset
                                   }
                                 } else {
                                   position_p[i] <- position_p[i-1]
                                   entry_price_p[i] <- entry_price_p[i-1]
                                   stop_loss_p[i] <- stop_loss_p[i-1]
                                   take_profit_p[i] <- take_profit_p[i-1]
                                 }
                                 
                                 if(position_p[i] != 0) {
                                   exit_triggered <- FALSE
                                   exit_price <- perm_data$close[i]
                                   
                                   if(position_p[i] == 1) {
                                     if(perm_data$low[i] <= stop_loss_p[i]) {
                                       exit_triggered <- TRUE
                                       exit_price <- stop_loss_p[i]
                                     } else if(perm_data$high[i] >= take_profit_p[i]) {
                                       exit_triggered <- TRUE
                                       exit_price <- take_profit_p[i]
                                     }
                                   }
                                   
                                   if(position_p[i] == -1) {
                                     if(perm_data$high[i] >= stop_loss_p[i]) {
                                       exit_triggered <- TRUE
                                       exit_price <- stop_loss_p[i]
                                     } else if(perm_data$low[i] <= take_profit_p[i]) {
                                       exit_triggered <- TRUE
                                       exit_price <- take_profit_p[i]
                                     }
                                   }
                                   
                                   if(exit_triggered) {
                                     if(position_p[i] == 1) {
                                       pnl <- (exit_price - entry_price_p[i]) - spread_cost_p
                                     } else {
                                       pnl <- (entry_price_p[i] - exit_price) - spread_cost_p
                                     }
                                     
                                     trade_size <- equity_p[i-1] * position_size
                                     contracts <- trade_size / entry_price_p[i]
                                     pnl_dollar <- pnl * contracts
                                     equity_p[i] <- equity_p[i-1] + pnl_dollar
                                     
                                     position_p[i] <- 0
                                     entry_price_p[i] <- NA
                                     stop_loss_p[i] <- NA
                                     take_profit_p[i] <- NA
                                   }
                                 }
                               }
                               
                               returns_p <- c(0, diff(equity_p) / equity_p[-n_p])
                               sharpe_p <- mean(returns_p, na.rm = TRUE) / sd(returns_p, na.rm = TRUE) * sqrt(96 * 252)
                               
                               return(sharpe_p)
                             }

stopCluster(cl_perm)

p_value_train <- sum(permuted_sharpes_train >= original_sharpe_train, na.rm = TRUE) / 
  sum(!is.na(permuted_sharpes_train))

cat("\n=== PERMUTATIONSTEST TRAIN ERGEBNISSE ===\n")
cat("Original Sharpe Ratio:", round(original_sharpe_train, 3), "\n")
cat("Durchschn. Sharpe (Permutiert):", round(mean(permuted_sharpes_train, na.rm = TRUE), 3), "\n")
cat("P-Wert:", round(p_value_train, 4), "\n")

if(p_value_train < 0.05) {
  cat("✓ Strategie ist statistisch signifikant auf Train (p < 0.05)\n\n")
} else {
  cat("✗ WARNUNG: Mögliches Overfitting auf Train (p >= 0.05)\n\n")
}

# =============================================================================
# PERMUTATIONSTEST AUF TEST DATA
# =============================================================================
cat("========================================\n")
cat("PERMUTATIONSTEST (TEST)\n")
cat("========================================\n\n")

cat("Starte", n_permutations_test, "Permutationen auf Test Data...\n\n")

cl_perm_test <- makeCluster(n_cores)
registerDoParallel(cl_perm_test)

permuted_sharpes_test <- foreach(perm_idx = 1:n_permutations_test,
                                 .combine = 'c',
                                 .packages = c('tidyverse', 'TTR')) %dopar% {
                                   
                                   returns_original <- c(0, diff(log(data_test$close)))
                                   
                                   n_blocks <- ceiling(length(returns_original) / block_size)
                                   block_indices <- sample(1:(length(returns_original) - block_size), 
                                                           n_blocks, replace = TRUE)
                                   
                                   synthetic_returns <- numeric(0)
                                   for(b_idx in block_indices) {
                                     synthetic_returns <- c(synthetic_returns, 
                                                            returns_original[b_idx:(b_idx + block_size - 1)])
                                   }
                                   
                                   synthetic_returns <- synthetic_returns[1:length(returns_original)]
                                   synthetic_prices <- exp(cumsum(synthetic_returns)) * data_test$close[1]
                                   
                                   perm_data <- data_test
                                   perm_data$close <- synthetic_prices
                                   perm_data$open <- lag(synthetic_prices, 1)
                                   perm_data$open[1] <- data_test$close[1]
                                   perm_data$high <- pmax(perm_data$open, perm_data$close, na.rm = TRUE) * 1.001
                                   perm_data$low <- pmin(perm_data$open, perm_data$close, na.rm = TRUE) * 0.999
                                   
                                   perm_data$tenkan <- (runMax(perm_data$high, opt_tenkan) + 
                                                          runMin(perm_data$low, opt_tenkan)) / 2
                                   perm_data$kijun <- (runMax(perm_data$high, opt_kijun) + 
                                                         runMin(perm_data$low, opt_kijun)) / 2
                                   
                                   senkou_a_p <- (perm_data$tenkan + perm_data$kijun) / 2
                                   senkou_b_p <- (runMax(perm_data$high, opt_senkou_b) + 
                                                    runMin(perm_data$low, opt_senkou_b)) / 2
                                   
                                   perm_data$senkou_a_displaced <- lag(senkou_a_p, opt_displacement - 1)
                                   perm_data$senkou_b_displaced <- lag(senkou_b_p, opt_displacement - 1)
                                   
                                   perm_data$kumo_high <- pmax(perm_data$senkou_a_displaced, 
                                                               perm_data$senkou_b_displaced, na.rm = TRUE)
                                   perm_data$kumo_low <- pmin(perm_data$senkou_a_displaced, 
                                                              perm_data$senkou_b_displaced, na.rm = TRUE)
                                   
                                   perm_data$price_above_kumo <- perm_data$close > perm_data$kumo_high
                                   perm_data$price_below_kumo <- perm_data$close < perm_data$kumo_low
                                   
                                   perm_data$chikou_free_long <- perm_data$close > lag(perm_data$high, opt_displacement - 1)
                                   perm_data$chikou_free_short <- perm_data$close < lag(perm_data$low, opt_displacement - 1)
                                   
                                   perm_data$atr <- ATR(perm_data[, c("high", "low", "close")], n = 56)[, "atr"]
                                   
                                   perm_data$tk_cross_up <- perm_data$tenkan > perm_data$kijun & 
                                     lag(perm_data$tenkan) <= lag(perm_data$kijun)
                                   perm_data$tk_cross_down <- perm_data$tenkan < perm_data$kijun & 
                                     lag(perm_data$tenkan) >= lag(perm_data$kijun)
                                   
                                   perm_data$signal_long <- perm_data$tk_cross_up & 
                                     perm_data$price_above_kumo & 
                                     perm_data$chikou_free_long
                                   perm_data$signal_short <- perm_data$tk_cross_down & 
                                     perm_data$price_below_kumo & 
                                     perm_data$chikou_free_short
                                   
                                   perm_data$signal_long[is.na(perm_data$signal_long)] <- FALSE
                                   perm_data$signal_short[is.na(perm_data$signal_short)] <- FALSE
                                   
                                   n_p <- nrow(perm_data)
                                   position_p <- rep(0, n_p)
                                   entry_price_p <- rep(NA_real_, n_p)
                                   stop_loss_p <- rep(NA_real_, n_p)
                                   take_profit_p <- rep(NA_real_, n_p)
                                   equity_p <- rep(initial_capital, n_p)
                                   
                                   spread_cost_p <- opt_spread
                                   
                                   for(i in 2:n_p) {
                                     equity_p[i] <- equity_p[i-1]
                                     
                                     if(position_p[i-1] == 0) {
                                       if(perm_data$signal_long[i]) {
                                         position_p[i] <- 1
                                         entry_price_p[i] <- perm_data$close[i]
                                         sl_offset <- perm_data$atr[i] * opt_sl_atr + spread_cost_p
                                         tp_offset <- perm_data$atr[i] * opt_tp_atr + spread_cost_p
                                         stop_loss_p[i] <- entry_price_p[i] - sl_offset
                                         take_profit_p[i] <- entry_price_p[i] + tp_offset
                                       } else if(perm_data$signal_short[i]) {
                                         position_p[i] <- -1
                                         entry_price_p[i] <- perm_data$close[i]
                                         sl_offset <- perm_data$atr[i] * opt_sl_atr + spread_cost_p
                                         tp_offset <- perm_data$atr[i] * opt_tp_atr + spread_cost_p
                                         stop_loss_p[i] <- entry_price_p[i] + sl_offset
                                         take_profit_p[i] <- entry_price_p[i] - tp_offset
                                       }
                                     } else {
                                       position_p[i] <- position_p[i-1]
                                       entry_price_p[i] <- entry_price_p[i-1]
                                       stop_loss_p[i] <- stop_loss_p[i-1]
                                       take_profit_p[i] <- take_profit_p[i-1]
                                     }
                                     
                                     if(position_p[i] != 0) {
                                       exit_triggered <- FALSE
                                       exit_price <- perm_data$close[i]
                                       
                                       if(position_p[i] == 1) {
                                         if(perm_data$low[i] <= stop_loss_p[i]) {
                                           exit_triggered <- TRUE
                                           exit_price <- stop_loss_p[i]
                                         } else if(perm_data$high[i] >= take_profit_p[i]) {
                                           exit_triggered <- TRUE
                                           exit_price <- take_profit_p[i]
                                         }
                                       }
                                       
                                       if(position_p[i] == -1) {
                                         if(perm_data$high[i] >= stop_loss_p[i]) {
                                           exit_triggered <- TRUE
                                           exit_price <- stop_loss_p[i]
                                         } else if(perm_data$low[i] <= take_profit_p[i]) {
                                           exit_triggered <- TRUE
                                           exit_price <- take_profit_p[i]
                                         }
                                       }
                                       
                                       if(exit_triggered) {
                                         if(position_p[i] == 1) {
                                           pnl <- (exit_price - entry_price_p[i]) - spread_cost_p
                                         } else {
                                           pnl <- (entry_price_p[i] - exit_price) - spread_cost_p
                                         }
                                         
                                         trade_size <- equity_p[i-1] * position_size
                                         contracts <- trade_size / entry_price_p[i]
                                         pnl_dollar <- pnl * contracts
                                         equity_p[i] <- equity_p[i-1] + pnl_dollar
                                         
                                         position_p[i] <- 0
                                         entry_price_p[i] <- NA
                                         stop_loss_p[i] <- NA
                                         take_profit_p[i] <- NA
                                       }
                                     }
                                   }
                                   
                                   returns_p <- c(0, diff(equity_p) / equity_p[-n_p])
                                   sharpe_p <- mean(returns_p, na.rm = TRUE) / sd(returns_p, na.rm = TRUE) * sqrt(96 * 252)
                                   
                                   return(sharpe_p)
                                 }

stopCluster(cl_perm_test)

p_value_test <- sum(permuted_sharpes_test >= sharpe_ratio_test, na.rm = TRUE) / 
  sum(!is.na(permuted_sharpes_test))

cat("\n=== PERMUTATIONSTEST TEST ERGEBNISSE ===\n")
cat("Original Sharpe Ratio:", round(sharpe_ratio_test, 3), "\n")
cat("Durchschn. Sharpe (Permutiert):", round(mean(permuted_sharpes_test, na.rm = TRUE), 3), "\n")
cat("P-Wert:", round(p_value_test, 4), "\n")

if(p_value_test < 0.05) {
  cat("✓ Strategie ist statistisch signifikant auf Test (p < 0.05)\n\n")
} else {
  cat("✗ WARNUNG: Mögliches Overfitting auf Test (p >= 0.05)\n\n")
}

perm_results <- data.frame(
  original_sharpe_train = original_sharpe_train,
  p_value_train = p_value_train,
  original_sharpe_test = sharpe_ratio_test,
  p_value_test = p_value_test
)


# =============================================================================
# VISUALISIERUNGEN
# =============================================================================
cat("========================================\n")
cat("ERSTELLE VISUALISIERUNGEN\n")
cat("========================================\n\n")

# Equity Curve
equity_df <- data.frame(
  datetime = test_data$datetime,
  equity = equity_test
)

p1 <- ggplot(equity_df, aes(x = datetime, y = equity)) +
  geom_line(color = "steelblue", size = 0.5) +
  geom_hline(yintercept = initial_capital, linetype = "dashed", color = "red") +
  labs(title = paste("Equity Curve (Out-of-Sample) -", EPIC, INTERVAL),
       subtitle = paste("Sharpe Ratio:", round(sharpe_ratio_test, 3)),
       x = "Datum/Zeit", y = "Equity ($)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

print(p1)

# Permutation Distribution
perm_df <- data.frame(sharpe = permuted_sharpes_test)

p2 <- ggplot(perm_df, aes(x = sharpe)) +
  geom_histogram(bins = 30, fill = "lightgray", color = "black", alpha = 0.7) +
  geom_vline(xintercept = sharpe_ratio_test, color = "red", size = 1.5) +
  annotate("text", x = sharpe_ratio_test, y = Inf, 
           label = paste("Original:", round(sharpe_ratio_test, 3)),
           vjust = 2, hjust = -0.1, color = "red", fontface = "bold") +
  labs(title = "Permutationstest: Sharpe Ratio Distribution (Test Data)",
       subtitle = paste("P-Wert:", round(p_value_test, 4)),
       x = "Sharpe Ratio", y = "Häufigkeit") +
  theme_minimal()

print(p2)

# Trade Analysis
if(length(trades_test) > 0) {
  trades_summary <- trades_test_df %>%
    group_by(direction, exit_reason) %>%
    summarise(count = n(), avg_pnl = mean(pnl), .groups = "drop")
  
  p3 <- ggplot(trades_summary, aes(x = exit_reason, y = count, fill = direction)) +
    geom_col(position = "dodge") +
    labs(title = paste("Trade Analysis -", EPIC, INTERVAL),
         x = "Exit Grund", y = "Anzahl Trades") +
    scale_fill_manual(values = c("Long" = "#00BA38", "Short" = "#F8766D")) +
    theme_minimal()
  
  print(p3)
}

# =============================================================================
# FINALE ZUSAMMENFASSUNG
# =============================================================================
cat("\n")
cat("================================================================================\n")
cat("                          BACKTEST ABGESCHLOSSEN!\n")
cat("================================================================================\n\n")

cat("📊 Zusammenfassung für", EPIC, "-", INTERVAL, ":\n\n")
cat("   • Total Return (Test):", round(total_return_test, 2), "%\n")
cat("   • Sharpe Ratio (Test):", round(sharpe_ratio_test, 3), "\n")
cat("   • Win Rate:", round(win_rate_test, 1), "%\n")
cat("   • Anzahl Trades:", n_trades_test, "\n")
cat("   • Profit Factor:", round(profit_factor_test, 2), "\n")
cat("   • Max Drawdown:", round(max_drawdown_test, 2), "%\n")
cat("   • P-Wert Train:", round(p_value_train, 4), "\n")
cat("   • P-Wert Test:", round(p_value_test, 4), "\n\n")

if(p_value_test < 0.05) {
  cat("✓ STRATEGIE STATISTISCH SIGNIFIKANT\n")
} else {
  cat("⚠ WARNUNG: Mögliches Overfitting - weitere Validierung empfohlen\n")
}

cat("\n")
cat("Alle Ergebnisse wurden gespeichert in:\n")
cat(" ", output_path, "\n\n")
cat("Dateien:\n")
cat("  - best_parameters.csv\n")
cat("  - optimization_results.csv\n")
cat("  - performance_metrics.csv\n")
cat("  - trades.csv\n")
cat("  - permutation_results.csv\n")
cat("  - equity_curve.png\n")
cat("  - permutation_test.png\n")
cat("  - trade_analysis.png\n\n")

cat("================================================================================\n")
cat("Script erfolgreich beendet!\n")
cat("================================================================================\n\n")