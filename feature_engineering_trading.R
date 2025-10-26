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
#Daten laden und vorbereiten
df_raw <- read_csv(file.path(getwd(), "capitalcom_backtesting", "api-data", "GOLD_MINUTE_15_testdata.csv")) %>%
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

