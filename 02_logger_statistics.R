# --- Checking and Loading Necessary Packages ---
message("Checking for required packages...")
required_packages <- c("dplyr", "lubridate", "data.table")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Package not found:", pkg, "- Installing..."))
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}
message("All required packages are loaded.")


# --- Convert Salt Data to a data.table for Speed ----
salt_dt <- as.data.table(Salt$final_df)
salt_dt <- salt_dt[!is.na(DateTime)]

# --- Prepare for the Loop ---
unique_loggers <- unique(salt_dt$Logger)
results_list <- vector("list", length = length(unique_loggers))

# --- Loop Through Each Salt Logger to Calculate Stats and Show Progress ----
execution_time <- system.time({
 
  for (i in seq_along(unique_loggers)) {
    
    current_logger <- unique_loggers[i]
    
    cat(sprintf("--> Processing logger %s (%d of %d)...\n", current_logger, i, length(unique_loggers)))
    
    one_logger_dt <- salt_dt[Logger == current_logger]
    
    one_logger_dt[, window_start := DateTime - hours(12)]
    one_logger_dt[, window_end   := DateTime + hours(12)]
    
    window_groups <- one_logger_dt[one_logger_dt, 
                                   on = .(Logger, DateTime >= window_start, DateTime <= window_end), 
                                   nomatch = 0L]
    
    logger_stats <- window_groups[, .(
      Temp_median = median(`Temperature [C]`, na.rm = TRUE),
      Temp_min    = min(`Temperature [C]`, na.rm = TRUE),
      Temp_max    = max(`Temperature [C]`, na.rm = TRUE),
      Temp_25th   = quantile(`Temperature [C]`, probs = 0.25, na.rm = TRUE),
      Temp_75th   = quantile(`Temperature [C]`, probs = 0.75, na.rm = TRUE),
      
      Height_median = median(`Height [ft]`, na.rm = TRUE),
      Height_min    = min(`Height [ft]`, na.rm = TRUE),
      Height_max    = max(`Height [ft]`, na.rm = TRUE),
      Height_25th   = quantile(`Height [ft]`, probs = 0.25, na.rm = TRUE),
      Height_75th   = quantile(`Height [ft]`, probs = 0.75, na.rm = TRUE),
      
      n_points_in_window = .N
    ), by = .(Logger, DateTime = i.DateTime)]
    
    results_list[[i]] <- logger_stats
  }
  
  
  # --- Combine All Salt Results ----
  Salt$stats_df <- bind_rows(results_list)
  
})


cat("--- Total Time Taken ---\n")
print(execution_time)
cat("\n")


objects_to_remove <- c(
  "logger_stats", "one_logger_dt", "results_list", "salt_dt", "window_groups",
  "current_logger", "i", "unique_loggers", "execution_time"
)

rm(list = objects_to_remove)
gc()


# --- Convert Licking Data to a data.table for Speed ----
licking_dt <- as.data.table(Licking$final_df)
licking_dt <- licking_dt[!is.na(DateTime)]

# --- Prepare for the Loop ---
unique_loggers <- unique(licking_dt$Logger)
results_list <- vector("list", length = length(unique_loggers))


# --- Loop Through Each Licking Logger to Calculate Stats and Show Progress ----
execution_time <- system.time({
  

  for (i in seq_along(unique_loggers)) {
    
    current_logger <- unique_loggers[i]
    
    cat(sprintf("--> Processing logger %s (%d of %d)...\n", current_logger, i, length(unique_loggers)))
    
    one_logger_dt <- licking_dt[Logger == current_logger]
    
    one_logger_dt[, `:=`(window_start = DateTime - hours(12),
                         window_end   = DateTime + hours(12))]
    
    window_groups <- one_logger_dt[one_logger_dt, 
                                   on = .(Logger, DateTime >= window_start, DateTime <= window_end), 
                                   nomatch = 0L]
    
    logger_stats <- window_groups[, .(
      Temp_median = median(`Temperature [C]`, na.rm = TRUE),
      Temp_min    = min(`Temperature [C]`, na.rm = TRUE),
      Temp_max    = max(`Temperature [C]`, na.rm = TRUE),
      Temp_25th   = quantile(`Temperature [C]`, probs = 0.25, na.rm = TRUE),
      Temp_75th   = quantile(`Temperature [C]`, probs = 0.75, na.rm = TRUE),
      
      Height_median = median(`Height [ft]`, na.rm = TRUE),
      Height_min    = min(`Height [ft]`, na.rm = TRUE),
      Height_max    = max(`Height [ft]`, na.rm = TRUE),
      Height_25th   = quantile(`Height [ft]`, probs = 0.25, na.rm = TRUE),
      Height_75th   = quantile(`Height [ft]`, probs = 0.75, na.rm = TRUE),
      
      n_points_in_window = .N
    ), by = .(Logger, DateTime = i.DateTime)]
    
    results_list[[i]] <- logger_stats
  }
  
  # --- Combine All Licking Results ---
  Licking$stats_df <- bind_rows(results_list)
  
})


cat("\n--- Total Time Taken ---\n")
print(execution_time)
cat("\n")


objects_to_remove <- c(
  "logger_stats", "one_logger_dt", "results_list", "licking_dt", "window_groups",
  "current_logger", "i", "unique_loggers", "execution_time"
)

rm(list = objects_to_remove)
gc()

