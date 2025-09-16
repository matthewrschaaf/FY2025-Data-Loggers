message("Step 1/4: Running 01_preprocessing_logger_data.R...")
source("01_preprocessing_logger_data.R")

message("Step 2/4: Running 02_logger_statistics.R...")
source("02_logger_statistics.R")

message("Step 3/4: Running 03_logger_graphs.R...")
source("03_logger_graphs.R")

message("Step 4/4: Launching 04_logger_shiny.R...")
source("04_logger_shiny.R")