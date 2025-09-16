# --- Checking and Loading Necessary Packages ---
message("Checking for required packages...")
required_packages <- c("dplyr", "readxl", "lubridate", "ggplot2", "scales")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Package not found:", pkg, "- Installing..."))
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}
message("All required packages are loaded.")


# --- Define a Function for Defining Data Segments ----
add_data_segments <- function(df) {
  df %>%
    arrange(Logger, DateTime) %>%
    group_by(Logger) %>%
    mutate(
      time_gap = as.numeric(difftime(DateTime, lag(DateTime), units = "mins")),
      data_segment = cumsum(is.na(lag(DateTime)) | time_gap > 15)
    ) %>%
    ungroup() %>%
    select(-time_gap)
}

Salt$stats_df <- add_data_segments(Salt$stats_df)
Licking$stats_df <- add_data_segments(Licking$stats_df)


# --- Define File Path and Column Types (Guide Curve)----
guide_curve_path <- "O:/ED/Private/Water Quality/Data/FY2025/2025 Data Loggers/FY2025-Data-Loggers/Excel/CRR_TAR_GuideCurves.xlsx"


# --- Import the Excel Sheets (Guide Curve) ----
TAR_Guide_Curve <- read_excel(
  path = guide_curve_path,
  sheet = 1,
  skip = 1,
) 

CRR_Guide_Curve <- read_excel(
  path = guide_curve_path,
  sheet = 2,
  skip = 1,
) 


# --- Define File Path (Outflow Data) ----
outflow_path <- "O:/ED/Private/Water Quality/Data/FY2025/2025 Data Loggers/FY2025-Data-Loggers/Excel/OutflowData_Loggers.xlsx"


# --- Import the Data (Outflow Data) ----
Outflow_Excel <- read_excel(
  path = outflow_path,
  skip = 7,
  col_names = FALSE
)


# --- Create the Data Frames (Outflow Data) ----
TAR_Outflow <- Outflow_Excel %>%

  select(2, 3) %>%
  transmute(
    DateTime = as.POSIXct(...2),
    CFS_INST_VAL = ...3
  )

CRR_Outflow <- Outflow_Excel %>%
  select(2, 4) %>%
  transmute(
    DateTime = as.POSIXct(...2),
    CFS_INST_VAL = ...4
  )


Salt$outflow <- TAR_Outflow
Salt$guide_curve <- TAR_Guide_Curve

Licking$outflow <- CRR_Outflow
Licking$guide_curve <- CRR_Guide_Curve

objects_to_remove <- c("Outflow_Excel", "CRR_Guide_Curve", "CRR_Outflow", "TAR_Guide_Curve", "TAR_Outflow")

rm(list = objects_to_remove)
gc()

