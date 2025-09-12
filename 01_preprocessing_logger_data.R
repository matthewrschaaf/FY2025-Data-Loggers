# --- Checking and Loading Necessary Packages ---
message("Checking for required packages...")
required_packages <- c("readxl", "dplyr")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Package not found:", pkg, "- Installing..."))
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}
message("All required packages are loaded.")


# --- Define File Path, Sheet Groups, and Column Types ----
file_path <- "O:/ED/Private/Water Quality/Data/FY2025/2025 Data Loggers/FY2025 Data Logger Analysis/Excel/Loggers_UptoDate_09082025.xlsx"

sheets_salt <- 1:12
sheets_licking <- 13:28

column_types <- c("text", "date", "numeric", "numeric")


# --- Read the Sheets into Two Lists ----
# Read sheets 1-12 into a list called Salt_import
Salt_import <- lapply(sheets_salt, function(sheet_number) {
  read_excel(path = file_path, sheet = sheet_number, col_types = column_types)
})

# Read sheets 13-28 into a list called Licking_import
Licking_import <- lapply(sheets_licking, function(sheet_number) {
  read_excel(path = file_path, sheet = sheet_number, col_types = column_types)
})


# --- Create Empty Dataframes for Future Date Join ----
# Define start and end times of empty dataframe
saltstart_datetime <- as.POSIXct("2024-03-01 00:00:00", tz = "UTC")
saltend_datetime   <- as.POSIXct("2025-07-31 23:45:00", tz = "UTC")
lickingstart_datetime <- as.POSIXct("2024-03-01 00:00:00", tz = "UTC")
lickingend_datetime <- as.POSIXct("2025-02-02 23:45:00", tz = "UTC")

# Generate the 15-Minute DateTime Sequence
saltdatetime_sequence <- seq(from = saltstart_datetime, to = saltend_datetime, by = "15 mins")
lickingdatetime_sequence <- seq(from = lickingstart_datetime, to = lickingend_datetime, by = "15 mins")

# Create the Empty DataFrame
salt_empty_df <- data.frame(
  DateTime = saltdatetime_sequence,
  Logger = NA_character_,
  `Temperature [C]` = NA_real_,
  `Height [ft]` = NA_real_,
  check.names = FALSE
)

licking_empty_df <- data.frame(
  DateTime = lickingdatetime_sequence,
  Logger = NA_character_,
  `Temperature [C]` = NA_real_,
  `Height [ft]` = NA_real_,
  check.names = FALSE
)


# --- Combine the Lists into a Single Data Frame ----
all_salt_import_data <- bind_rows(Salt_import)
all_licking_import_data <- bind_rows(Licking_import)


salt_df <- right_join(all_salt_import_data, select(salt_empty_df, DateTime), by = "DateTime") %>%
  filter(!is.na(Logger))
licking_df <- right_join(all_licking_import_data, select(licking_empty_df, DateTime), by = "DateTime") %>%
  filter(!is.na(Logger))


# --- Organize all related objects into a single list ----
Licking <- list(
  import          = Licking_import,
  combined_import = all_licking_import_data,
  empty_df        = licking_empty_df,
  final_df        = licking_df
)

Salt <- list(
  import          = Salt_import,
  combined_import = all_salt_import_data,
  empty_df        = salt_empty_df,
  final_df        = salt_df
)

objects_to_remove <- c(
  "Licking_import", "licking_empty_df", "licking_df", "all_licking_import_data",
  "Salt_import", "salt_empty_df", "salt_df", "all_salt_import_data"
)

rm(list = objects_to_remove)
gc()

