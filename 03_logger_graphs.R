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


# --- Prepare the Data (Graphs) ----
s1_data <- Salt$stats_df %>%
  filter(Logger == "S1" & !is.na(Temp_median))


# --- Define the Fixed Axis Limits (Graphs) ----
x_axis_start <- as.POSIXct("2024-03-01 00:00:00")
x_axis_end   <- as.POSIXct("2025-07-31 00:00:00")


# --- Build the Plot (Graphs) ----
s1_temp_graph <- ggplot(s1_data, aes(x = DateTime)) +
  
  geom_ribbon(aes(ymin = Temp_min, ymax = Temp_max), fill = "orange", alpha = 0.3) +
  
  geom_ribbon(aes(ymin = Temp_25th, ymax = Temp_75th), fill = "green", alpha = 0.4) +
  
  geom_line(aes(y = Temp_median), color = "black", linewidth = 0.25) +
  
  geom_line(data = Salt$guide_curve, aes(y = Guide), color = "blue", linewidth = 0.75) +
  
  geom_line(data = Salt$guide_curve, aes(y = Max), color = "blue", linetype = "dashed") +
  
  geom_line(data = Salt$guide_curve, aes(y = Min), color = "blue", linetype = "dashed") +
  
  
  # --- Format the Axes and Labels ---
  scale_x_datetime(
    limits = c(x_axis_start, x_axis_end),
    date_breaks = "2 months",
    labels = label_date_short(format = c("%Y", "%b")) # Formats as "MAR", "MAY", etc.
  ) +
  
  labs(
    title = "24-Hour Rolling Temperature Statistics for Logger S1",
    subtitle = "Data from Salt River Loggers",
    y = "Temperature [C]",
    x = "Date"
  ) +
  
  theme_minimal()


# --- Export the Graph to a PNG File (Graphs) ----
ggsave(
  filename = "S1_Temperature_Graph.png",
  plot = s1_temp_graph,
  width = 14,
  height = 7,
  dpi = 1200
)


# --- Prepare the Data (Outflow Graphs) ----
outflow_data <- Salt$outflow %>%
  filter(!is.na(CFS_INST_VAL))


# --- Define the Fixed Axis Limits (Outflow Graphs) ----
x_axis_start <- as.POSIXct("2024-03-01 00:00:00")
x_axis_end   <- as.POSIXct("2025-07-31 00:00:00")


# --- Build the Plot (Outflow Graphs) ----
outflow_graph <- ggplot(outflow_data, aes(x = DateTime, y = CFS_INST_VAL)) +
  
  geom_line(color = "steelblue", linewidth = 0.8) +
  
  # --- Format the Axes and Labels ---
  scale_x_datetime(
    limits = c(x_axis_start, x_axis_end),
    date_breaks = "2 months",
    labels = label_date_short(format = c("%Y", "%b"))
  ) +
  
  labs(
    title = "Instantaneous Outflow at TAR",
    subtitle = "Data from Salt River Loggers",
    y = "Outflow (CFS)",
    x = "Date"
  ) +
  

  theme_minimal()


# --- Export the Graph to a PNG File (Outflow Graphs) ----
ggsave(
  filename = "Salt_Outflow_Graph.png",
  plot = outflow_graph,
  width = 14,
  height = 7,
  dpi = 1200
)
