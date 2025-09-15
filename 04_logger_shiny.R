# --- Checking and Loading Necessary Packages ---
message("Checking for required packages...")
required_packages <- c("dplyr", "shiny", "lubridate", "ggplot2", "scales", "plotly")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Package not found:", pkg, "- Installing..."))
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}
message("All required packages are loaded.")


# --- App Setup ----

unique_loggers <- unique(Salt$stats_df$Logger)

logger_colors <- setNames(hue_pal()(length(unique_loggers)), unique_loggers)

x_axis_start <- as.POSIXct("2024-03-01 00:00:00")
x_axis_end   <- as.POSIXct("2025-07-31 00:00:00")


# --- Define UI ----
ui <- fluidPage(
  titlePanel("FY2025 Data Logger Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Logger Selection"),
      p("Select one or more loggers to display their median temperature."),
      checkboxGroupInput(
        inputId = "selected_loggers",
        label = "Available Loggers:",
        choices = unique_loggers,
        selected = unique_loggers[1]
      ),
      
      hr(),
      
      h3("Display Options"),
      checkboxInput(
        inputId = "show_guides",
        label = "Show Guide Curves",
        value = FALSE
      ),
      checkboxInput(
        inputId = "show_ranges",
        label = "Show Statistical Ranges (for selected loggers)",
        value = FALSE
      )
    ),
    
    mainPanel(

      plotlyOutput("logger_plot", height = "600px")
    )
  )
)


# --- Define Server Logic ----
server <- function(input, output) {
  
  output$logger_plot <- renderPlotly({
    
    req(input$selected_loggers)
    
    filtered_data <- Salt$stats_df %>%
      filter(Logger %in% input$selected_loggers)
    
    p <- ggplot()
    
    if (input$show_ranges) {
      p <- p + 
        geom_ribbon(data = filtered_data, 
                    aes(x = DateTime, ymin = Temp_min, ymax = Temp_max, group = Logger), 
                    fill = "darkorange", alpha = 0.1) +
        geom_ribbon(data = filtered_data, 
                    aes(x = DateTime, ymin = Temp_25th, ymax = Temp_75th, group = Logger), 
                    fill = "darkgreen", alpha = 0.2)
    }
    
    p <- p + 
      geom_line(data = filtered_data, 
                aes(x = DateTime, y = Temp_median, color = Logger), 
                linewidth = 0.4)
    
    if (input$show_guides) {
      p <- p + 
        geom_line(data = Salt$guide_curve, aes(x = DateTime, y = Guide), color = "blue", linewidth = 0.2) +
        geom_line(data = Salt$guide_curve, aes(x = DateTime, y = Max), color = "blue", linetype = "dashed") +
        geom_line(data = Salt$guide_curve, aes(x = DateTime, y = Min), color = "blue", linetype = "dashed")
    }
    
    p <- p +
      scale_x_datetime(
        limits = c(x_axis_start, x_axis_end),
        date_breaks = "2 months",
        labels = label_date_short(format = c("%Y", "%b"))
      ) +
      scale_color_manual(values = logger_colors) +
      labs(
        title = "24-Hour Rolling Temperature Statistics",
        y = "Temperature [C]",
        x = "Date",
        color = "Logger ID"
      ) +
      theme_minimal(base_size = 14)
    
    ggplotly(p)
    
  })
}


# --- Run the App ----
shinyApp(ui = ui, server = server)