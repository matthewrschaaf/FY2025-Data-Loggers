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
salt_loggers <- unique(Salt$stats_df$Logger)
licking_loggers <- unique(Licking$stats_df$Logger)
all_unique_loggers <- unique(c(salt_loggers, licking_loggers))


# --- Define UI ----
ui <- fluidPage(
  titlePanel("FY2025 Data Logger Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Data Source"),
      selectInput("data_source", "Select River Basin:",
                  choices = c("Salt", "Licking"), selected = "Salt"),
      hr(),
      h3("Logger Selection"),
      p("Select one or more loggers to display."),
      uiOutput("logger_checkboxes_ui"),
      hr(),
      h3("Display Options"),
      checkboxInput("show_guides", "Show Guide Curves", value = FALSE),
      checkboxInput("show_ranges", "Show Statistical Ranges", value = FALSE),
      checkboxInput("show_outflow_plot", "Show Outflow Plot", value = FALSE),
    ),
    
    mainPanel(
      plotlyOutput("logger_plot", height = "500px"),
      
      conditionalPanel(
        condition = "input.show_outflow_plot == true",
        hr(),
        plotlyOutput("outflow_plot", height = "300px")
      )
    )
  )
)


# --- Define Server Logic ----
server <- function(input, output) {
  
  
  reactive_stats_df <- reactive({
    if (input$data_source == "Salt") return(Salt$stats_df)
    else return(Licking$stats_df)
  })
  
  reactive_guide_curve <- reactive({
    if (input$data_source == "Salt") return(Salt$guide_curve)
    else return(Licking$guide_curve)
  })
  
  
  reactive_xaxis_start <- reactive({
    
    as.POSIXct("2024-03-01 00:00:00")
  })
  
  reactive_xaxis_end <- reactive({
    if (input$data_source == "Salt") {
      as.POSIXct("2025-07-31 00:00:00")
    } else {
      as.POSIXct("2025-02-02 23:45:00")
    }
  })
  
  reactive_logger_colors <- reactive({
    current_loggers <- unique(reactive_stats_df()$Logger)
    setNames(hue_pal()(length(current_loggers)), current_loggers)
  })
  
  
  output$logger_checkboxes_ui <- renderUI({
    available_loggers <- unique(reactive_stats_df()$Logger)
    checkboxGroupInput(
      inputId = "selected_loggers",
      label = "Available Loggers:",
      choices = available_loggers,
      selected = available_loggers[1]
    )
  })
  
  # Main plot output
  output$logger_plot <- renderPlotly({
    
    stats_data <- reactive_stats_df()
    guide_data <- reactive_guide_curve()
    
    req(input$selected_loggers)
    
    filtered_data <- stats_data %>%
      filter(Logger %in% input$selected_loggers)
    
    p <- ggplot()
    
    if (input$show_ranges) {
      p <- p + 
        geom_ribbon(data = filtered_data, 
                    aes(x = DateTime, ymin = Temp_min, ymax = Temp_max, group = data_segment, text = paste("Logger:", Logger,
                                                                                                     "<br>DateTime:", DateTime,
                                                                                                     "<br>Max Temp:", round(Temp_max, 2),
                                                                                                     "<br>Min Temp:", round(Temp_min, 2))), 
                    fill = "darkorange", alpha = 0.1) +
        geom_ribbon(data = filtered_data, 
                    aes(x = DateTime, ymin = Temp_25th, ymax = Temp_75th, group = data_segment, text = paste("Logger:", Logger,
                                                                                                       "<br>DateTime:", DateTime,
                                                                                                       "<br>75th percentile:", round(Temp_75th, 2),
                                                                                                       "<br>25th percentile:", round(Temp_25th, 2))), 
                    fill = "darkgreen", alpha = 0.2)
    }
    
    p <- p + 
          geom_line(data = filtered_data, 
                    aes(x = DateTime, y = Temp_median, color = Logger, group = data_segment, text = paste("Logger:", Logger,
                                                                                                    "<br>DateTime:", DateTime,
                                                                                                    "<br>Temp_median:", round(Temp_median, 2))), 
                    linewidth = 0.4)
    
    if (input$show_guides) {
      p <- p + 
        geom_line(data = guide_data, aes(x = DateTime, y = Guide), color = "blue", linewidth = 0.2) +
        geom_line(data = guide_data, aes(x = DateTime, y = Max), color = "blue", linetype = "dashed") +
        geom_line(data = guide_data, aes(x = DateTime, y = Min), color = "blue", linetype = "dashed")
    }
    
    p <- p +
      
      scale_x_datetime(
        limits = c(reactive_xaxis_start(), reactive_xaxis_end()),
        date_breaks = "2 months",
        labels = label_date_short(format = c("%Y", "%b"))
      ) +
      scale_color_manual(values = reactive_logger_colors()) +
      labs(
        title = paste("24-Hour Rolling Temperature Statistics for", input$data_source),
        y = "Temperature [C]",
        x = "Date",
        color = "Logger ID"
      ) +
      theme_minimal(base_size = 14)
    
    ggplotly(p, tooltip = "text")
  })
  
  
# --- Outflow Plot Logic ---
  reactive_outflow_df <- reactive({
    if (input$data_source == "Salt") Salt$outflow
    else Licking$outflow
  })
  
  output$outflow_plot <- renderPlotly({
    outflow_data <- reactive_outflow_df()
    p_outflow <- ggplot(outflow_data, aes(x = DateTime, y = CFS_INST_VAL)) +
      geom_line(color = "steelblue") +
      scale_x_datetime(
        limits = c(reactive_xaxis_start(), reactive_xaxis_end()),
        date_breaks = "2 months",
        labels = label_date_short(format = c("%Y", "%b"))
      ) +
      labs(title = "Instantaneous Outflow", y = "Outflow (CFS)", x = NULL) +
      theme_minimal(base_size = 14)
    ggplotly(p_outflow)
  })

}


# --- Run the App ----
shinyApp(ui = ui, server = server)