library(shiny)
library(leaflet)
library(sf)
library(readxl)
library(dplyr)
library(readr)
library(scales)
library(stringr)
library(purrr)

############# DATA ############
parks <- st_read("app_data/park_boundaries_2025-08-14.gpkg")

# read in water supply database
water_supplies <- read_csv("app_data/water_supplies.csv") %>% 
  select(wsd_source_id, park_unit, park_name, region, state, 
         water_system_name, source_longitude, source_latitude)

# read in indicators
fire_exp <- read_csv("app_data/fire_exposure_2025-09-09.csv")
fire_sen <- read_csv("app_data/fire_sensitivity_2025-09-22.csv")
runoff <- read_csv("app_data/runoff_exposure_2025-09-18.csv")

# combine all indicators
combined_data <- reduce(list(water_supplies, fire_exp, fire_sen, runoff), left_join, by = c("wsd_source_id", "park_name", "park_unit")) %>% 
  st_as_sf(coords = c("source_longitude", "source_latitude"), crs = 4326)

# Define indicator structure for UI
indicator_config <- list(
  exposure = list(
    "Fire Probability Change" = list(
      base_name = "fire_prob_change",
      measures = c("10th Percentile" = "p10", "Median" = "p50", "90th Percentile" = "p90"),
      description = "Projected change in likelihood of wildfire"
    ),
    "Runoff Change" = list(
      base_name = "runoff_change", 
      measures = c("10th Percentile" = "p10", "Median" = "p50", "90th Percentile" = "p90"),
      description = "Projected change in runoff"
    ),
    "Models Showing Runoff Decrease" = list(
      base_name = "models_showing_decrease",
      measures = c("Percentage" = "pct"),
      description = "Percentage of climate models projecting runoff decrease"
    )
  ),
  sensitivity = list(
    "Wildfire Hazard Potential" = list(
      base_name = "wildfire_hazard",
      measures = c("Mean" = "mean"),
      description = "Current wildfire hazard potential"
    )
  )
)

###################### UI ###############################
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f8f9fa;
        font-family: 'Arial', 'Helvetica', sans-serif;
      }
      
      .navbar-brand {
        font-weight: bold;
      }
      
      .main-header {
        background: linear-gradient(135deg, #2d5a27 0%, #4a7c59 100%);
        color: white;
        padding: 20px 15px;
        margin-bottom: 15px;
        border-radius: 8px;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
      }
      
      .main-header h1 {
        margin: 0;
        font-size: 2.2rem;
        font-weight: 300;
        text-align: center;
      }
      
      .main-header .subtitle {
        text-align: center;
        margin-top: 8px;
        font-size: 1.1rem;
        opacity: 0.9;
      }
      
      .control-panel {
        background: white;
        padding: 15px;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
        margin-bottom: 15px;
        border-top: 4px solid #2d5a27;
      }
      
      .form-group label {
        font-weight: 600;
        color: #2d5a27;
        margin-bottom: 8px;
      }
      
      .radio label, .checkbox label {
        font-weight: normal;
        color: #495057;
      }
      
      .btn-primary {
        background-color: #2d5a27;
        border-color: #2d5a27;
      }
      
      .btn-primary:hover {
        background-color: #1e3a1b;
        border-color: #1e3a1b;
      }
      
      .leaflet-container {
        border-radius: 8px;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
      }
      
      .info-box {
        background: #e8f5e8;
        border: 1px solid #c3e6c3;
        border-radius: 6px;
        padding: 10px;
        margin-bottom: 10px;
      }
      
      .info-box h4 {
        color: #2d5a27;
        margin-top: 0;
        margin-bottom: 5px;
        font-size: 1.25rem; 
      }
      
      .info-box p {
        margin-bottom: 5px;
        font-size: 1.25rem;
  }
      
      .radio input[type='radio']:checked + span {
        color: #2d5a27;
        font-weight: 600;
      }
      
      .selectize-input {
        border: 2px solid #e9ecef;
        border-radius: 6px;
      }
      
      .selectize-input.focus {
        border-color: #2d5a27;
        box-shadow: 0 0 0 0.2rem rgba(45, 90, 39, 0.25);
      }
    "))
  ),
  
  div(class = "main-header",
      h1("National Park Service", style = "margin-bottom: 5px;"),
      h1("Water Supply Risk Assessment Tool", style = "font-size: 1.8rem; font-weight: 400;")#,
      #div(class = "subtitle", "Water Supply Systems Wildfire Risk Analysis")
  ),
  
  div(class = "control-panel",
      # Information box
      div(class = "info-box",
          h4(icon("info-circle"), " How to Use This Tool"),
          p("Select a risk category, indicator, and statistical measure. Choose between raw values or percentile rankings. 
            Larger, darker circles indicate higher risk. Click points for detailed information."),
          # textOutput("indicator_description")
      ),
      # Controls 
      fluidRow(
        column(3,
               h5("Risk Category", style = "color: #2d5a27; margin-bottom: 15px; font-weight: 600;"),
               radioButtons(
                 "risk_category",
                 label = NULL,
                 choices = list(
                   "Exposure" = "exposure",
                   "Sensitivity" = "sensitivity"
                 ),
                 selected = "exposure"
               )
        ),
        column(3,
               selectInput(
                 "indicator",
                 label = tags$span("Risk Indicator", style = "color: #2d5a27; font-weight: 600;"),
                 choices = NULL
               )
        ),
        column(3,
               selectInput(
                 "measure",
                 label = tags$span("Statistical Measure", style = "color: #2d5a27; font-weight: 600;"),
                 choices = NULL
               )
        ),
        column(3,
               h5("Data Type", style = "color: #2d5a27; margin-bottom: 15px; font-weight: 600;"),
               radioButtons(
                 "data_type",
                 label = NULL,
                 choices = list(
                   "Raw Values" = "raw",
                   "Percentile Ranking" = "rank"
                 ),
                 selected = "raw"
               )
        )
      ),
      
     
  ),
  
  # Map section
  fluidRow(
    column(12,
           div(style = "position: relative;",
               uiOutput("map_title"),
               leafletOutput("map", height = "calc(100vh - 300px)")
           )
    )
  ),
  
  # Footer
  div(style = "margin-top: 30px; padding: 20px; background-color: #2d5a27; color: white; text-align: center;",
      p("Application developed by the Colorado State University Geospatial Centroid | Data current as of August 2025", 
        style = "margin: 0; opacity: 0.9;")
  )
)

# Server
server <- function(input, output, session) {
  
  # Update indicator choices based on risk category
  observe({
    category_indicators <- indicator_config[[input$risk_category]]
    indicator_choices <- names(category_indicators)
    names(indicator_choices) <- indicator_choices
    
    # Set default indicator based on category
    default_indicator <- if(input$risk_category == "exposure") {
      "Runoff Change"  # Default to runoff for exposure
    } else {
      indicator_choices[1]  # First indicator for sensitivity
    }
    
    updateSelectInput(
      session, 
      "indicator",
      choices = indicator_choices,
      selected = default_indicator
    )
  })
  
  # Update measure choices based on selected indicator
  observe({
    req(input$indicator, input$risk_category)
    
    indicator_info <- indicator_config[[input$risk_category]][[input$indicator]]
    measure_choices <- indicator_info$measures
    
    # Set default measure - 10th Percentile for Runoff Change, otherwise first option
    default_measure <- if(input$indicator == "Runoff Change" && "10th Percentile" %in% names(measure_choices)) {
      "10th Percentile"
    } else {
      names(measure_choices)[1]
    }
    
    updateSelectInput(
      session,
      "measure", 
      choices = measure_choices,
      selected = measure_choices[default_measure]
    )
  })
  
  # Generate column name based on selections
  column_name <- reactive({
    req(input$risk_category, input$indicator, input$measure, input$data_type)
    
    # Get base name and measure
    indicator_info <- indicator_config[[input$risk_category]][[input$indicator]]
    base_name <- indicator_info$base_name
    measure_code <- input$measure
    
    # Create standardized column name
    col_name <- paste(input$data_type, base_name, measure_code, sep = "_")
    
    # Validate column exists in data
    if (!col_name %in% names(combined_data)) {
      # Try alternative naming patterns for backwards compatibility
      alt_names <- c(
        paste0(base_name, "_", measure_code),
        paste0("raw_", base_name, "_", measure_code),
        paste0("rank_", base_name, "_", measure_code)
      )
      
      existing_alt <- alt_names[alt_names %in% names(combined_data)]
      if (length(existing_alt) > 0) {
        col_name <- existing_alt[1]
      }
    }
    
    return(col_name)
  })
  
  # Filter data for mapping
  map_data <- reactive({
    req(column_name())
    
    # Check if column exists
    if (!column_name() %in% names(combined_data)) {
      return(NULL)
    }
    
    # Filter out missing values
    filtered_data <- combined_data[!is.na(combined_data[[column_name()]]), ]
    
    return(filtered_data)
  })
  
  # Display indicator description
  # output$indicator_description <- renderText({
  #   req(input$risk_category, input$indicator)
  #   
  #   indicator_info <- indicator_config[[input$risk_category]][[input$indicator]]
  #   indicator_info$description
  # })
  
  # Generate dynamic map title
  output$map_title <- renderUI({
    req(input$risk_category,
        input$indicator,
        input$measure,
        input$data_type)
    
    # Build title text
    indicator_info <- indicator_config[[input$risk_category]][[input$indicator]]
    title_text <- indicator_info$description
    
    # Return styled title with absolute positioning
    div(
      style = "position: absolute; top: 10px; left: 60px;
             background: white; padding: 10px 15px; 
             border-radius: 6px; box-shadow: 0 2px 8px rgba(0, 0, 0, 0.2);
             border-left: 4px solid #2d5a27; z-index: 1000;
             max-width: 200px;",
      h5(title_text, 
         style = "margin: 0; color: #2d5a27; font-weight: 600; font-size: 1.25rem;")
    )
  })
  
  # Initialize the base map once
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -98.5, lat = 39.8, zoom = 4)
  })
  
  # Update map when selections change
  observe({
    req(map_data(), column_name())
    
    plot_data <- map_data()
    
    if (nrow(plot_data) == 0) {
      # Clear map if no data
      leafletProxy("map") %>%
        clearMarkers() %>%
        clearControls()
      return()
    }
    
    # Get values for mapping
    values <- plot_data[[column_name()]]
    
    # Determine if this is a "lower is worse" indicator (like runoff change)
    is_inverse_risk <- str_detect(column_name(), "runoff_change")
    
    # Create color palette
    if (is_inverse_risk) {
      # For runoff change: more negative = higher risk (red)
      pal <- colorNumeric(
        palette = c("#bd0026", "#f03b20", "#fd8d3c", "#fed976", "#ffffcc"),
        domain = values
      )
      # Size mapping: more negative values get larger circles
      size_values <- -values
    } else {
      # Standard: higher values = higher risk (red)
      pal <- colorNumeric(
        palette = c("#ffffcc", "#fed976", "#fd8d3c", "#f03b20", "#bd0026"),
        domain = values
      )
      size_values <- values
    }
    
    # Create dynamic legend title
    legend_title <- paste0(
      str_wrap(input$indicator, 20), "\n",
      input$measure,
      if (input$data_type == "rank") " (Percentile)" else ""
    )
    
    # Update map
    leafletProxy("map") %>%
      clearMarkers() %>%
      clearControls() %>%
      addCircleMarkers(
        data = plot_data,
        radius = ~ pmax(4, pmin(16, scales::rescale(
          if (is_inverse_risk) -get(column_name()) else get(column_name()),
          to = c(4, 16)
        ))),
        color = "#2c2c2c",
        fillColor = ~ pal(get(column_name())),
        fillOpacity = 0.8,
        stroke = TRUE,
        weight = 1,
        popup = ~ create_popup(
          indicator = input$indicator,
          measure = input$measure,
          value = get(column_name()),
          wsd_source_id = wsd_source_id,
          park_unit = park_unit,
          park_name = park_name,
          water_system_name = water_system_name,
          state = state
        )
      ) %>%
      addLegend(
        pal = pal,
        values = values,
        title = legend_title,
        position = "bottomright",
        opacity = 0.9
      )
  })
  
  # Create standardized popup content
  create_popup <- function(indicator, measure, value, wsd_source_id, 
                           park_unit, park_name, water_system_name, state) {
    paste0(
      "<div style='font-family: Arial; font-size: 12px;'>",
      "<b style='color: #2d5a27; font-size: 14px;'>", indicator, "</b><br>",
      "<b>", measure, ":</b> <span style='font-size: 14px; font-weight: bold;'>", 
      round(value, 3), "</span><br><br>",
      "<b>Water Supply ID:</b> ", wsd_source_id, "<br>",
      "<b>Park Unit:</b> ", park_unit, "<br>",
      "<b>Park Name:</b> ", park_name, "<br>",
      "<b>Water System:</b> ", water_system_name, "<br>",
      "<b>State:</b> ", state,
      "</div>"
    )
  }
}

# Run the application
shinyApp(ui = ui, server = server)