library(shiny)
library(leaflet)
library(sf)
library(readxl)
library(dplyr)
library(readr)
library(scales)
library(stringr)
library(purrr)

# load in data
parks <- st_read("app_data/park_boundaries_2025-08-14.gpkg")

# read in water supply database
water_supplies <- read_csv("app_data/water_supplies.csv")%>% 
  mutate(id = water_supply_id)

# read in indicators
fire_exp <- read_csv("app_data/fire_exp_2025-08-26.csv")
fire_sen <- read_csv("app_data/fire_sen_2025-08-26.csv")
runoff <- read_csv("app_data/runoff_vulnerability_indicator.csv") %>%
  mutate(id = str_remove(wsd_source_id, "_0\\d+$")) %>% 
  select(id, "Change in Runoff 10th Percentile" = p10_percent_change,
         "Runoff Decrease Model Percentage" = percent_models_negative) %>% 
  mutate(mutate(across(
    where(is.numeric),
    .fns = list(pcntl = ~ cume_dist(.) * 100),
    .names = "{col}_{fn}"
  )))

data <- reduce(list(water_supplies, fire_exp, fire_sen, runoff), left_join, by = "id") %>% 
  select(source_longitude, source_latitude, park_name, region, state, park_code, water_system_name, water_supply_id, names(fire_exp)[-c(1:2)],
         names(fire_sen)[-c(1:2)], names(runoff)[-1])  %>% 
  # change beginning string
  rename_with(~str_replace(.x, "delta_fp", "Change in fire probability "), 
              .cols = contains("delta_fp")) %>% 
  rename_with(~str_replace(.x, "mean_whp", "Wildfire hazard potential "), 
              .cols = contains("mean_whp")) %>% 
  # change end string
  rename_with(~str_replace(.x, "_p10", "10th Percentile"), 
              .cols = contains("_p10")) %>% 
  rename_with(~str_replace(.x, "_p50", "Median"), 
              .cols = contains("_p50")) %>% 
  rename_with(~str_replace(.x, "_p90", "90th Percentile"), 
              .cols = contains("_p90")) %>% 
  rename_with(~str_replace(.x, "_ws_buffer", "Mean"), 
              .cols = contains("_ws_buffer")) %>% 
  st_as_sf(coords = c("source_longitude", "source_latitude"), crs = 4326)

vars <- names(data)[str_detect(names(data), "Percentile|Median|Mean|Percentage")]

# UI with enhanced styling
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
        padding: 25px 15px;
        margin-bottom: 25px;
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
        padding: 20px;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
        margin-bottom: 20px;
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
        padding: 15px;
        margin-bottom: 20px;
      }
      
      .info-box h4 {
        color: #2d5a27;
        margin-top: 0;
        margin-bottom: 10px;
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
      fluidRow(
        column(3,
               div(
                 h5("Risk Category", style = "color: #2d5a27; margin-bottom: 15px;"),
                 radioButtons(
                   "metric_type",
                   label = NULL,
                   choices = list(
                     "Exposure" = "exposure", 
                     "Sensitivity" = "sensitivity"
                   ),
                   selected = "exposure",
                   inline = FALSE
                 )
               )
        ),
        column(3,
               selectInput(
                 "color_var",
                 label = tags$span("Risk Indicator", style = "color: #2d5a27; font-weight: 600;"),
                 choices = NULL,
                 width = "100%"
               )
        ),
        column(3,
               selectInput(
                 "value",
                 label = tags$span("Statistical Measure", style = "color: #2d5a27; font-weight: 600;"),
                 choices = NULL,
                 width = "100%"
               )
        ),
        column(3,
               div(
                 h5("Data Type", style = "color: #2d5a27; margin-bottom: 15px;"),
                 radioButtons(
                   "var_type",
                   label = NULL,
                   choices = list(
                     "Actual Values" = "Raw Value",
                     "Percentile Ranking" = "Ranking"
                   ),
                   selected = "Raw Value",
                   inline = FALSE
                 )
               )
        )
      ),
      
      div(class = "info-box",
          h4(icon("info-circle"), " How to Use This Tool"),
          p("Select a risk category (Exposure or Sensitivity), choose your indicator and statistical measure, 
            then select whether to view actual values or percentile rankings. Larger, darker red circles 
            indicate higher risk values. Click on any point for detailed information.")
      )
  ),
  
  # Map section
  fluidRow(
    column(12,
           leafletOutput("map", height = "calc(100vh - 400px)")
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
  
  # Update indicator choices based on metric type
  observe({
    if (input$metric_type == "exposure") {
      # Get all exposure indicators
      exposure_cols <- names(data)[str_detect(names(data), "Change in fire probability|Change in Runoff|Runoff Decrease Model")]
      
      # Extract base indicator names
      exposure_indicators <- unique(c(
        str_replace(exposure_cols[str_detect(exposure_cols, "Change in fire probability")], " (10th Percentile|Median|90th Percentile).*", ""),
        str_replace(exposure_cols[str_detect(exposure_cols, "Change in Runoff")], " (10th Percentile|Median|90th Percentile).*", ""),
        str_replace(exposure_cols[str_detect(exposure_cols, "Runoff Decrease Model")], " Percentage.*", "")
      ))
      
      updateSelectInput(session, "color_var",
                        choices = setNames(exposure_indicators, exposure_indicators),
                        selected = exposure_indicators[1])
    } else if (input$metric_type == "sensitivity") {
      sensitivity_cols <- names(data)[str_detect(names(data), "Wildfire hazard potential")]
      sensitivity_indicators <- unique(str_replace(sensitivity_cols, " Mean.*", ""))
      
      updateSelectInput(session, "color_var",
                        choices = setNames(sensitivity_indicators, sensitivity_indicators),
                        selected = sensitivity_indicators[1])
    }
  })
  
  # Update value choices based on selected indicator
  observe({
    req(input$color_var)
    
    matching_cols <- names(data)[str_detect(names(data), paste0("^", str_escape(input$color_var)))]
    
    if (input$metric_type == "exposure") {
      if (str_detect(input$color_var, "Runoff Decrease Model")) {
        # For Runoff Decrease Model, extract "Percentage"
        value_parts <- str_extract(matching_cols, "Percentage")
      } else {
        # For other exposure variables, extract percentiles
        value_parts <- str_extract(matching_cols, "(10th Percentile|Median|90th Percentile)")
      }
    } else {
      value_parts <- str_extract(matching_cols, "Mean")
    }
    
    unique_values <- unique(value_parts[!is.na(value_parts)])
    
    updateSelectInput(session, "value",
                      choices = setNames(unique_values, unique_values),
                      selected = unique_values[1])
  })
  
  # Create the actual column name based on all selections
  selected_column <- reactive({
    req(input$color_var, input$value, input$var_type)
    
    base_name <- paste(input$color_var, input$value)
    
    if (input$var_type == "Ranking") {
      base_name <- paste0(base_name, "_pcntl")
    }
    
    return(base_name)
  })
  # Initialize the base map once
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -98.5, lat = 39.8, zoom = 4)
  })
  
  # Update map markers when variable changes
  observe({
    req(selected_column())
    
    if (!selected_column() %in% names(data)) {
      return()
    }
    
    var_values <- data[[selected_column()]]
    plot_data <- data[!is.na(var_values), ]
    var_values_clean <- var_values[!is.na(var_values)]
    
    # Check if this is a Change in Runoff variable (smaller values = higher risk)
    is_runoff_change <- str_detect(selected_column(), "Change in Runoff")
    
    # Create color palette - reverse for Change in Runoff
    if (is_runoff_change) {
      pal <- colorNumeric(
        palette = c("#bd0026", "#f03b20", "#fd8d3c", "#fed976", "#ffffcc"),
        domain = var_values_clean
      )
      # For runoff change: smaller values get larger circles
      size_values <- -var_values_clean  # Negate so smaller becomes larger
    } else {
      pal <- colorNumeric(
        palette = c("#ffffcc", "#fed976", "#fd8d3c", "#f03b20", "#bd0026"),
        domain = var_values_clean
      )
      size_values <- var_values_clean
    }
    
    # Create dynamic title for legend
    legend_title <- if(input$var_type == "Ranking") {
      paste0(str_wrap(input$color_var, 20), "\n", input$value, " (Percentile)")
    } else {
      paste0(str_wrap(input$color_var, 20), "\n", input$value)
    }
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      clearControls() %>%
      addCircleMarkers(
        data = plot_data,
        radius = ~ if(str_detect(selected_column(), "Change in Runoff")) {
          # For Change in Runoff: more negative values (closer to -70) = larger circles
          pmax(4, pmin(16, scales::rescale(-get(selected_column()), to = c(4, 16))))
        } else {
          # For other variables: higher values = larger circles
          pmax(4, pmin(16, scales::rescale(get(selected_column()), to = c(4, 16))))
        },
        color = "#2c2c2c",
        fillColor = ~ pal(get(selected_column())),
        fillOpacity = 0.8,
        stroke = TRUE,
        weight = 1,
        popup = ~ paste0(
          "<div style='font-family: Arial; font-size: 12px;'>",
          "<b style='color: #2d5a27;'>", selected_column(), ":</b><br>",
          "<span style='font-size: 14px; font-weight: bold;'>", 
          round(get(selected_column()), 3), "</span><br><br>",
          "<b>Water Supply ID:</b> ", water_supply_id, "<br>",
          "<b>Park Unit:</b> ", park_code, "<br>",
          "<b>Water System:</b> ", water_system_name, "<br>",
          "<b>State:</b> ", state,
          "</div>"
        )
      ) %>%
      addLegend(
        pal = pal,
        values = var_values_clean,
        title = legend_title,
        position = "bottomright",
        opacity = 0.9
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)