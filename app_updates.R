library(shiny)
library(leaflet)
library(leaflegend)
library(sf)
library(dplyr)
library(readr)
library(scales)
library(stringr)
library(purrr)
library(later)
library(promises)
library(future)
plan(multisession)

############# DATA ############

parks <- st_read("app_data/park_boundaries_2025-08-14.gpkg") %>%
  st_transform(st_crs = 4326)

# Read water supplies — provides lat/lon and site metadata
water_supplies <- read_csv("app_data/water_supplies.csv") %>%
  select(wsd_source_id, park_unit, park_name, region, state, 
         water_system_name, source_longitude, source_latitude)

# Read final vulnerability index — output of vulnerability_index_calculation.Rmd.
# Contains pre-computed normalized indicators, raw indicators, factor scores,
# component scores, total vulnerability score, and hazard flags.
# Replace this file whenever the index is recalculated; no other changes needed.
final_index <- read_csv("app_data/final_index.csv") %>%
  select(-c(park_unit, park_name))

# Join index to water supply coordinates and convert to spatial object
combined_data <- water_supplies %>%
  left_join(final_index, by = "wsd_source_id") %>%
  st_as_sf(coords = c("source_longitude", "source_latitude"), crs = 4326)

# ---------------------------------------------------------------------------
# Indicator hierarchy config: Component -> Factor -> Indicator
#
# Each entry maps a UI label to the pre-computed normalized column in
# final_index.csv (all values 0-1, higher = more vulnerable).
#
# To add a new indicator: add its normalized column to final_index.csv and
# register it here under the appropriate Component and Factor.
# ---------------------------------------------------------------------------
indicator_config <- list(
  
  "Exposure" = list(
    
    "Runoff" = list(
      "Change in Runoff" = list(
        col         = "exp_runoff_change",
        raw_col     = "raw_runoff_change",
        raw_label   = "% change mean annual runoff (p10)",
        description = "Projected % change in mean annual runoff (p10, most vulnerable direction)"
      ),
      "Runoff Model Agreement" = list(
        col         = "exp_runoff_model_agree",
        raw_col     = "raw_runoff_model_agree",
        raw_label   = "% GCMs predicting decrease",
        description = "% of GCMs predicting a decrease in runoff"
      )
    ),
    
    "Precipitation" = list(
      "Change in Precipitation" = list(
        col         = "exp_precip_change",
        raw_col     = "raw_precip_change",
        raw_label   = "% change mean annual precip (p10)",
        description = "Projected % change in mean annual precipitation (p10)"
      ),
      "Precipitation Model Agreement" = list(
        col         = "exp_precip_model_agree",
        raw_col     = "raw_precip_model_agree",
        raw_label   = "% models predicting decrease",
        description = "% of climate models predicting a decrease in precipitation"
      )
    ),
    
    "Sea Level Rise" = list(
      "Inundation from Sea Level Rise" = list(
        col         = "exp_inundation_slr",
        raw_col     = "raw_inundation_slr",
        raw_label   = "% point change in inundated area",
        description = "Projected % change in area inundated by sea level rise"
      )
    ),
    
    "Wildfire" = list(
      "Change in Fire Probability" = list(
        col         = "exp_fire_prob_change",
        raw_col     = "raw_fire_prob_change",
        raw_label   = "% change fire probability (p90)",
        description = "Projected % change in probability of wildfire (p90)"
      )
    )
  ),
  
  "Sensitivity" = list(
    
    "Demand" = list(
      "Historic Visitation Trend" = list(
        col         = "sen_visitation_trend",
        raw_col     = "raw_visitation_trend",
        raw_label   = "Scaled visitation trend",
        description = "Historic trend in park visitation (scaled)"
      ),
      "Competition (Water Use Trend)" = list(
        col         = "sen_competition",
        raw_col     = "raw_competition",
        raw_label   = "Water use trend slope (per km\u00b2)",
        description = "Trend in nearby county water use (competition for supply)"
      )
    ),
    
    "Wildfire" = list(
      "Current Wildfire Risk" = list(
        col         = "sen_wildfire_hazard",
        raw_col     = "raw_wildfire_hazard",
        raw_label   = "Mean Wildfire Hazard Potential",
        description = "Current Wildfire Hazard Potential index"
      )
    ),
    
    "Flood" = list(
      "Current Flood Risk" = list(
        col         = "sen_flood_risk",
        raw_col     = "raw_flood_risk",
        raw_label   = "% area in FEMA flood zone",
        description = "% of surrounding area in a high-risk FEMA flood zone"
      )
    ),
    
    "Sea Level Rise" = list(
      "Current Inundation" = list(
        col         = "sen_inundation_current",
        raw_col     = "raw_inundation_current",
        raw_label   = "% catchment area inundated (reference)",
        description = "% of catchment area currently inundated (reference condition)"
      )
    ),
    
    "Runoff" = list(
      "Historic Runoff Trend" = list(
        col         = "sen_runoff_trend",
        raw_col     = "raw_runoff_trend",
        raw_label   = "Mann-Kendall slope (30-yr runoff)",
        description = "30-year historic trend in runoff (Mann-Kendall slope)"
      )
    ),
    
    "Precipitation" = list(
      "Historic Precipitation Trend" = list(
        col         = "sen_precip_trend",
        raw_col     = "raw_precip_trend",
        raw_label   = "Mann-Kendall slope (40-yr precip)",
        description = "40-year historic trend in precipitation (Mann-Kendall slope)"
      )
    )
  )
)

# Score / factor columns available in Score View mode.
# Names are display labels; values are column names in combined_data.
score_views <- list(
  # Composite scores
  "Total Vulnerability Score" = "VULNERABILITY",
  "Exposure Score"            = "EXPOSURE",
  "Sensitivity Score"         = "SENSITIVITY",
  # Exposure factor scores
  "Factor: Runoff Exposure"           = "factor_exp_runoff",
  "Factor: Precipitation Exposure"    = "factor_exp_precip",
  "Factor: Sea Level Rise Exposure"   = "factor_exp_slr",
  "Factor: Wildfire Exposure"         = "factor_exp_wildfire",
  # Sensitivity factor scores
  "Factor: Demand Sensitivity"        = "factor_sen_demand",
  "Factor: Wildfire Sensitivity"      = "factor_sen_wildfire",
  "Factor: Flood Sensitivity"         = "factor_sen_flood",
  "Factor: Sea Level Rise Sensitivity"= "factor_sen_slr",
  "Factor: Runoff Sensitivity"        = "factor_sen_runoff",
  "Factor: Precipitation Sensitivity" = "factor_sen_precip"
)

###################### UI ###############################

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f8f9fa;
        font-family: 'Arial', 'Helvetica', sans-serif;
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
        font-size: 1.1rem;
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

      .loading-overlay {
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        background: white;
        padding: 20px 30px;
        border-radius: 8px;
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.3);
        z-index: 1000;
        border-left: 4px solid #2d5a27;
      }

      .loading-overlay h4 {
        margin: 0;
        color: #2d5a27;
        font-weight: 600;
      }
    "))
  ),
  
  div(class = "main-header",
      h1("National Park Service", style = "margin-bottom: 5px;"),
      h1("Water Supply Vulnerability Assessment Tool",
         style = "font-size: 1.8rem; font-weight: 400;")
  ),
  
  div(class = "control-panel",
      
      div(class = "info-box",
          h4(icon("info-circle"), " How to Use This Tool"),
          p(HTML("The map opens on the <strong>Total Vulnerability Score</strong>
            (Exposure &amp; Sensitivity combined via Euclidean distance, rescaled 0&ndash;1;
            Michalak et al. 2021). Use <em>Score View</em> to explore component and
            factor-level scores, or switch to <em>Specific Indicator</em> to drill down:
            <strong>Component &rarr; Factor &rarr; Indicator &rarr; Metric</strong>.
            Larger, darker circles indicate higher vulnerability. Click any point for details."))
      ),
      
      fluidRow(
        
        # View mode toggle
        column(3,
               h5("View Mode", style = "color: #2d5a27; margin-bottom: 15px; font-weight: 600;"),
               radioButtons(
                 "view_mode",
                 label = NULL,
                 choices = list(
                   "Score View"         = "score",
                   "Specific Indicator" = "indicator"
                 ),
                 selected = "score"
               )
        ),
        
        # Score view selector (shown when view_mode == "score")
        conditionalPanel(
          condition = "input.view_mode == 'score'",
          column(5,
                 selectInput(
                   "score_view",
                   label = tags$span("Score", style = "color: #2d5a27; font-weight: 600;"),
                   choices  = names(score_views),
                   selected = "Total Vulnerability Score"
                 )
          )
        ),
        
        # Indicator drilldown (shown when view_mode == "indicator")
        conditionalPanel(
          condition = "input.view_mode == 'indicator'",
          column(2,
                 selectInput(
                   "component",
                   label = tags$span("Component",
                                     style = "color: #2d5a27; font-weight: 600;"),
                   choices = names(indicator_config)
                 )
          ),
          column(2,
                 selectInput(
                   "factor",
                   label = tags$span("Factor",
                                     style = "color: #2d5a27; font-weight: 600;"),
                   choices = NULL
                 )
          ),
          column(3,
                 selectInput(
                   "indicator",
                   label = tags$span("Indicator",
                                     style = "color: #2d5a27; font-weight: 600;"),
                   choices = NULL
                 )
          ),
          column(2,
                 selectInput(
                   "metric",
                   label = tags$span("Metric",
                                     style = "color: #2d5a27; font-weight: 600;"),
                   choices = c("Normalized (0\u20131)" = "norm",
                               "Raw value"            = "raw")
                 )
          )
        )
      )
  ),
  
  # Map
  fluidRow(
    column(12,
           div(style = "position: relative;",
               uiOutput("map_title"),
               conditionalPanel(
                 condition = "output.parks_loading",
                 div(class = "loading-overlay",
                     h4(icon("spinner", class = "fa-spin"), " Loading park boundaries..."))
               ),
               leafletOutput("map", height = "calc(100vh - 300px)")
           )
    )
  ),
  
  # Footer
  div(style = "margin-top: 30px; padding: 20px; background-color: #2d5a27;
               color: white; text-align: center;",
      p("Application developed by the Colorado State University Geospatial Centroid | Data current as of 2025",
        style = "margin: 0; opacity: 0.9;")
  )
)

############################ SERVER ##############################

server <- function(input, output, session) {
  
  # Parks loading state
  parks_loading <- reactiveVal(FALSE)
  output$parks_loading <- reactive({ parks_loading() })
  outputOptions(output, "parks_loading", suspendWhenHidden = FALSE)
  
  # ── Cascade: Component -> Factor ────────────────────────────────────────
  observe({
    req(input$component)
    factors <- names(indicator_config[[input$component]])
    updateSelectInput(session, "factor", choices = factors, selected = factors[1])
  })
  
  # ── Cascade: Factor -> Indicator ────────────────────────────────────────
  observe({
    req(input$component, input$factor)
    inds <- names(indicator_config[[input$component]][[input$factor]])
    updateSelectInput(session, "indicator", choices = inds, selected = inds[1])
  })
  
  # ── Resolve which column to map ─────────────────────────────────────────
  active_column <- reactive({
    if (input$view_mode == "score") {
      col <- score_views[[input$score_view]]
    } else {
      req(input$component, input$factor, input$indicator, input$metric)
      
      # During cascade updates the dropdowns briefly hold stale values —
      # validate the full path resolves before doing anything.
      cfg <- indicator_config[[input$component]][[input$factor]][[input$indicator]]
      req(!is.null(cfg))  # silently cancel if path not yet valid
      
      col <- if (input$metric == "raw") cfg$raw_col else cfg$col
    }
    
    # Only notify if the column name is non-empty but genuinely missing from
    # the data — not during normal cascade transitions where col may be NULL.
    if (is.null(col) || nchar(col) == 0) return(NULL)
    
    if (!col %in% names(combined_data)) {
      showNotification(
        paste0("Column '", col, "' not found. Check final_index.csv."),
        type = "warning", duration = 5
      )
      return(NULL)
    }
    col
  })
  
  # ── Map title overlay ────────────────────────────────────────────────────
  output$map_title <- renderUI({
    if (input$view_mode == "score") {
      title_text <- input$score_view
      # Colour-code border by score type
      border_col <- if (grepl("Exposure", title_text)) "#2a6f97" else
        if (grepl("Sensitivity", title_text)) "#c17817" else "#1a8a7d"
    } else {
      req(input$component, input$factor, input$indicator)
      title_text <- indicator_config[[input$component]][[input$factor]][[input$indicator]]$description
      border_col <- if (input$component == "Exposure") "#2a6f97" else "#c17817"
    }
    
    div(
      style = paste0(
        "position: absolute; top: 10px; left: 60px;
         background: white; padding: 10px 15px;
         border-radius: 6px; box-shadow: 0 2px 8px rgba(0, 0, 0, 0.2);
         border-left: 4px solid ", border_col, "; z-index: 1000; max-width: 240px;"
      ),
      h5(title_text,
         style = paste0("margin: 0; color: ", border_col,
                        "; font-weight: 600; font-size: 1.1rem;"))
    )
  })
  
  # ── Base map (rendered once) ─────────────────────────────────────────────
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -98.5, lat = 39.8, zoom = 4) %>%
      addMapPane("background", zIndex = 410) %>%
      addLayersControl(
        overlayGroups = c("Park Boundaries"),
        options = layersControlOptions(collapsed = FALSE),
        position = "topleft"
      ) %>%
      hideGroup("Park Boundaries")
  })
  
  # ── Park boundaries toggle ───────────────────────────────────────────────
  observeEvent(input$map_groups, {
    if ("Park Boundaries" %in% input$map_groups) {
      parks_loading(TRUE)
      Sys.sleep(0.3)
      leafletProxy("map", session = session) %>%
        addPolygons(
          data = parks,
          group = "Park Boundaries",
          fillColor = "lightgreen",
          fillOpacity = 0.1,
          color = "#2d5a27",
          weight = 1.5,
          opacity = 0.6,
          options = pathOptions(pane = "background"),
          popup = ~paste0(
            "<div style='font-family: Arial; font-size: 12px;'>",
            "<b style='color: #2d5a27; font-size: 14px;'>", UNIT_NAME, "</b><br>",
            "<b>Park Code:</b> ", UNIT_CODE,
            "</div>"
          )
        )
      parks_loading(FALSE)
    } else {
      leafletProxy("map", session = session) %>%
        clearGroup("Park Boundaries")
    }
  }, ignoreNULL = FALSE)
  
  # ── Update map markers when any selection changes ────────────────────────
  observe({
    req(active_column())
    col       <- active_column()
    plot_data <- combined_data
    values    <- plot_data[[col]]
    
    # Scores and normalized indicators use a fixed 0-1 domain for colour
    # consistency. Raw values use a data-driven domain.
    is_raw_indicator <- (input$view_mode == "indicator" && isTRUE(input$metric == "raw"))
    val_range   <- range(values, na.rm = TRUE)
    pal_domain  <- if (is_raw_indicator) val_range else c(0, 1)
    size_domain <- if (is_raw_indicator) val_range else c(0, 1)
    
    pal <- colorNumeric(
      palette  = c("#ffffcc", "#fed976", "#fd8d3c", "#f03b20", "#bd0026"),
      domain   = pal_domain,
      na.color = "lightgrey"
    )
    
    # Legend title
    legend_title <- if (input$view_mode == "score") {
      str_wrap(input$score_view, 20)
    } else {
      metric_label <- if (is_raw_indicator) "Raw value" else "Normalized (0\u20131)"
      paste0(str_wrap(input$indicator, 20), "\n", metric_label)
    }
    
    # Legend values: pass the two-element range (+ NA) so addLegendNumeric
    # gets a scalar domain, not the full data vector
    legend_values <- if (is_raw_indicator) c(val_range, NA) else c(0, 1, NA)
    
    # Pre-compute scalar values used inside the tilde formula.
    # if() inside ~ is evaluated per-row, so input$* comparisons there
    # produce length-n vectors and trigger "condition has length > 1".
    is_indicator  <- input$view_mode == "indicator"
    
    if (is_indicator) {
      req(input$component, input$factor, input$indicator)
      ind_cfg          <- indicator_config[[input$component]][[input$factor]][[input$indicator]]
      popup_metric      <- input$metric
      popup_score_label <- NA_character_
      popup_component   <- input$component
      popup_factor      <- input$factor
      popup_indicator   <- input$indicator
      popup_raw_label   <- ind_cfg$raw_label
      norm_col          <- ind_cfg$col
      raw_col_name      <- ind_cfg$raw_col
    } else {
      popup_metric      <- "norm"
      popup_score_label <- input$score_view
      popup_component   <- NA_character_
      popup_factor      <- NA_character_
      popup_indicator   <- NA_character_
      popup_raw_label   <- NA_character_
      norm_col          <- NULL
      raw_col_name      <- NULL
    }
    
    # Pre-compute per-row vectors outside tilde formulas.
    # Extract data frame once to avoid repeated sf conversion.
    df        <- as.data.frame(plot_data)
    plot_vals <- df[[col]]
    norm_vals <- if (is_indicator && !is.null(norm_col))     df[[norm_col]]     else rep(NA_real_, nrow(df))
    raw_vals  <- if (is_indicator && !is.null(raw_col_name)) df[[raw_col_name]] else rep(NA_real_, nrow(df))
    
    radius_vec <- ifelse(
      is.na(plot_vals), 3,
      pmax(4, pmin(16, scales::rescale(plot_vals, to = c(4, 16), from = size_domain)))
    )
    
    fill_vec <- pal(plot_vals)
    
    # Build popup HTML row-by-row using Map() which always returns a list,
    # then unlist to a character vector. Scalar arguments are recycled safely.
    popup_vec <- unlist(Map(
      create_popup,
      norm_value        = norm_vals,
      raw_value         = raw_vals,
      col_value         = plot_vals,
      vuln              = df[["VULNERABILITY"]],
      exposure          = df[["EXPOSURE"]],
      sensitivity       = df[["SENSITIVITY"]],
      priority          = df[["priority_national"]],
      flag_fire         = df[["flag_fire"]],
      flag_flood        = df[["flag_flood"]],
      flag_slr          = df[["flag_slr"]],
      flag_drought      = df[["flag_drought"]],
      wsd_source_id     = df[["wsd_source_id"]],
      park_unit         = df[["park_unit"]],
      park_name         = df[["park_name"]],
      water_system_name = df[["water_system_name"]],
      state             = df[["state"]],
      # Scalar args recycled across all rows
      MoreArgs = list(
        view_mode      = input$view_mode,
        metric         = popup_metric,
        score_label    = popup_score_label,
        component      = popup_component,
        factor_name    = popup_factor,
        indicator_name = popup_indicator,
        raw_label      = popup_raw_label
      )
    ))
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      clearControls() %>%
      addCircleMarkers(
        data        = plot_data,
        radius      = radius_vec,
        color       = "#2c2c2c",
        fillColor   = fill_vec,
        fillOpacity = 0.9,
        stroke      = TRUE,
        weight      = 1,
        popup       = popup_vec
      ) %>%
      addLegendNumeric(
        pal      = pal,
        values   = legend_values,
        title    = legend_title,
        naLabel  = "No Data",
        position = "bottomright"
      )
  })
  
  # ── Popup builder ────────────────────────────────────────────────────────
  create_popup <- function(view_mode, metric, score_label, component, factor_name,
                           indicator_name, norm_value, raw_value, raw_label, col_value,
                           vuln, exposure, sensitivity,
                           priority, flag_fire, flag_flood, flag_slr, flag_drought,
                           wsd_source_id, park_unit, park_name,
                           water_system_name, state) {
    
    # Top section: what is being shown
    top_section <- if (view_mode == "score") {
      paste0(
        "<b style='color: #1a8a7d; font-size: 14px;'>", score_label, "</b><br>",
        "<b>Value (0\u20131):</b> <span style='font-size: 14px; font-weight: bold;'>",
        round(col_value, 3), "</span>"
      )
    } else {
      # Raw value row — always show regardless of which metric is mapped
      raw_row <- if (!is.na(raw_value)) {
        paste0(
          "<b>", raw_label, ":</b> ",
          "<span style='font-size: 13px; font-weight: bold;'>",
          round(raw_value, 3), "</span><br>"
        )
      } else {
        paste0("<b>", raw_label, ":</b> <span style='color:#999;'>No data</span><br>")
      }
      # Normalized value row
      norm_row <- paste0(
        "<b>Normalized (0\u20131):</b> ",
        "<span style='font-size: 13px; font-weight: bold;'>",
        if (!is.na(norm_value)) round(norm_value, 3) else "<span style='color:#999;'>No data</span>",
        "</span>"
      )
      paste0(
        "<b style='color: #2d5a27; font-size: 14px;'>", indicator_name, "</b><br>",
        "<span style='font-size: 11px; color: #666;'>",
        component, " \u203a ", factor_name, "</span><br>",
        raw_row,
        norm_row
      )
    }
    
    # Vulnerability score summary
    score_section <- paste0(
      "<hr style='margin: 6px 0; border-color: #ddd;'>",
      "<table style='font-size: 11px; width: 100%;'>",
      "<tr>",
      "<td><b>Vulnerability</b></td>",
      "<td><b>Exposure</b></td>",
      "<td><b>Sensitivity</b></td>",
      "</tr><tr>",
      "<td style='color: #1a8a7d; font-weight: bold;'>", round(vuln, 3), "</td>",
      "<td style='color: #2a6f97; font-weight: bold;'>", round(exposure, 3), "</td>",
      "<td style='color: #c17817; font-weight: bold;'>", round(sensitivity, 3), "</td>",
      "</tr></table>"
    )
    
    # Hazard flags
    flags <- c(
      if (isTRUE(priority))     "<span style='background:#8B0000;color:white;padding:1px 5px;border-radius:3px;font-size:10px;'>HIGH PRIORITY</span>" else NULL,
      if (isTRUE(flag_fire))    "<span style='background:#fd8d3c;color:white;padding:1px 5px;border-radius:3px;font-size:10px;'>&#x1F525; Fire</span>"    else NULL,
      if (isTRUE(flag_flood))   "<span style='background:#2c7fb8;color:white;padding:1px 5px;border-radius:3px;font-size:10px;'>&#x1F4A7; Flood</span>"   else NULL,
      if (isTRUE(flag_slr))     "<span style='background:#253494;color:white;padding:1px 5px;border-radius:3px;font-size:10px;'>&#x1F30A; SLR</span>"     else NULL,
      if (isTRUE(flag_drought))  "<span style='background:#b8860b;color:white;padding:1px 5px;border-radius:3px;font-size:10px;'>&#x2600;&#xFE0F; Drought</span>" else NULL
    )
    flag_section <- if (length(flags) > 0) {
      paste0("<hr style='margin: 6px 0; border-color: #ddd;'>",
             paste(flags, collapse = " "))
    } else ""
    
    # Site details
    site_section <- paste0(
      "<hr style='margin: 6px 0; border-color: #ddd;'>",
      "<b>Water Supply ID:</b> ", wsd_source_id, "<br>",
      "<b>Park Unit:</b> ",       park_unit,      "<br>",
      "<b>Park Name:</b> ",       park_name,      "<br>",
      "<b>Water System:</b> ",    water_system_name, "<br>",
      "<b>State:</b> ",           state
    )
    
    paste0(
      "<div style='font-family: Arial; font-size: 12px; min-width: 210px;'>",
      top_section, score_section, flag_section, site_section,
      "</div>"
    )
  }
}

# Run the application
shinyApp(ui = ui, server = server)