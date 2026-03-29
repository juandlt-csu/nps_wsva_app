library(shiny)
library(leaflet)
library(leaflegend)
library(sf)
library(dplyr)
library(readr)
library(scales)
library(stringr)
library(purrr)
library(DT)
library(plotly)
library(later)
library(promises)
library(future)
plan(multisession)

############# DATA ############

# Simplify park boundaries at load time for faster rendering
parks <- st_read("app_data/park_boundaries_2025-08-14.gpkg") %>%
  st_transform(crs = 4326) %>%
  st_simplify(preserveTopology = TRUE, dTolerance = 0.01)

water_supplies <- read_csv("app_data/water_supplies.csv") %>%
  select(wsd_source_id, park_unit, park_name, region, state,
         water_system_name, source_longitude, source_latitude)

final_index <- read_csv("app_data/final_index.csv")

combined_data <- water_supplies %>%
  left_join(final_index, by = "wsd_source_id") %>%
  st_as_sf(coords = c("source_longitude", "source_latitude"), crs = 4326)

# Pre-compute filter options
all_regions <- sort(unique(na.omit(combined_data$region)))
all_states  <- sort(unique(na.omit(combined_data$state)))

# Normalized indicator columns used in vulnerability score calculation
exp_ind_cols <- c("exp_runoff_change", "exp_runoff_model_agree",
                  "exp_precip_change", "exp_precip_model_agree",
                  "exp_inundation_slr", "exp_fire_prob_change")
sen_ind_cols <- c("sen_visitation_trend", "sen_competition",
                  "sen_wildfire_hazard", "sen_flood_risk",
                  "sen_inundation_current", "sen_runoff_trend", "sen_precip_trend")

# Helper functions — mirror vulnerability_index_calculation.Rmd
minmax_norm <- function(x, na.rm = TRUE) {
  rng <- range(x, na.rm = na.rm)
  if (is.na(rng[1]) || rng[2] == rng[1]) return(rep(0, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}
euclid_agg <- function(df) sqrt(rowSums(df^2, na.rm = TRUE))
agg_norm   <- function(df) minmax_norm(euclid_agg(df))

# Recalculate vulnerability scores within a filtered subset, keeping the
# same Euclidean-distance framework used in the Rmd workflow.
recalc_vuln <- function(df) {
  df <- as.data.frame(df)
  
  exp_df  <- df[, exp_ind_cols, drop = FALSE]
  sen_df  <- df[, sen_ind_cols, drop = FALSE]
  
  exp_score <- agg_norm(exp_df)
  sen_score <- agg_norm(sen_df)
  vuln_raw  <- sqrt(exp_score^2 + sen_score^2)
  
  df$EXPOSURE       <- exp_score
  df$SENSITIVITY    <- sen_score
  df$VULNERABILITY  <- minmax_norm(vuln_raw)
  df
}

# ---------------------------------------------------------------------------
# Indicator config: Component -> Factor -> Indicator
# ---------------------------------------------------------------------------
indicator_config <- list(
  
  "Exposure" = list(
    "Runoff" = list(
      "Change in Runoff" = list(
        col = "exp_runoff_change", raw_col = "raw_runoff_change",
        raw_label = "% change mean annual runoff (p10)",
        description = "Projected % change in mean annual runoff (p10)"
      ),
      "Runoff Model Agreement" = list(
        col = "exp_runoff_model_agree", raw_col = "raw_runoff_model_agree",
        raw_label = "% GCMs predicting decrease",
        description = "% of GCMs predicting a decrease in runoff"
      )
    ),
    "Precipitation" = list(
      "Change in Precipitation" = list(
        col = "exp_precip_change", raw_col = "raw_precip_change",
        raw_label = "% change mean annual precip (p10)",
        description = "Projected % change in mean annual precipitation (p10)"
      ),
      "Precipitation Model Agreement" = list(
        col = "exp_precip_model_agree", raw_col = "raw_precip_model_agree",
        raw_label = "% models predicting decrease",
        description = "% of climate models predicting a precipitation decrease"
      )
    ),
    "Sea Level Rise" = list(
      "Inundation from Sea Level Rise" = list(
        col = "exp_inundation_slr", raw_col = "raw_inundation_slr",
        raw_label = "% point change in inundated area",
        description = "Projected % change in area inundated by sea level rise"
      )
    ),
    "Wildfire" = list(
      "Change in Fire Probability" = list(
        col = "exp_fire_prob_change", raw_col = "raw_fire_prob_change",
        raw_label = "% change fire probability (p90)",
        description = "Projected % change in probability of wildfire (p90)"
      )
    )
  ),
  
  "Sensitivity" = list(
    "Demand" = list(
      "Historic Visitation Trend" = list(
        col = "sen_visitation_trend", raw_col = "raw_visitation_trend",
        raw_label = "Scaled visitation trend",
        description = "Historic trend in park visitation (scaled)"
      ),
      "Competition (Water Use Trend)" = list(
        col = "sen_competition", raw_col = "raw_competition",
        raw_label = "Water use trend slope (per km\u00b2)",
        description = "Trend in nearby county water use (competition for supply)"
      )
    ),
    "Wildfire" = list(
      "Current Wildfire Risk" = list(
        col = "sen_wildfire_hazard", raw_col = "raw_wildfire_hazard",
        raw_label = "Mean Wildfire Hazard Potential",
        description = "Current Wildfire Hazard Potential index"
      )
    ),
    "Flood" = list(
      "Current Flood Risk" = list(
        col = "sen_flood_risk", raw_col = "raw_flood_risk",
        raw_label = "% area in FEMA flood zone",
        description = "% of surrounding area in a high-risk FEMA flood zone"
      )
    ),
    "Sea Level Rise" = list(
      "Current Inundation" = list(
        col = "sen_inundation_current", raw_col = "raw_inundation_current",
        raw_label = "% catchment area inundated (reference)",
        description = "% of catchment area currently inundated (reference condition)"
      )
    ),
    "Runoff" = list(
      "Historic Runoff Trend" = list(
        col = "sen_runoff_trend", raw_col = "raw_runoff_trend",
        raw_label = "Mann-Kendall slope (30-yr runoff)",
        description = "30-year historic trend in runoff (Mann-Kendall slope)"
      )
    ),
    "Precipitation" = list(
      "Historic Precipitation Trend" = list(
        col = "sen_precip_trend", raw_col = "raw_precip_trend",
        raw_label = "Mann-Kendall slope (40-yr precip)",
        description = "40-year historic trend in precipitation (Mann-Kendall slope)"
      )
    )
  )
)

score_views <- list(
  "Total Vulnerability Score"          = "VULNERABILITY",
  "Exposure Score"                     = "EXPOSURE",
  "Sensitivity Score"                  = "SENSITIVITY",
  "Factor: Runoff Exposure"            = "factor_exp_runoff",
  "Factor: Precipitation Exposure"     = "factor_exp_precip",
  "Factor: Sea Level Rise Exposure"    = "factor_exp_slr",
  "Factor: Wildfire Exposure"          = "factor_exp_wildfire",
  "Factor: Demand Sensitivity"         = "factor_sen_demand",
  "Factor: Wildfire Sensitivity"       = "factor_sen_wildfire",
  "Factor: Flood Sensitivity"          = "factor_sen_flood",
  "Factor: Sea Level Rise Sensitivity" = "factor_sen_slr",
  "Factor: Runoff Sensitivity"         = "factor_sen_runoff",
  "Factor: Precipitation Sensitivity"  = "factor_sen_precip"
)

# Factor labels for the score breakdown chart
factor_labels <- c(
  "factor_exp_runoff"    = "Runoff\n(Exp)",
  "factor_exp_precip"    = "Precip\n(Exp)",
  "factor_exp_slr"       = "SLR\n(Exp)",
  "factor_exp_wildfire"  = "Wildfire\n(Exp)",
  "factor_sen_demand"    = "Demand\n(Sen)",
  "factor_sen_wildfire"  = "Wildfire\n(Sen)",
  "factor_sen_flood"     = "Flood\n(Sen)",
  "factor_sen_slr"       = "SLR\n(Sen)",
  "factor_sen_runoff"    = "Runoff\n(Sen)",
  "factor_sen_precip"    = "Precip\n(Sen)"
)

###################### UI ###############################

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background-color: #f8f9fa; font-family: 'Arial','Helvetica',sans-serif; }

      .main-header {
        background: linear-gradient(135deg, #2d5a27 0%, #4a7c59 100%);
        color: white; padding: 20px 15px; margin-bottom: 15px;
        border-radius: 8px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);
      }
      .main-header h1 { margin:0; font-size:2.2rem; font-weight:300; text-align:center; }

      .control-panel {
        background: white; padding: 15px; border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1); margin-bottom: 15px;
        border-top: 4px solid #2d5a27;
      }
      .filter-panel {
        background: white; padding: 12px 15px; border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1); margin-bottom: 15px;
        border-top: 4px solid #4a7c59;
      }
      .form-group label { font-weight:600; color:#2d5a27; margin-bottom:8px; }
      .radio label, .checkbox label { font-weight:normal; color:#495057; }
      .btn-primary { background-color:#2d5a27; border-color:#2d5a27; }
      .btn-primary:hover { background-color:#1e3a1b; border-color:#1e3a1b; }
      .btn-default { border-color:#2d5a27; color:#2d5a27; }

      .leaflet-container { border-radius:8px; box-shadow:0 4px 6px rgba(0,0,0,0.1); }

      .info-box {
        background:#e8f5e8; border:1px solid #c3e6c3;
        border-radius:6px; padding:10px; margin-bottom:10px;
      }
      .info-box h4 { color:#2d5a27; margin-top:0; margin-bottom:5px; font-size:1.25rem; }
      .info-box p  { margin-bottom:5px; font-size:1.1rem; }

      .filter-badge {
        background:#2d5a27; color:white; border-radius:12px;
        padding:2px 10px; font-size:0.8rem; margin-left:8px;
      }

      .selectize-input { border:2px solid #e9ecef; border-radius:6px; }
      .selectize-input.focus {
        border-color:#2d5a27; box-shadow:0 0 0 0.2rem rgba(45,90,39,0.25);
      }
      .loading-overlay {
        position:absolute; top:50%; left:50%;
        transform:translate(-50%,-50%);
        background:white; padding:20px 30px; border-radius:8px;
        box-shadow:0 4px 12px rgba(0,0,0,0.3);
        z-index:1000; border-left:4px solid #2d5a27;
      }
      .loading-overlay h4 { margin:0; color:#2d5a27; font-weight:600; }

      .section-panel {
        background:white; border-radius:8px; padding:15px;
        box-shadow:0 2px 4px rgba(0,0,0,0.1); margin-top:15px;
      }
      .section-panel h4 { color:#2d5a27; font-weight:600; margin-top:0; }
    "))
  ),
  
  div(class = "main-header",
      h1("National Park Service", style = "margin-bottom:5px;"),
      h1("Water Supply Vulnerability Assessment Tool",
         style = "font-size:1.8rem; font-weight:400;")
  ),
  
  # ── Controls ──
  div(class = "control-panel",
      div(class = "info-box",
          h4(icon("info-circle"), " How to Use This Tool"),
          p(HTML("Opens on the <strong>Total Vulnerability Score</strong> (Exposure &amp;
            Sensitivity via Euclidean distance, rescaled 0&ndash;1; Michalak et al. 2021).
            Filter by region or state to recalculate scores within that group.
            Use <em>Score View</em> for composite/factor scores or <em>Specific Indicator</em>
            to drill down: <strong>Component &rarr; Factor &rarr; Indicator &rarr; Metric</strong>.
            Larger, darker circles = higher vulnerability."))
      ),
      
      fluidRow(
        column(3,
               h5("View Mode", style = "color:#2d5a27; margin-bottom:15px; font-weight:600;"),
               radioButtons("view_mode", label = NULL,
                            choices = list("Score View" = "score",
                                           "Specific Indicator" = "indicator"),
                            selected = "score")
        ),
        
        conditionalPanel(
          condition = "input.view_mode == 'score'",
          column(5,
                 selectInput("score_view",
                             label = tags$span("Score", style = "color:#2d5a27; font-weight:600;"),
                             choices = names(score_views),
                             selected = "Total Vulnerability Score")
          )
        ),
        
        conditionalPanel(
          condition = "input.view_mode == 'indicator'",
          column(2,
                 selectInput("component",
                             label = tags$span("Component", style = "color:#2d5a27; font-weight:600;"),
                             choices = names(indicator_config))
          ),
          column(2,
                 selectInput("factor",
                             label = tags$span("Factor", style = "color:#2d5a27; font-weight:600;"),
                             choices = NULL)
          ),
          column(3,
                 selectInput("indicator",
                             label = tags$span("Indicator", style = "color:#2d5a27; font-weight:600;"),
                             choices = NULL)
          ),
          column(2,
                 selectInput("metric",
                             label = tags$span("Metric", style = "color:#2d5a27; font-weight:600;"),
                             choices = c("Normalized (0\u20131)" = "norm", "Raw value" = "raw"))
          )
        )
      )
  ),
  
  # ── Filters ──
  div(class = "filter-panel",
      fluidRow(
        column(1,
               br(),
               actionButton("clear_filters", "Clear", class = "btn-default btn-sm",
                            style = "margin-top:4px;")
        ),
        column(3,
               selectInput("filter_region", label = tags$span("Region",
                                                              style = "color:#2d5a27; font-weight:600;"),
                           choices = c("All Regions" = "", all_regions),
                           selected = "")
        ),
        column(4,
               selectInput("filter_state", label = tags$span("State",
                                                             style = "color:#2d5a27; font-weight:600;"),
                           choices = c("All States" = "", all_states),
                           selected = "")
        ),
        column(4,
               br(),
               uiOutput("filter_info")
        )
      )
  ),
  
  # ── Map ──
  fluidRow(
    column(12,
           div(style = "position:relative;",
               uiOutput("map_title"),
               conditionalPanel(
                 condition = "output.parks_loading",
                 div(class = "loading-overlay",
                     h4(icon("spinner", class = "fa-spin"), " Loading park boundaries..."))
               ),
               leafletOutput("map", height = "calc(100vh - 360px)")
           )
    )
  ),
  
  # ── Score breakdown chart + Data table ──
  fluidRow(
    column(6,
           div(class = "section-panel",
               h4(icon("chart-bar"), " Score Breakdown"),
               p(style = "font-size:0.85rem; color:#666;",
                 "Mean factor scores for the currently filtered water supplies."),
               plotlyOutput("factor_chart", height = "300px")
           )
    ),
    column(6,
           div(class = "section-panel",
               h4(icon("table"), " Data Table"),
               p(style = "font-size:0.85rem; color:#666;",
                 "Click a row to highlight it on the map."),
               DTOutput("data_table")
           )
    )
  ),
  
  # ── Footer ──
  div(style = "margin-top:20px; padding:20px; background-color:#2d5a27;
               color:white; text-align:center;",
      p("Application developed by the Colorado State University Geospatial Centroid | Data current as of 2025",
        style = "margin:0; opacity:0.9;")
  )
)

############################ SERVER ##############################

server <- function(input, output, session) {
  
  parks_loading <- reactiveVal(FALSE)
  output$parks_loading <- reactive({ parks_loading() })
  outputOptions(output, "parks_loading", suspendWhenHidden = FALSE)
  
  # ── Cascade: Component -> Factor -> Indicator ───────────────────────────
  observe({
    req(input$component)
    factors <- names(indicator_config[[input$component]])
    updateSelectInput(session, "factor", choices = factors, selected = factors[1])
  })
  
  observe({
    req(input$component, input$factor)
    inds <- names(indicator_config[[input$component]][[input$factor]])
    updateSelectInput(session, "indicator", choices = inds, selected = inds[1])
  })
  
  # ── Clear filters ───────────────────────────────────────────────────────
  observeEvent(input$clear_filters, {
    updateSelectInput(session, "filter_region", selected = "")
    updateSelectInput(session, "filter_state",  selected = "")
  })
  
  # ── State filter cascades from region ──────────────────────────────────
  observe({
    if (input$filter_region != "") {
      states_in_region <- combined_data %>%
        as.data.frame() %>%
        filter(region == input$filter_region) %>%
        pull(state) %>% na.omit() %>% unique() %>% sort()
      updateSelectInput(session, "filter_state",
                        choices = c("All States" = "", states_in_region),
                        selected = "")
    } else {
      updateSelectInput(session, "filter_state",
                        choices = c("All States" = "", all_states),
                        selected = "")
    }
  })
  
  # ── Filtered + recalculated data ────────────────────────────────────────
  # When a geographic filter is active, vulnerability scores are recalculated
  # within that subset so rankings are relative to the filtered group.
  filtered_data <- reactive({
    df <- combined_data
    
    if (input$filter_region != "") df <- df %>% filter(region == input$filter_region)
    if (input$filter_state  != "") df <- df %>% filter(state  == input$filter_state)
    
    # Recalculate only when a filter is active
    if (input$filter_region != "" || input$filter_state != "") {
      df_recalc <- recalc_vuln(df)
      # Patch recalculated scores back into the sf object
      df$EXPOSURE      <- df_recalc$EXPOSURE
      df$SENSITIVITY   <- df_recalc$SENSITIVITY
      df$VULNERABILITY <- df_recalc$VULNERABILITY
    }
    
    df
  })
  
  # ── Filter info badge ───────────────────────────────────────────────────
  output$filter_info <- renderUI({
    n   <- nrow(filtered_data())
    tot <- nrow(combined_data)
    if (input$filter_region == "" && input$filter_state == "") {
      p(style = "color:#666; font-size:0.9rem; margin-top:8px;",
        paste0("Showing all ", tot, " water supplies"))
    } else {
      tagList(
        p(style = "color:#2d5a27; font-weight:600; font-size:0.9rem; margin-top:8px;",
          paste0("Showing ", n, " of ", tot, " water supplies"),
          tags$span(class = "filter-badge", "Scores recalculated within filter"))
      )
    }
  })
  
  # ── Active column ───────────────────────────────────────────────────────
  active_column <- reactive({
    if (input$view_mode == "score") {
      col <- score_views[[input$score_view]]
    } else {
      req(input$component, input$factor, input$indicator, input$metric)
      cfg <- indicator_config[[input$component]][[input$factor]][[input$indicator]]
      req(!is.null(cfg))
      col <- if (input$metric == "raw") cfg$raw_col else cfg$col
    }
    if (is.null(col) || nchar(col) == 0) return(NULL)
    if (!col %in% names(combined_data)) {
      showNotification(paste0("Column '", col, "' not found. Check final_index.csv."),
                       type = "warning", duration = 5)
      return(NULL)
    }
    col
  })
  
  # ── Map title ───────────────────────────────────────────────────────────
  output$map_title <- renderUI({
    if (input$view_mode == "score") {
      title_text <- input$score_view
      border_col <- if (grepl("Exposure",    title_text)) "#2a6f97" else
        if (grepl("Sensitivity", title_text)) "#c17817" else "#1a8a7d"
    } else {
      req(input$component, input$factor, input$indicator)
      cfg        <- indicator_config[[input$component]][[input$factor]][[input$indicator]]
      req(!is.null(cfg))
      title_text <- cfg$description
      border_col <- if (input$component == "Exposure") "#2a6f97" else "#c17817"
    }
    div(
      style = paste0("position:absolute; top:10px; left:60px;
                      background:white; padding:10px 15px;
                      border-radius:6px; box-shadow:0 2px 8px rgba(0,0,0,0.2);
                      border-left:4px solid ", border_col, "; z-index:1000; max-width:260px;"),
      h5(title_text, style = paste0("margin:0; color:", border_col,
                                    "; font-weight:600; font-size:1.05rem;"))
    )
  })
  
  # ── Base map (once) ─────────────────────────────────────────────────────
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -98.5, lat = 39.8, zoom = 4) %>%
      addMapPane("background", zIndex = 410) %>%
      addLayersControl(overlayGroups = c("Park Boundaries"),
                       options = layersControlOptions(collapsed = FALSE),
                       position = "topleft") %>%
      hideGroup("Park Boundaries")
  })
  
  # ── Park boundaries — simplified + rendered async ────────────────────────
  observeEvent(input$map_groups, {
    if ("Park Boundaries" %in% input$map_groups) {
      parks_loading(TRUE)
      # Yield to the event loop so the loading spinner renders first
      later::later(function() {
        leafletProxy("map", session = session) %>%
          addPolygons(
            data        = parks,
            group       = "Park Boundaries",
            fillColor   = "lightgreen",
            fillOpacity = 0.1,
            color       = "#2d5a27",
            weight      = 1,
            opacity     = 0.5,
            options     = pathOptions(pane = "background"),
            # No popup on boundaries keeps rendering faster
            label       = ~UNIT_NAME,
            labelOptions = labelOptions(textsize = "11px")
          )
        parks_loading(FALSE)
      }, delay = 0)
    } else {
      leafletProxy("map", session = session) %>% clearGroup("Park Boundaries")
    }
  }, ignoreNULL = FALSE)
  
  # ── Map markers ─────────────────────────────────────────────────────────
  observe({
    req(active_column())
    col       <- active_column()
    plot_data <- filtered_data()
    req(col %in% names(plot_data))
    
    is_raw  <- (input$view_mode == "indicator" && isTRUE(input$metric == "raw"))
    vals    <- as.data.frame(plot_data)[[col]]
    val_rng <- range(vals, na.rm = TRUE)
    pal_dom <- if (is_raw) val_rng else c(0, 1)
    
    pal <- colorNumeric(c("#ffffcc","#fed976","#fd8d3c","#f03b20","#bd0026"),
                        domain = pal_dom, na.color = "lightgrey")
    
    legend_title <- if (input$view_mode == "score") {
      str_wrap(input$score_view, 20)
    } else {
      paste0(str_wrap(input$indicator, 20), "\n",
             if (is_raw) "Raw value" else "Normalized (0\u20131)")
    }
    
    legend_vals <- if (is_raw) c(val_rng, NA) else c(0, 1, NA)
    
    # Pre-compute indicator config lookups
    is_ind <- input$view_mode == "indicator"
    if (is_ind) {
      req(input$component, input$factor, input$indicator)
      ind_cfg       <- indicator_config[[input$component]][[input$factor]][[input$indicator]]
      req(!is.null(ind_cfg))
      popup_metric  <- input$metric
      popup_score   <- NA_character_
      popup_comp    <- input$component
      popup_fac     <- input$factor
      popup_ind     <- input$indicator
      popup_rawlbl  <- ind_cfg$raw_label
      norm_col      <- ind_cfg$col
      raw_col_name  <- ind_cfg$raw_col
    } else {
      popup_metric  <- "norm"
      popup_score   <- input$score_view
      popup_comp    <- NA_character_
      popup_fac     <- NA_character_
      popup_ind     <- NA_character_
      popup_rawlbl  <- NA_character_
      norm_col      <- NULL
      raw_col_name  <- NULL
    }
    
    df        <- as.data.frame(plot_data)
    plot_vals <- df[[col]]
    norm_vals <- if (is_ind && !is.null(norm_col))    df[[norm_col]]    else rep(NA_real_, nrow(df))
    raw_vals  <- if (is_ind && !is.null(raw_col_name)) df[[raw_col_name]] else rep(NA_real_, nrow(df))
    
    radius_vec <- ifelse(is.na(plot_vals), 3,
                         pmax(4, pmin(16, scales::rescale(plot_vals, to=c(4,16), from=pal_dom))))
    fill_vec   <- pal(plot_vals)
    
    popup_vec <- unlist(Map(
      create_popup,
      norm_value = norm_vals, raw_value = raw_vals, col_value = plot_vals,
      vuln = df[["VULNERABILITY"]], exposure = df[["EXPOSURE"]], sensitivity = df[["SENSITIVITY"]],
      priority = df[["priority_national"]], flag_fire = df[["flag_fire"]],
      flag_flood = df[["flag_flood"]], flag_slr = df[["flag_slr"]], flag_drought = df[["flag_drought"]],
      wsd_source_id = df[["wsd_source_id"]], park_unit = df[["park_unit"]],
      park_name = df[["park_name"]], water_system_name = df[["water_system_name"]],
      state = df[["state"]],
      MoreArgs = list(view_mode = input$view_mode, metric = popup_metric,
                      score_label = popup_score, component = popup_comp,
                      factor_name = popup_fac, indicator_name = popup_ind,
                      raw_label = popup_rawlbl)
    ))
    
    leafletProxy("map") %>%
      clearMarkers() %>% clearControls() %>%
      addCircleMarkers(data = plot_data, radius = radius_vec, color = "#2c2c2c",
                       fillColor = fill_vec, fillOpacity = 0.9,
                       stroke = TRUE, weight = 1, popup = popup_vec) %>%
      addLegendNumeric(pal = pal, values = legend_vals, title = legend_title,
                       naLabel = "No Data", position = "bottomright")
  })
  
  # ── Factor score breakdown chart ────────────────────────────────────────
  output$factor_chart <- renderPlotly({
    df      <- as.data.frame(filtered_data())
    fac_cols <- names(factor_labels)
    fac_cols <- fac_cols[fac_cols %in% names(df)]
    
    means <- colMeans(df[, fac_cols, drop = FALSE], na.rm = TRUE)
    chart_df <- data.frame(
      factor = factor_labels[fac_cols],
      mean   = round(means, 3),
      component = ifelse(grepl("Exp", factor_labels[fac_cols]), "Exposure", "Sensitivity")
    )
    
    colors <- ifelse(chart_df$component == "Exposure", "#2a6f97", "#c17817")
    
    plot_ly(chart_df, x = ~factor, y = ~mean, type = "bar",
            marker = list(color = colors),
            hovertemplate = "%{x}<br>Mean score: %{y:.3f}<extra></extra>") %>%
      layout(
        xaxis = list(title = "", tickfont = list(size = 10)),
        yaxis = list(title = "Mean Score (0-1)", range = c(0, 1),
                     tickfont = list(size = 10)),
        margin = list(t = 10, b = 60),
        showlegend = FALSE,
        plot_bgcolor  = "white",
        paper_bgcolor = "white"
      )
  })
  
  # ── Data table ──────────────────────────────────────────────────────────
  output$data_table <- renderDT({
    df <- as.data.frame(filtered_data()) %>%
      select(wsd_source_id, park_unit, park_name, state, region,
             water_system_name,
             VULNERABILITY, EXPOSURE, SENSITIVITY,
             priority_national, flag_fire, flag_flood, flag_slr, flag_drought) %>%
      mutate(across(c(VULNERABILITY, EXPOSURE, SENSITIVITY), ~round(., 3)),
             across(c(priority_national, flag_fire, flag_flood, flag_slr, flag_drought),
                    ~ifelse(., "\u2713", "")))
    
    names(df) <- c("WSD ID", "Park Unit", "Park Name", "State", "Region",
                   "Water System", "Vulnerability", "Exposure", "Sensitivity",
                   "Priority", "Fire", "Flood", "SLR", "Drought")
    
    datatable(df,
              selection  = "single",
              rownames   = FALSE,
              extensions = "Buttons",
              options    = list(
                pageLength = 8,
                dom        = "Bfrtip",
                buttons    = list("csv", "excel"),
                scrollX    = TRUE,
                columnDefs = list(list(className = "dt-center",
                                       targets = 6:13))
              )) %>%
      formatStyle("Vulnerability",
                  background = styleColorBar(c(0,1), "#fd8d3c"),
                  backgroundSize = "100% 80%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") %>%
      formatStyle("Priority", color = "#8B0000", fontWeight = "bold") %>%
      formatStyle(c("Fire","Flood","SLR","Drought"),
                  color = "#2a6f97", fontWeight = "bold")
  })
  
  # ── Popup builder ───────────────────────────────────────────────────────
  create_popup <- function(view_mode, metric, score_label, component, factor_name,
                           indicator_name, norm_value, raw_value, raw_label, col_value,
                           vuln, exposure, sensitivity,
                           priority, flag_fire, flag_flood, flag_slr, flag_drought,
                           wsd_source_id, park_unit, park_name, water_system_name, state) {
    
    top_section <- if (view_mode == "score") {
      paste0("<b style='color:#1a8a7d; font-size:14px;'>", score_label, "</b><br>",
             "<b>Value (0\u20131):</b> <span style='font-size:14px; font-weight:bold;'>",
             round(col_value, 3), "</span>")
    } else {
      raw_row <- if (!is.na(raw_value)) {
        paste0("<b>", raw_label, ":</b> <span style='font-size:13px; font-weight:bold;'>",
               round(raw_value, 3), "</span><br>")
      } else {
        paste0("<b>", raw_label, ":</b> <span style='color:#999;'>No data</span><br>")
      }
      norm_row <- paste0(
        "<b>Normalized (0\u20131):</b> <span style='font-size:13px; font-weight:bold;'>",
        if (!is.na(norm_value)) round(norm_value, 3) else "<span style='color:#999;'>No data</span>",
        "</span>")
      paste0("<b style='color:#2d5a27; font-size:14px;'>", indicator_name, "</b><br>",
             "<span style='font-size:11px; color:#666;'>", component, " \u203a ", factor_name,
             "</span><br>", raw_row, norm_row)
    }
    
    score_section <- paste0(
      "<hr style='margin:6px 0; border-color:#ddd;'>",
      "<table style='font-size:11px; width:100%;'><tr>",
      "<td><b>Vulnerability</b></td><td><b>Exposure</b></td><td><b>Sensitivity</b></td>",
      "</tr><tr>",
      "<td style='color:#1a8a7d; font-weight:bold;'>", round(vuln, 3), "</td>",
      "<td style='color:#2a6f97; font-weight:bold;'>", round(exposure, 3), "</td>",
      "<td style='color:#c17817; font-weight:bold;'>", round(sensitivity, 3), "</td>",
      "</tr></table>")
    
    flags <- c(
      if (isTRUE(priority))    "<span style='background:#8B0000;color:white;padding:1px 5px;border-radius:3px;font-size:10px;'>HIGH PRIORITY</span>" else NULL,
      if (isTRUE(flag_fire))   "<span style='background:#fd8d3c;color:white;padding:1px 5px;border-radius:3px;font-size:10px;'>&#x1F525; Fire</span>"   else NULL,
      if (isTRUE(flag_flood))  "<span style='background:#2c7fb8;color:white;padding:1px 5px;border-radius:3px;font-size:10px;'>&#x1F4A7; Flood</span>"  else NULL,
      if (isTRUE(flag_slr))    "<span style='background:#253494;color:white;padding:1px 5px;border-radius:3px;font-size:10px;'>&#x1F30A; SLR</span>"    else NULL,
      if (isTRUE(flag_drought)) "<span style='background:#b8860b;color:white;padding:1px 5px;border-radius:3px;font-size:10px;'>&#x2600;&#xFE0F; Drought</span>" else NULL
    )
    flag_section <- if (length(flags) > 0)
      paste0("<hr style='margin:6px 0; border-color:#ddd;'>", paste(flags, collapse=" "))
    else ""
    
    site_section <- paste0(
      "<hr style='margin:6px 0; border-color:#ddd;'>",
      "<b>Water Supply ID:</b> ", wsd_source_id, "<br>",
      "<b>Park Unit:</b> ",       park_unit,      "<br>",
      "<b>Park Name:</b> ",       park_name,      "<br>",
      "<b>Water System:</b> ",    water_system_name, "<br>",
      "<b>State:</b> ",           state)
    
    paste0("<div style='font-family:Arial; font-size:12px; min-width:210px;'>",
           top_section, score_section, flag_section, site_section, "</div>")
  }
}

shinyApp(ui = ui, server = server)