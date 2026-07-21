# Libraries ----
library(shiny)
library(leaflet)
library(leaflegend)
library(htmlwidgets)
library(shinyWidgets)
library(shinyjs)
library(sf)
library(dplyr)
library(readr)
library(scales)
library(stringr)
library(purrr)
library(DT)
library(plotly)
library(later)
library(base64enc)
library(ggplot2)
# library(promises)
# library(future)
# plan(multisession)

# Data & Configuration ----

## Load Spatial & Tabular Data ----
# Source the vulnerability index calculation function
source("calc_vulnerability_index.R")

## Data Last Updated ----
DATA_LAST_UPDATED <- format(Sys.Date(), "%B %Y")


# Load and simplify park boundaries, keep only name column, write to GeoJSON
# for fast native Leaflet rendering
# parks_raw <- st_read("rapid_app/app_data/park_boundaries_2025-08-14.gpkg", quiet = TRUE) %>%
#   st_transform(crs = 4326) %>%
#   st_simplify(preserveTopology = TRUE, dTolerance = 2) %>%
#   select(UNIT_NAME)   # keep only the label column — much smaller object
# write_sf(parks_raw, "rapid_app/app_data/parks_simplified.gpkg")
park_boundaries <- st_read("app_data/parks_simplified.gpkg")

# read in municipal and hauled sources
municipal_hauled <- st_read('app_data/municipal_hauled.gpkg') %>% 
  select(park_name, park_unit, region, state, wsd_source_id, source_type)


water_supplies <- read_csv("app_data/water_supplies.csv") %>%
  select(wsd_source_id, park_unit, park_name, region, state,
         water_system_name, source_type, description,
         source_longitude, source_latitude)

# Raw indicators — vulnerability scores are calculated on the fly
final_indicators <- read_csv("app_data/final_indicators.csv")

# Join metadata + raw indicators, calculate national scores, convert to sf
combined_raw <- water_supplies %>%
  left_join(final_indicators, by = "wsd_source_id")

combined_data <- calc_vulnerability_index(combined_raw) %>%
  st_as_sf(coords = c("source_longitude", "source_latitude"), crs = 4326)

## Pre-Compute Filter Options ----
all_regions <- sort(unique(na.omit(combined_data$region)))
all_states  <- sort(unique(na.omit(combined_data$state)))
all_parks   <- sort(unique(na.omit(combined_data$park_unit)))

## Indicator Configuration: Component -> Factor -> Indicator ----
# col = normalized column name (for map display)
# raw_col = raw indicator column name (from final_indicators)
indicator_config <- list(
  
  "Exposure" = list(
    "Runoff" = list(
      "Change in Runoff" = list(
        col = "norm_exp_runoff_change", raw_col = "exp_runoff_change",
        raw_label = "% change mean annual runoff (10th percentile of all models)",
        description = "Projected % change in mean annual runoff (10th percentile of all models)"
      ),
      "Runoff Model Agreement" = list(
        col = "norm_exp_runoff_model_agree", raw_col = "exp_runoff_model_agree",
        raw_label = "% models predicting decrease",
        description = "% of climate models predicting a decrease in runoff"
      )
    ),
    "Precipitation" = list(
      "Change in Precipitation" = list(
        col = "norm_exp_precip_change", raw_col = "exp_precip_change",
        raw_label = "% change mean annual precip (10th percentile of all models)",
        description = "Projected % change in mean annual precipitation (10th percentile of all models)"
      ),
      "Precipitation Model Agreement" = list(
        col = "norm_exp_precip_model_agree", raw_col = "exp_precip_model_agree",
        raw_label = "% models predicting decrease",
        description = "% of climate models predicting a precipitation decrease"
      )
    ),
    "Drought" = list(
      "Change in Drought (SPEI)" = list(
        col = "norm_exp_drought_change", raw_col = "exp_drought_change",
        raw_label = "Change in drought (SPEI)",
        description = "Projected change in SPEI based on monthly precipitation and PET"
      ),
      "Drought Model Agreement" = list(
        col = "norm_exp_drought_model_agree", raw_col = "exp_drought_model_agree",
        raw_label = "% models predicting decrease",
        description = "% of climate models predicting a decrease in SPEI"
      )
    ),
    "Demand" = list(
      "Change in Competition" = list(
        col = "norm_exp_nearby_use_change", raw_col = "exp_nearby_use_change",
        raw_label = "% change in nearby water use per km\u00b2 (90th percentile of all models)",
        description = "Projected change in county water use (competition for supply) "
      ),
      "Competition Model Agreement" = list(
        col = "norm_exp_nearby_use_model_agree", raw_col = "exp_nearby_use_model_agree",
        raw_label = "% models predicting increase",
        description = "% of climate models predicting an increase in county water use"
      )
    ),
    "Sea Level Rise" = list(
      "Inundation from Sea Level Rise" = list(
        col = "norm_exp_inundation_slr", raw_col = "exp_inundation_slr",
        raw_label = "% point change in inundated area",
        description = "Projected % change in area inundated by sea level rise"
      ),
      "Saltwater Intrusion" = list(
        col = "norm_exp_swi", raw_col = "exp_swi",
        raw_label = "Change in saltwater intrusion (m)",
        description = "Projected saltwater migration in meters (Case-C)"
      ),
      "Storm Surge" = list(
        col = "norm_exp_storm_surge", raw_col = "exp_storm_surge",
        raw_label = "Change in storm surge (m)",
        description = "Absolute change in storm surge in meters"
      )
    ),
    "Wildfire" = list(
      "Change in Fire Probability" = list(
        col = "norm_exp_fire_prob_change", raw_col = "exp_fire_prob_change",
        raw_label = "% change fire probability (90th percentile of all models)",
        description = "Projected % change in probability of wildfire (90th percentile of all models)"
      ),
      "Fire Model Agreement" = list(
        col = "norm_exp_fire_model_agree", raw_col = "exp_fire_model_agree",
        raw_label = "% models predicting increase",
        description = "% of climate models predicting an increase in fire probability"
      )
    )
  ),
  
  "Sensitivity" = list(
    "Demand" = list(
      "Historical Visitation Trend" = list(
        col = "norm_sen_visitation_trend", raw_col = "sen_visitation_trend",
        raw_label = "Scaled visitation trend",
        description = "Historical trend in park visitation (scaled)"
      ),
      "Competition" = list(
        col = "norm_sen_competition", raw_col = "sen_competition",
        raw_label = "Water use trend slope (per km\u00b2)",
        description = "Trend in nearby county water use (competition for supply)"
      )
    ),
    # "Water Supply" = list(
    #   "Water Supply Type" = list(
    #     col = "norm_sen_water_supply_type", raw_col = "sen_water_supply_type",
    #     raw_label = "Source type vulnerability (ordinal)",
    #     description = "Water supply source type vulnerability rating"
    #   )
    # ),
    "Wildfire" = list(
      "Current Wildfire Risk" = list(
        col = "norm_sen_wildfire_hazard", raw_col = "sen_wildfire_hazard",
        raw_label = "Mean Wildfire Hazard Potential",
        description = "Current Wildfire Hazard Potential index"
      )
    ),
    "Flood" = list(
      "Current Flood Risk" = list(
        col = "norm_sen_flood_risk", raw_col = "sen_flood_risk",
        raw_label = "% area in FEMA flood zone",
        description = "% of surrounding area in a high-risk FEMA flood zone"
      )
    ),
    "Sea Level Rise" = list(
      "Current Inundation" = list(
        col = "norm_sen_inundation_current", raw_col = "sen_inundation_current",
        raw_label = "% catchment area inundated (reference)",
        description = "% of catchment area currently inundated (reference condition)"
      )
    ),
    "Runoff" = list(
      "Historical Runoff Trend" = list(
        col = "norm_sen_runoff_trend", raw_col = "sen_runoff_trend",
        raw_label = "Mann-Kendall slope (30-yr runoff)",
        description = "30-year historical trend in runoff (Mann-Kendall slope)"
      )
    ),
    "Precipitation" = list(
      "Historical Precipitation Trend" = list(
        col = "norm_sen_precip_trend", raw_col = "sen_precip_trend",
        raw_label = "Mann-Kendall slope (30-yr precip)",
        description = "30-year historical trend in precipitation (Mann-Kendall slope)"
      )
    ),
    "Drought" = list(
      "Historical Drought Trend" = list(
        col = "norm_sen_drought_trend", raw_col = "sen_drought_trend",
        raw_label = "Mann-Kendall slope (30-yr SPEI)",
        description = "30-year historical trend in SPEI (water deficit metric for drought)"
      )
    )
  )
)

## Score View, Rank & Factor Label Mappings ----
score_views <- list(
  "Relative Vulnerability Score" = "VULNERABILITY",
  "Relative Exposure Score"      = "EXPOSURE",
  "Relative Sensitivity Score"   = "SENSITIVITY"
)

# Mapping from score name to its rank column
score_rank_cols <- list(
  "VULNERABILITY" = "VULNERABILITY_rank",
  "EXPOSURE"      = "EXPOSURE_rank",
  "SENSITIVITY"   = "SENSITIVITY_rank"
)

# Factor labels for the score breakdown chart
factor_labels <- c(
  "factor_exp_runoff"       = "Runoff\n(Exp)",
  "factor_exp_precip"       = "Precip\n(Exp)",
  "factor_exp_drought"      = "Drought\n(Exp)",
  "factor_exp_slr"          = "Sea Level Rise\n(Exp)",
  "factor_exp_wildfire"     = "Wildfire\n(Exp)",
  "factor_exp_demand"       = "Demand\n(Exp)",
  "factor_sen_demand"       = "Demand\n(Sen)",
  #"factor_sen_water_supply" = "Supply\n(Sen)",
  "factor_sen_wildfire"     = "Wildfire\n(Sen)",
  "factor_sen_flood"        = "Flood\n(Sen)",
  "factor_sen_slr"          = "Sea Level Rise\n(Sen)",
  "factor_sen_runoff"       = "Runoff\n(Sen)",
  "factor_sen_precip"       = "Precip\n(Sen)",
  "factor_sen_drought"      = "Drought\n(Sen)"
)

# UI ----

# UI ----

ui <- navbarPage(
  title = HTML("<i class=\"fa fa-tint\" style=\"color:#A8DADC; font-size:1.3rem;\"></i>"),
  id = "main_tabs",
  collapsible = TRUE,
  
  header = tagList(
    tags$head(
      useShinyjs(),
      tags$script(HTML("document.documentElement.lang = \'en\';")),
      tags$style(HTML("
      body { background-color: #EDF4F7; font-family: 'Arial','Helvetica',sans-serif; }

      .skip-link {
        position:absolute; left:-9999px; top:0; z-index:2000;
        background:#1D3557; color:white; padding:10px 16px;
        border-radius:0 0 6px 0; font-weight:600;
      }
      .skip-link:focus {
        left:0;
      }

      .main-header {
  background: #0d1f35;
  color: white;
  padding: 0;
  margin-bottom: 15px;
  border-radius: 10px;
  box-shadow: 0 6px 24px rgba(13,31,53,0.4);
  overflow: hidden;
}
/* Top accent strip */
.main-header-accent {
  height: 5px;
  background: linear-gradient(90deg, #1D3557 0%, #2a7f7f 40%, #A8DADC 70%, #2a7f7f 100%);
}
.main-header-body {
  padding: 22px 20px 18px 20px;
}
.main-header h1 { margin:0; text-align:center; }

      .control-panel {
        background: white; padding: 15px 18px; border-radius: 10px;
        box-shadow: 0 2px 8px rgba(29,53,87,0.1); margin-bottom: 12px;
        border-top: 4px solid #1D3557;
      }
      .filter-panel {
        background: white; padding: 14px 18px; border-radius: 10px;
        box-shadow: 0 2px 8px rgba(29,53,87,0.1); margin-bottom: 12px;
        border-top: 4px solid #457B9D;
      }
      .form-group label { font-weight:600; color:#1D3557; margin-bottom:6px; }
      .radio label, .checkbox label { font-weight:normal; color:#495057; }
      .btn-primary { background-color:#1D3557; border-color:#1D3557; }
      .btn-primary:hover { background-color:#122440; border-color:#122440; }
      .btn-default { border-color:#1D3557; color:#1D3557; }
      
      #clear_filters {
      width: 100%; background-color: #2a7f7f; color: white;
      border: none; border-radius: 6px; font-weight: 600;
      font-size: 1.2rem; padding: 8px 10px; margin-top: 10px;
      }
      #clear_filters:hover { background-color: #1f5f5f; color: white; }

      .leaflet-container { border-radius:10px; box-shadow:0 4px 12px rgba(29,53,87,0.15); border:1px solid #c1d5e0; }

      .info-box {
        background: linear-gradient(135deg, #f0f5f8 0%, #e8f0f5 100%);
        border:1px solid #c1d5e0;
        border-radius:8px; padding:16px 20px; margin-bottom:14px;
        border-left: 4px solid #1D3557;
      }
      .info-box h4 { color:#1D3557; margin-top:0; margin-bottom:10px; font-size:1.3rem; }
      .info-box p  { margin-bottom:8px; font-size:1.15rem; line-height:1.6; color:#333; }
      .info-box summary::-webkit-details-marker { display:none; }
      .info-box[open] summary .fa-chevron-down { transform:rotate(180deg); }
      .info-box .info-section-title {
        font-weight:700; color:#1D3557; font-size:1.15rem; margin:0;
      }
      .info-box .badge-demo {
        display:inline-block; color:white; padding:2px 7px;
        border-radius:3px; font-size:0.75rem; margin-right:4px; vertical-align:middle;
      }
      
      #how_to_details[open] .how-to-chevron { transform: rotate(180deg); }
      #how_to_details[open] #how_to_hint { display: none; }
      #how_to_details summary::-webkit-details-marker { display: none; }
      #how_to_details summary::marker { display: none; }

      /* ── View mode toggle tabs ── */
      .view-toggle-wrap {
        display: flex; gap: 0; margin-bottom: 14px;
        border: 2px solid #1D3557; border-radius: 8px; overflow: hidden;
        width: fit-content;
      }
      .view-toggle-btn {
        padding: 7px 20px; font-size: 1.2rem; font-weight: 600;
        border: none; cursor: pointer; transition: all 0.18s ease;
        background: white; color: #1D3557; outline: none;
      }
      .view-toggle-btn:focus-visible {
        outline: 3px solid #457B9D !important; outline-offset: -3px;
      }
      .view-toggle-btn:first-child { border-right: 1px solid #1D3557; }
      .view-toggle-btn.active {
        background: #1D3557; color: white;
      }
      .view-toggle-btn:hover:not(.active) { background: #EDF4F7; }

      /* ── View mode option cards ── */
      .view-card {
        background: #f7fafc; border: 1px solid #dce8ef;
        border-radius: 8px; padding: 12px 16px;
      }
      .view-card-label {
        font-size: 1em; font-weight: 700; text-transform: uppercase;
        letter-spacing: 0.08em; color: #696969; margin-bottom: 10px;
      }
      .view-card.score-card  { border-left: 3px solid #386150; }
      .view-card.ind-card    { border-left: 3px solid #457B9D; }

      /* ── Filter panel refinements ── */
      .filter-section-label {
        font-size: 1em; font-weight: 700; text-transform: uppercase;
        letter-spacing: 0.09em; color: #457B9D; margin-bottom: 2px;
      }
      .filter-divider {
        width: 1px; background: #dce8ef; align-self: stretch; margin: 0 6px;
      }
      .filter-badge {
        background:#1D3557; color:white; border-radius:12px;
        padding:2px 10px; font-size:0.8rem; margin-left:8px;
      }
      #view_mode { display: none !important; }
      .selectize-dropdown { z-index:1100 !important; }
      .selectize-input { border:2px solid #e9ecef; border-radius:6px; }
      .selectize-input.focus {
        border-color:#1D3557; box-shadow:0 0 0 0.2rem rgba(29,53,87,0.2);
      }
      .loading-overlay {
        position:absolute; top:50%; left:50%;
        transform:translate(-50%,-50%);
        background:white; padding:20px 30px; border-radius:8px;
        box-shadow:0 4px 12px rgba(0,0,0,0.3);
        z-index:1000; border-left:4px solid #1D3557;
      }
      .loading-overlay h4 { margin:0; color:#1D3557; font-weight:600; }

      .section-panel {
        background:white; border-radius:10px; padding:18px;
        box-shadow:0 2px 8px rgba(29,53,87,0.1); margin-top:15px;
        border:1px solid #dce8ef;
      }
      .section-panel h2 { color:#1D3557; font-weight:600; font-size:1.3rem; margin-top:0; }
      
      /* ── DT selected row highlight ── */
      table.dataTable tbody tr.selected td,
      table.dataTable tbody tr.selected,
      table.dataTable tbody > tr.selected,
      table.dataTable tbody > tr > td.selected,
      table.dataTable tbody > tr.active td {
        background-color: #d0e8f2 !important;
        color: #1D3557 !important;
        box-shadow: none !important;
      }
      
      .dataTables_wrapper table.dataTable tbody tr.selected td {
        background-color: #d0e8f2 !important;
      }
      

      /* ---- Navbar overrides ---- */
      .navbar-default {
        background-color: #0d1f35 !important;
        border-color: #2a7f7f !important;
        border-bottom: 3px solid #2a7f7f !important;
      }
      .navbar-default .navbar-brand {
        color: #A8DADC !important; font-weight: 700; letter-spacing: 0.08em;
      }
      .navbar-default .navbar-brand:hover { color: white !important; }
      .navbar-default .navbar-nav > li > a {
        color: #A8DADC !important; font-weight: 600; font-size: 1.1rem;
      }
      .navbar-default .navbar-nav > li > a:hover {
        background-color: #1D3557 !important; color: white !important;
      }
      .navbar-default .navbar-nav > .active > a,
      .navbar-default .navbar-nav > .active > a:hover,
      .navbar-default .navbar-nav > .active > a:focus {
        background-color: #2a7f7f !important; color: white !important;
      }
      .navbar-default .navbar-toggle { border-color: #2a7f7f !important; }
      .navbar-default .navbar-toggle .icon-bar { background-color: #A8DADC !important; }
      .nav-tabs { border-bottom: 2px solid #dce8ef; }
      /* ---- Keep footer at page bottom in short-content tabs ---- */
      /* Avoid touching .container-fluid (navbarPage uses it inside the navbar too) */
      .tab-pane { min-height: calc(100vh - 120px); }
      /* ---- Expand map title overlay for description + controls ---- */
      .map-info-overlay {
        position: absolute; top: 10px; left: 60px; z-index: 1000;
        background: white; padding: 10px 14px; border-radius: 6px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.2); max-width: 310px;
      }
      .map-info-overlay .data-updated-badge {
        background: #e8f4f0; border-left: 3px solid #2a7f7f;
        border-radius: 3px; padding: 4px 8px; font-size: 0.95rem;
        color: #1a5c4a; margin: 6px 0;
      }
      .btn-export-map {
        background: #2a7f7f; color: white; border: none;
        border-radius: 4px; font-size: 0.95rem; width: 100%;
        padding: 5px 10px; font-weight: 600; cursor: pointer;
        margin-top: 4px;
      }
      .btn-export-map:hover { background: #1f5f5f; color: white; }
      /* ---- WBM Explorer tab ---- */
      .wbm-header {
        background: linear-gradient(135deg, #1D3557 0%, #2a7f7f 100%);
        color: white; padding: 18px 24px; border-radius: 10px;
        margin-bottom: 16px;
      }
      .wbm-header h2 { margin: 0 0 6px 0; font-size: 1.5rem; }
      .wbm-placeholder-card {
        background: #f5f9fb; border: 2px dashed #c1d5e0; border-radius: 10px;
        padding: 20px; text-align: center; color: #696969;
      }
      /* ---- Tech Docs tab ---- */
      .techdoc-card {
        background: white; border-radius: 10px; padding: 32px 40px;
        box-shadow: 0 2px 10px rgba(29,53,87,0.12);
        border-top: 4px solid #2a7f7f; max-width: 720px; margin: 0 auto;
      }
      .techdoc-pdf-btn {
        display: inline-block; background: #C05235; color: white;
        padding: 12px 28px; border-radius: 6px; font-size: 1.1rem;
        font-weight: 700; text-decoration: none; margin: 12px 0;
        border: none; cursor: not-allowed; opacity: 0.65;
      }
      "))
    )
  ),
  
  footer = div(
    style = "margin-top:20px; padding:20px; background-color:#1D3557;
             color:white; text-align:center;",
    p("Application developed by the Colorado State University Geospatial Centroid | Data current as of July 2026",
      style = "margin:0; opacity:0.9;")
  ),
  
  ## Tab 1: NPS WSVA Tool ----
  tabPanel("NPS WSVA Tool",
           value = "tab_main",
           tags$a(href = "#main-content", class = "skip-link", "Skip to main content"),
           ## Header ----
           div(class = "main-header",
               div(class = "main-header-accent"),
               div(class = "main-header-body",
                   div("NATIONAL PARK SERVICE",
                       style = "font-size:1rem; font-weight:700; letter-spacing:0.25em;
                    color:#A8DADC; margin-bottom:8px; text-align:center;"),
                   h1("Water Supply Vulnerability Assessment Tool",
                      style = "font-size:2rem; font-weight:800; letter-spacing:0.01em;
                    line-height:1.2; margin:0;"),
                   tags$div(style = "width:50px; height:3px; background:#2a7f7f;
                          margin:12px auto 10px auto; border-radius:2px;"),
                   em(HTML("&#128679; This app is under active development. All results are preliminary and should not be shared out at this time."),
                      style = "display:block; text-align:center; font-size:1.5rem; font-weight: 600;
                    color:rgba(255, 100, 0, 1);")
               )
           ),
           div(class = "control-panel", id = "main-content",
               
               ### How-To Info Box ----
               tags$details(class = "info-box", id = "how_to_details", style = "cursor:pointer;",
                            tags$summary(
                              style = paste0(
                                "font-size:1.3rem; font-weight:700; color:white; list-style:none;",
                                "background:#1D3557; border-radius:6px; padding:10px 16px;",
                                "display:flex; align-items:center; justify-content:space-between;",
                                "user-select:none;"
                              ),
                              tags$span(
                                icon("info-circle", style = "margin-right:8px;"),
                                h2(style = "display:inline; margin:0; font:inherit; color:inherit;", "How to Use This Tool"),
                                tags$span(
                                  id = "how_to_hint",
                                  style = "font-size:1.15rem; font-weight:400; color:#A8DADC; margin-left:10px; font-style:italic",
                                  "click to expand"
                                )
                              ),
                              tags$span(
                                class = "how-to-chevron",
                                style = "font-size:0.9rem; transition: transform 0.2s ease;",
                                icon("chevron-down")
                              )
                            ),
                            div(style = "margin-top:12px;",
                                p(HTML("This tool visualizes water supply vulnerability across the National Park System
          using a framework modeled after <a href='https://conbio.onlinelibrary.wiley.com/doi/10.1111/con4.70020' target='_blank' style='color:#457B9D;'>Michalak et al. 2026</a>. Each water supply is scored
          on two components &mdash; <strong style='color:#457B9D;'>Exposure</strong> (projected climate threats)
          and <strong style='color:#C05235;'>Sensitivity</strong> (current susceptibility) &mdash;
          which combine into an overall <strong style='color:#386150;'>Relative Vulnerability Score</strong>.
          Larger, darker circles indicate higher vulnerability.")),
                                div(style = "background:white; border-radius:8px; padding:14px 18px; margin:10px 0 6px 0; border:1px solid #dce8ef;",
                                    p(style = "font-size:1.2rem; color:#555; margin-bottom:10px;",
                                      HTML("Scores are computed in four steps using a Euclidean distance framework:")),
                                    div(style = "display:flex; align-items:center; justify-content:center; flex-wrap:wrap; gap:6px; margin-bottom:12px; text-align:center;",
                                        div(style = "background:#f5f8fa; border:1px solid #dce8ef; border-radius:6px; padding:8px 12px;",
                                            div(style = "font-size:1.2rem; color:#696969; margin-bottom:2px;", "Step 1"),
                                            HTML("<b>Indicators</b> (0&ndash;1)<br><span style='font-size:1.2rem; color:#555;'>normalized raw values</span>")),
                                        div(style = "font-size:1.4rem; color:#aaa;", HTML("&rarr;")),
                                        div(style = "background:#f5f8fa; border:1px solid #dce8ef; border-radius:6px; padding:8px 12px;",
                                            div(style = "font-size:1.2rem; color:#696969; margin-bottom:2px;", "Step 2"),
                                            HTML("<b>Factors</b><br><span style='font-size:1.2rem; color:#555;'>&radic;<span style='text-decoration:overline;'>&thinsp;&Sigma; indicator<sup>2</sup>&thinsp;</span></span><br>
                     <span style='font-size:1.2rem; color:#696969; font-style:italic;'>then normalized to 0&ndash;1</span>")),
                                        div(style = "font-size:1.4rem; color:#aaa;", HTML("&rarr;")),
                                        div(style = "background:#edf3f8; border:1px solid #c1d5e0; border-radius:6px; padding:8px 12px;",
                                            div(style = "font-size:1.2rem; color:#696969; margin-bottom:2px;", "Step 3"),
                                            HTML("<b style='color:#457B9D;'>Exposure</b> &amp; <b style='color:#C05235;'>Sensitivity</b><br>
                     <span style='font-size:1.2rem; color:#555;'>&radic;<span style='text-decoration:overline;'>&thinsp;&Sigma; factor<sup>2</sup>&thinsp;</span></span>")),
                                        div(style = "font-size:1.4rem; color:#aaa;", HTML("&rarr;")),
                                        div(style = "background:#e8f0e9; border:1px solid #b2ccb5; border-radius:6px; padding:8px 12px;",
                                            div(style = "font-size:1.2rem; color:#696969; margin-bottom:2px;", "Step 4"),
                                            HTML("<b style='color:#386150;'>Vulnerability</b><br>
                     <span style='font-size:1.25rem; color:#555;'>&radic;<span style='text-decoration:overline;'>&thinsp;Exp<sup>2</sup> + Sen<sup>2</sup>&thinsp;</span></span>"))
                                    ),
                                    tags$hr(style = "border-color:#dce8ef; margin:8px 0;"),
                                    p(style = "font-size:1.2rem; color:#555; margin-bottom:6px;",
                                      HTML("<b>Percentile ranks</b> and <b>High Priority</b> flags (shown in popups and the data table)
            indicate where a water supply falls relative to others in the <b>current view</b> &mdash;
            a rank of <b>90</b> means the supply scores higher than 90% of the comparison group.
            Both are recalculated whenever a region, state, or park filter is applied, so
            a supply flagged as High Priority within a filtered view may not hold that designation nationally.")),
                                    div(style = "background:#fff8e6; border-left:3px solid #8B6914; border-radius:4px; padding:8px 12px; margin-top:6px;",
                                        p(style = "font-size:1.4rem; color:#5a4a1a; margin:0;",
                                          HTML("<b><span aria-hidden='true'>&#x26A0;&#xFE0F;</span> Scores are relative, not absolute.</b>
                A score only has meaning within the group it is compared against.
                The same water supply may rank differently depending on whether it is evaluated
                across CONUS, a region, a state, or a subset of parks. Use filters intentionally to ensure comparisons are meaningful.")))
                                ),
                                h3(class = "info-section-title", icon("exclamation-triangle"), " Priority & Hazard Flags"),
                                p(HTML("Water supplies in the <strong>top 25% vulnerability score of the current comparison group</strong>
          are flagged as
          <span class='badge-demo' style='background:#9B2226;'>HIGH PRIORITY</span>.
          This threshold is recalculated relative to the active filter (CONUS, region, or state subset),
          so priority designations reflect the selected comparison group, not a fixed national threshold.
          Popups also display hazard-specific flags:
          <span class='badge-demo' style='background:#C05235;'><span aria-hidden='true'>&#x1F525;</span> Fire</span>
          <span class='badge-demo' style='background:#457B9D;'><span aria-hidden='true'>&#x1F4A7;</span> Flood</span>
          <span class='badge-demo' style='background:#1D3557;'><span aria-hidden='true'>&#x1F30A;</span> Sea Level Rise</span>
          <span class='badge-demo' style='background:#8B6914;'><span aria-hidden='true'>&#x2600;&#xFE0F;</span> Drought</span>"))
                            )
               ),
               
               ### View Mode Toggle & Options ----
               div(style = "margin-top:6px;",
                   
                   # Toggle buttons (JS-driven, sets hidden input)
                   tags$div(class = "view-toggle-wrap", role = "group", `aria-label` = "Map display mode",
                            tags$button(id = "btn_score", class = "view-toggle-btn active",
                                        onclick = "setViewMode('score')", `aria-pressed` = "true", "Relative Vulnerability Score"),
                            tags$button(id = "btn_indicator", class = "view-toggle-btn",
                                        onclick = "setViewMode('indicator')", `aria-pressed` = "false", "Raw Indicator Values")
                   ),
                   
                   # Hidden input read by server
                   textInput("view_mode", label = NULL, value = "score"),
                   tags$script(HTML("
        function setViewMode(mode) {
          document.getElementById('view_mode').value = mode;
          Shiny.setInputValue('view_mode', mode, {priority: 'event'});
          document.getElementById('btn_score').classList.toggle('active', mode === 'score');
          document.getElementById('btn_indicator').classList.toggle('active', mode === 'indicator');
          document.getElementById('btn_score').setAttribute('aria-pressed', mode === 'score');
          document.getElementById('btn_indicator').setAttribute('aria-pressed', mode === 'indicator');
        }
      ")),
                   
                   # Score View card
                   conditionalPanel(
                     condition = "input.view_mode == 'score'",
                     div(class = "view-card score-card", role = "group", `aria-labelledby` = "score-card-label",
                         div(id = "score-card-label", class = "view-card-label", HTML("<span aria-hidden='true'>&#x1F4CA;</span> Map a composite score")),
                         fluidRow(
                           column(5,
                                  selectInput("score_view",
                                              label = tags$span("Score", style = "color:#386150; font-weight:600;"),
                                              choices = names(score_views),
                                              selected = "Relative Vulnerability Score")
                           ),
                           column(4,
                                  selectInput("score_metric",
                                              label = tags$span("Display as", style = "color:#386150; font-weight:600;"),
                                              choices = c("Percentile Rank" = "rank", "Raw Score" = "raw"),
                                              selected = "rank")
                           )
                         )
                     )
                   ),
                   
                   # Specific Indicator card
                   conditionalPanel(
                     condition = "input.view_mode == 'indicator'",
                     div(class = "view-card ind-card", role = "group", `aria-labelledby` = "ind-card-label",
                         div(id = "ind-card-label", class = "view-card-label", HTML("<span aria-hidden='true'>&#x1F50D;</span> Drill into raw indicator values")),
                         fluidRow(
                           column(3,
                                  selectInput("component",
                                              label = tags$span("Component", style = "color:#457B9D; font-weight:600;"),
                                              choices = names(indicator_config))
                           ),
                           column(3,
                                  selectInput("factor",
                                              label = tags$span("Factor", style = "color:#457B9D; font-weight:600;"),
                                              choices = NULL)
                           ),
                           column(3,
                                  selectInput("indicator",
                                              label = tags$span("Indicator", style = "color:#457B9D; font-weight:600;"),
                                              choices = NULL)
                           ),
                           column(3,
                                  div(style = "margin-top:26px;",
                                      uiOutput("indicator_description"))
                           )
                         )
                     )
                   )
               )
           ),
           ## Filters ----
           div(class = "filter-panel",
               fluidRow(
                 # Geographic filters group
                 column(6,
                        div(role = "group", `aria-labelledby` = "geo-filter-label",
                            div(id = "geo-filter-label", class = "filter-section-label", HTML("<span aria-hidden='true'>&#x1F4CD;</span> Geographic Filter"),
                                tags$span(style = "color:#aaa; font-weight:400; margin-left:6px; font-size:1.2rem;",
                                          "(filtering recalculates scores)")),
                            fluidRow(
                              column(4,
                                     selectizeInput("filter_region",
                                                    label = tags$span("Region", style = "color:#1D3557; font-weight:600; font-size:1.2rem;"),
                                                    choices = c("All Regions" = "", all_regions),
                                                    selected = NULL, multiple = TRUE,
                                                    options = list(placeholder = "Search regions...",
                                                                   plugins = list("remove_button")))
                              ),
                              column(4,
                                     selectizeInput("filter_state",
                                                    label = tags$span("State", style = "color:#1D3557; font-weight:600; font-size:1.2rem;"),
                                                    choices = c("All States" = "", all_states),
                                                    selected = NULL, multiple = TRUE,
                                                    options = list(placeholder = "Search states...",
                                                                   plugins = list("remove_button")))
                              ),
                              column(4,
                                     selectizeInput("filter_park",
                                                    label = tags$span("Park Unit", style = "color:#1D3557; font-weight:600; font-size:1.2rem;"),
                                                    choices = c("All Parks" = "", all_parks),
                                                    selected = NULL, multiple = TRUE,
                                                    options = list(placeholder = "Search parks...",
                                                                   plugins = list("remove_button")))
                              )
                            )
                        )
                 ),
                 
                 # Divider
                 # column(1, div(style = "border-left:1px solid #dce8ef; height:70px; margin:2px auto 0;")),
                 
                 # Priority toggle + Source Type filter
                 column(4,
                        fluidRow(
                          column(6,
                                 div(class = "filter-section-label", HTML("<span aria-hidden='true'>&#x26A0;&#xFE0F;</span> Priority Filter")),
                                 br(),
                                 div(style = "margin-top:6px;",
                                     materialSwitch(
                                       inputId = "filter_priority",
                                       label   = tags$span("Top Priority Only",
                                                           style = "color:#1D3557; font-weight:600; font-size:1.15rem;"),
                                       value   = FALSE,
                                       status  = "danger"
                                     )
                                 )
                          ),
                          column(6,
                                 div(class = "filter-section-label", HTML("<span aria-hidden='true'>&#x1F4A7;</span> Filter by Source Type")),
                                 selectizeInput("filter_source_type",
                                                label = tags$span("Source Type", style = "color:#1D3557; font-weight:600; font-size:1.2rem;"),
                                                choices = NULL,
                                                selected = NULL,
                                                multiple = TRUE,
                                                options = list(placeholder = "All source types...",
                                                               plugins = list("remove_button")))
                          )
                        )
                 ),
                 
                 # Status badge + clear
                 column(2,
                        div(class = "filter-section-label", HTML("<span aria-hidden='true'>&#x2139;&#xFE0F;</span> Current View")),
                        div(style = "margin-top:6px;",
                            uiOutput("filter_info"),
                            actionButton("clear_filters", "Clear All Filters",
                                         class = "btn-sm")
                        )
                 )
               )
           ),
           ## Map ----
           fluidRow(
             column(12,
                    h2(class = "sr-only", "Interactive Vulnerability Map"),
                    p(class = "sr-only",
                      "This interactive map is not fully accessible by screen reader. Every water supply shown on the map is also listed in the sortable, exportable Data Table further down this page."),
                    div(style = "position:relative; margin: 0 30px;",
                        uiOutput("map_title"),
                        # conditionalPanel(
                        #   condition = "output.parks_loading",
                        #   div(class = "loading-overlay",
                        #       h4(icon("spinner", class = "fa-spin"), " Loading park boundaries..."))
                        # ),
                        leafletOutput("map", height = "calc(100vh - 180px)"),
                        conditionalPanel(
                          condition = "input.view_mode == 'indicator'",
                          div(
                            style = "position:absolute; bottom:20px; left:10px; z-index:1000;
                            background:white; padding:10px 12px 6px 12px; border-radius:8px;
                            box-shadow:0 2px 8px rgba(0,0,0,0.2); width:300px;",
                            h3(style = "font-size:1.05rem; font-weight:600; color:#1D3557; margin:0 0 2px 0;",
                               "Indicator Distribution"),
                            plotlyOutput("indicator_distribution_chart", height = "220px", width = "100%"),
                            uiOutput("indicator_distribution_srsummary"),
                            uiOutput("indicator_distribution_caption")
                          )
                        )
                    )
             )
           ),
           ## Data Table ----
           fluidRow(
             column(12,
                    div(class = "section-panel",
                        h2(icon("table"), " Data Table"),
                        p(style = "font-size:1.2rem; color:#666;",
                          "Click a row to highlight it on the map."),
                        DTOutput("data_table")
                    )
             )
           ),
  ),
  
  ## Tab 2: Water Balance Model Explorer ----
  tabPanel("Water Balance Model Explorer",
           value = "tab_wbm",
           div(class = "wbm-header",
               h2(icon("cloud-rain", style = "margin-right:10px;"),
                  "NPS Water Balance Model Explorer"),
               p(HTML("<strong>Prototype:</strong> This tab will display historic and future projections of the NPS Water Balance Model components.
           Select a variable and time period to explore spatial patterns across the National Park System."),
                 style = "margin:0; font-size:1.1rem; opacity:0.9;")
           ),
           
           fluidRow(
             column(3,
                    div(class = "control-panel",
                        div(class = "view-card-label",
                            HTML("<span aria-hidden=\'true\'>&#x1F4C6;</span> Model Controls")),
                        br(),
                        div(class = "wbm-placeholder-card",
                            icon("clock", style = "font-size:2rem; color:#c1d5e0; margin-bottom:8px; display:block;"),
                            p(HTML("<strong>Controls coming soon</strong><br>
                       Future controls will allow selection of:"), style = "margin:0 0 8px 0;"),
                            tags$ul(style = "text-align:left; font-size:1.1rem; color:#555;",
                                    tags$li("Variable: Precipitation, Temperature, Snow Water Equivalent, ET, Runoff"),
                                    tags$li("Time Period: Historic (1950-2020) or Future Projections (2021-2099)"),
                                    tags$li("Scenario: SSP2-4.5, SSP5-8.5"),
                                    tags$li("Season / Annual summary")
                            )
                        )
                    )
             ),
             column(9,
                    div(style = "position:relative;",
                        div(style = "background:#e8f0f5; border:2px dashed #c1d5e0; border-radius:10px;
                         height:calc(80vh - 200px); display:flex; align-items:center;
                         justify-content:center; flex-direction:column;",
                            icon("map", style = "font-size:3.5rem; color:#c1d5e0; margin-bottom:12px;"),
                            h3("Water Balance Map", style = "color:#c1d5e0; margin:0 0 6px 0;"),
                            p(HTML("Spatial water balance model outputs will display here.<br>
                       Data layers are in preparation."),
                              style = "color:#aaa; text-align:center; font-size:1.2rem; margin:0;")
                        )
                    )
             )
           )
  ),
  
  ## Tab 3: Technical Documentation ----
  tabPanel("Technical Documentation",
           value = "tab_docs",
           br(),
           div(class = "techdoc-card",
               div(style = "display:flex; align-items:center; gap:14px; margin-bottom:6px;",
                   icon("file-alt", style = "font-size:2rem; color:#2a7f7f;"),
                   h2(style = "margin:0; color:#1D3557; font-size:1.6rem;",
                      "Technical Documentation")
               ),
               tags$hr(style = "border-color:#dce8ef; margin:12px 0 16px 0;"),
               
               p(HTML("The <strong>NPS Water Supply Vulnerability Assessment Technical Report</strong> describes the full
           methodology, data sources, and validation behind the vulnerability index scores displayed in this tool.
           The report follows the <a href=\'https://conbio.onlinelibrary.wiley.com/doi/10.1111/con4.70020\'
           target=\'_blank\' style=\'color:#457B9D;\'>Michalak et al. (2026)</a> hierarchical Euclidean
           distance framework."),
                 style = "font-size:1.2rem; line-height:1.7; color:#333;"),
               
               div(style = "margin:20px 0;",
                   h3(style = "color:#1D3557; font-size:1.2rem; margin-bottom:6px;",
                      icon("list-ul", style = "margin-right:6px;"), "Report Contents"),
                   tags$ul(style = "font-size:1.15rem; color:#555; line-height:1.8;",
                           tags$li("Conceptual framework and indicator hierarchy"),
                           tags$li("Raw data sources and processing steps for all 19 indicators"),
                           tags$li("Euclidean distance normalization and aggregation methodology"),
                           tags$li("Source-type conditional zeroing (rainwater, ocean sources)"),
                           tags$li("Priority classification and hazard flag thresholds"),
                           tags$li("Validation against field observations and expert review"),
                           tags$li("Limitations and recommended uses of the vulnerability scores")
                   )
               ),
               
               div(style = "background:#f0f5f8; border-radius:8px; padding:20px 24px; margin-top:20px;",
                   h3(style = "color:#1D3557; margin:0 0 8px 0; font-size:1.2rem;",
                      icon("download", style = "margin-right:6px; color:#2a7f7f;"),
                      "Download Technical Report"),
                   p(HTML("<strong>PDF report link coming soon.</strong> The full technical documentation is currently in final review.
               It will be linked here upon completion."),
                     style = "font-size:1.15rem; color:#555; margin:0 0 12px 0;"),
                   tags$button(
                     class = "techdoc-pdf-btn",
                     disabled = NA,
                     HTML("<span aria-hidden=\'true\'>&#x1F4C4;</span> Technical Report PDF (Link Pending)")
                   ),
                   p(style = "font-size:1.05rem; color:#888; margin:10px 0 0 0;",
                     "For questions about the methodology, contact the Colorado State University Geospatial Centroid.")
               )
           ),
           br()
  )
)

# Server ----

server <- function(input, output, session) {
  
  parks_loading <- reactiveVal(FALSE)
  output$parks_loading <- reactive({ parks_loading() })
  outputOptions(output, "parks_loading", suspendWhenHidden = FALSE)
  
  ## Cascade: Component -> Factor -> Indicator ----
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
  
  ## Indicator Description Blurb ----
  ## Indicator Description ----
  output$indicator_description <- renderUI({
    req(input$view_mode == "indicator", input$component, input$factor, input$indicator)
    cfg <- indicator_config[[input$component]][[input$factor]][[input$indicator]]
    req(!is.null(cfg))
    border_col <- if (input$component == "Exposure") "#457B9D" else "#C05235"
    div(
      style = paste0("background:#edf3f8; border-left:3px solid ", border_col, ";
                      border-radius:4px; padding:8px 10px; font-size:1.1rem;
                      color:#2c4a6e; line-height:1.5;"),
      tags$strong(
        style = paste0("color:", border_col, "; font-size:1.05rem;"),
        paste0(input$component, " Indicator Description:")
      ),
      tags$br(),
      span(cfg$description, style = "color:#333; font-size:1.1rem;"),
      tags$hr(style = "margin:6px 0; border-color:#c1d5e0;"),
      div(
        style = "background:#e8f4f0; border-left:2px solid #2a7f7f; border-radius:3px;
                 padding:3px 8px; font-size:1rem; color:#1a5c4a;",
        icon("calendar-alt", style = "margin-right:4px; font-size:0.95rem;"),
        paste0("Data Last Updated: ", DATA_LAST_UPDATED)
      )
    )
  })
  
  
  ## Clear Filters ----
  observeEvent(input$clear_filters, {
    updateSelectizeInput(session, "filter_region", selected = character(0))
    updateSelectizeInput(session, "filter_state",  selected = character(0))
    updateSelectizeInput(session, "filter_park", selected = character(0))
    updateSelectizeInput(session, "filter_source_type", selected = character(0))
    clicked_site(NULL)
  })
  
  ## Description Pop-Out Modal ----
  observeEvent(input$show_description_btn, {
    site_id <- input$show_description_btn
    df_all  <- as.data.frame(filtered_data())
    req(site_id %in% df_all$wsd_source_id)
    desc_text <- df_all$description[df_all$wsd_source_id == site_id][1]
    
    showModal(modalDialog(
      title = paste0("System Description — ", site_id),
      p(desc_text, style = "font-size:1.5rem; line-height:1.6; color:#333;"),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  ## State & Park Filters Cascade ----
  observe({
    df <- as.data.frame(combined_data)
    if (length(input$filter_region) > 0) {
      df <- df %>% filter(region %in% input$filter_region)
    }
    
    states_avail <- sort(unique(na.omit(df$state)))
    current_sel  <- intersect(input$filter_state, states_avail)
    updateSelectizeInput(session, "filter_state",
                         choices = c("All States" = "", states_avail),
                         selected = current_sel)
  })
  
  observe({
    df <- as.data.frame(combined_data)
    if (length(input$filter_region) > 0) {
      df <- df %>% filter(region %in% input$filter_region)
    }
    if (length(input$filter_state) > 0) {
      df <- df %>% filter(state %in% input$filter_state)
    }
    
    parks_avail <- sort(unique(na.omit(df$park_unit)))
    current_sel <- intersect(input$filter_park, parks_avail)
    
    # If any selected parks are no longer valid, clear them immediately
    if (!setequal(current_sel, input$filter_park)) {
      updateSelectizeInput(session, "filter_park",
                           choices = c("All Parks" = "", parks_avail),
                           selected = character(0))
    } else {
      updateSelectizeInput(session, "filter_park",
                           choices = c("All Parks" = "", parks_avail),
                           selected = current_sel)
    }
  })
  
  ## Filtered & Recalculated Data ----
  # When a geographic filter is active, vulnerability scores are recalculated
  # within that subset so rankings are relative to the filtered group.
  geo_recalculated_data <- reactive({
    df <- combined_data
    if (length(input$filter_region) > 0) df <- df %>% filter(region %in% input$filter_region)
    if (length(input$filter_state)  > 0) df <- df %>% filter(state  %in% input$filter_state)
    
    has_geo_filter <- length(input$filter_region) > 0 || length(input$filter_state) > 0
    if (has_geo_filter) {
      geom <- st_geometry(df)
      df_recalc <- tryCatch(
        calc_vulnerability_index(as.data.frame(df)),
        error = function(e) {
          showNotification(
            "Score recalculation failed for this selection due to low sample size — showing national scores instead.",
            type = "error", duration = NULL
          )
          as.data.frame(df)
        }
      )
      df <- st_sf(df_recalc, geometry = geom)
    }
    df
  })
  
  filtered_data <- reactive({
    df <- geo_recalculated_data()
    if (length(input$filter_park) > 0) df <- df %>% filter(park_unit %in% input$filter_park)
    if (isTRUE(input$filter_priority)) df <- df %>% filter(priority_group == TRUE)
    df
  })
  
  ## Populate Source Type Choices ----
  observe({
    df <- as.data.frame(filtered_data())
    types_avail <- sort(unique(na.omit(df$source_type)))
    current_sel <- intersect(input$filter_source_type, types_avail)
    updateSelectizeInput(session, "filter_source_type",
                         choices  = types_avail,
                         selected = current_sel)
  })
  
  ## Display Data (Source Type Filter) ----
  filtered_data_display <- reactive({
    df <- filtered_data()
    if (length(input$filter_source_type) > 0) {
      df <- df %>% filter(source_type %in% input$filter_source_type)
    }
    df
  })
  
  # Debounce so panning doesn't re-render the table on every pixel of drag
  map_bounds_debounced <- reactive({ input$map_bounds }) %>% debounce(400)
  
  ## Zoom to Filtered Extent ----
  observeEvent(filtered_data(), {
    df <- filtered_data()
    req(nrow(df) > 0)
    
    bbox <- st_bbox(df)
    
    leafletProxy("map") %>%
      fitBounds(
        lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
        lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]],
        options = list(maxZoom = 10)
      )
  }, ignoreInit = TRUE)
  
  ## Filter Info Badge ----
  output$filter_info <- renderUI({
    n   <- nrow(filtered_data())
    tot <- nrow(combined_data)
    has_geo_filter  <- length(input$filter_region) > 0 || length(input$filter_state) > 0
    has_park_filter <- length(input$filter_park) > 0
    
    region_label <- paste(input$filter_region, collapse = ", ")
    state_label  <- paste(input$filter_state,  collapse = ", ")
    
    context_label <- if (length(input$filter_region) > 0 && length(input$filter_state) > 0) {
      paste0(state_label, " (", region_label, ")")
    } else if (length(input$filter_state) > 0) {
      state_label
    } else if (length(input$filter_region) > 0) {
      region_label
    } else {
      "National"
    }
    
    if (!has_geo_filter && !has_park_filter) {
      tagList(
        p(style = "color:#666; font-size:1.5rem; margin-top:8px;",
          paste0("Showing all ", tot, " water supplies")),
        tags$span(class = "filter-badge", style = "font-size:1.2rem; background:#386150;",
                  "\U0001f30e Scores: National")
      )
    } else {
      tagList(
        p(style = "color:#1D3557; font-weight:600; font-size:1.2rem; margin-top:8px;",
          paste0("Showing ", n, " of ", tot, " water supplies")),
        tags$span(class = "filter-badge",
                  style = paste0("font-size:1.5rem; background:",
                                 if (has_geo_filter) "#386150;" else "#457B9D;"),
                  if (has_geo_filter)
                    paste0("\U0001f4ca Scores: ", context_label)
                  else
                    "\U0001f4ca Scores: National (park filter only)")
      )
    }
  })
  
  ## Active Column ----
  active_column <- reactive({
    req(!is.null(input$view_mode) && nchar(input$view_mode) > 0)
    if (input$view_mode == "score") {
      base_col <- score_views[[input$score_view]]
      if (isTRUE(input$score_metric == "rank")) {
        score_rank_cols[[base_col]]
      } else {
        base_col
      }
    } else {
      req(input$component, input$factor, input$indicator)
      cfg <- indicator_config[[input$component]][[input$factor]][[input$indicator]]
      req(!is.null(cfg))
      cfg$raw_col
    }
  })
  
  ## Map Title ----
  ## Map Title + Info Overlay ----
  output$map_title <- renderUI({
    req(!is.null(input$view_mode) && nchar(input$view_mode) > 0)
    if (input$view_mode == "score") {
      title_label <- input$score_view
      desc_text   <- NULL
      border_col  <- if (grepl("Exposure",    title_label)) "#5B8C6E" else
        if (grepl("Sensitivity", title_label)) "#7C6FAD" else "#C05235"
    } else {
      req(input$component, input$factor, input$indicator)
      cfg        <- indicator_config[[input$component]][[input$factor]][[input$indicator]]
      req(!is.null(cfg))
      title_label <- paste0(input$component, " Indicator Description:")
      desc_text   <- cfg$description
      border_col  <- if (input$component == "Exposure") "#5B8C6E" else "#7C6FAD"
    }
    div(
      class = "map-info-overlay",
      style = paste0("border-left:4px solid ", border_col, ";"),
      h3(title_label, style = paste0("margin:0 0 4px 0; color:", border_col,
                                     "; font-weight:700; font-size:1.1rem;")),
      if (!is.null(desc_text))
        p(desc_text, style = "margin:0 0 6px 0; font-size:1.05rem; color:#333; line-height:1.4;"),
      div(class = "data-updated-badge",
          icon("calendar-alt", style = "margin-right:4px; font-size:0.9rem;"),
          paste0("Data Last Updated: ", DATA_LAST_UPDATED)),
      actionButton("export_map_btn",
                   label = tagList(icon("download"), " Export Current Map"),
                   class = "btn-export-map btn-sm")
    )
  })
  
  
  # Icon for municipal/hauled supplies 
  tri_icon <- leaflegend::makeSymbol(
    shape = "triangle",
    width = 12,
    color = "#1D3557",
    fillColor = "black",
    fillOpacity = 0.9,
    opacity = 1
  )
  
  ## Base Map (Rendered Once) ----
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo") %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
      setView(lng = -98.5, lat = 37, zoom = 4) %>%
      addMapPane("background", zIndex = 410) %>%
      addMapPane("markers",    zIndex = 450) %>%
      addMapPane("highlights", zIndex = 420) %>% 
      addMapPane("m_h", zIndex = 420) %>% 
      addPolygons(
        data        = park_boundaries,
        group       = "Park Boundaries",
        fillColor   = "#A8DADC",
        fillOpacity = 0.1,
        color       = "#1D3557",
        weight      = 1,
        opacity     = 0.5,
        options     = pathOptions(pane = "background"),
        label       = ~UNIT_NAME,
        labelOptions = labelOptions(
          textsize  = "11px",
          direction = "auto",
          style     = list("font-weight" = "normal", "border" = "none",
                           "box-shadow" = "none", "background" = "transparent")
        )
      ) %>%
      addMarkers(
        data = municipal_hauled,
        group = "Municipal/Hauled Supplies",
        icon = leaflet::icons(iconUrl = tri_icon, iconWidth = 12, iconHeight = 12),
        # radius = 5,
        # fillColor = "black",
        # fillOpacity = 0.75,
        # stroke = FALSE,
        popup = ~paste0(
          "<b>Park Name:</b> ", park_name, "<br/>",
          "<b>Park Unit:</b> ", park_unit, "<br/>",
          "<b>Regiont:</b> ", region, "<br/>",
          "<b>State:</b> ", state, "<br/>",
          "<b>Source Type:</b> ", source_type
        ),
        options     = pathOptions(pane = "m_h")
      ) %>% 
      addLayersControl(baseGroups = c("OpenStreetMap", "Satellite", "Terrain"),
                       overlayGroups = "Municipal/Hauled Supplies") %>% 
      hideGroup("Municipal/Hauled Supplies")
  })
  
  ## Map Markers ----
  observe({
    req(active_column())
    col       <- active_column()
    plot_data <- filtered_data_display()
    req(col %in% names(plot_data))
    
    vals    <- as.numeric(as.data.frame(plot_data)[[col]])
    vals    <- vals[is.finite(vals)]
    req(length(vals) > 0)
    val_rng <- range(vals, na.rm = TRUE)
    # Prevent zero-width domain which breaks colorNumeric
    if (val_rng[1] == val_rng[2]) val_rng <- c(val_rng[1] - 0.001, val_rng[2] + 0.001)
    
    # Determine palette domain
    is_rank <- (input$view_mode == "score" && isTRUE(input$score_metric == "rank"))
    pal_dom <- if (is_rank) c(0, 100) else val_rng
    
    # Columns where lower raw values = higher vulnerability (invert color scale)
    invert_raw_cols <- c("exp_runoff_change", "exp_precip_change", "exp_drought_change",
                         "sen_runoff_trend", "sen_precip_trend")
    is_inverted <- (input$view_mode == "indicator") && col %in% invert_raw_cols
    
    pal_colors <- if (is_inverted) {
      rev(c("#FFF3D6","#F0C75E","#DD8844","#C05235","#9B2226"))
    } else {
      c("#FFF3D6","#F0C75E","#DD8844","#C05235","#9B2226")
    }
    
    pal <- colorNumeric(pal_colors, domain = pal_dom, na.color = "lightgrey")
    
    legend_title <- if (input$view_mode == "score") {
      suffix <- if (is_rank) "\n(Percentile Rank)" else "\n(Raw Score)"
      paste0(str_wrap(input$score_view, 20), suffix)
    } else {
      str_wrap(input$indicator, 20)
    }
    
    # Pre-compute indicator config lookups
    is_ind <- input$view_mode == "indicator"
    if (is_ind) {
      req(input$component, input$factor, input$indicator)
      ind_cfg       <- indicator_config[[input$component]][[input$factor]][[input$indicator]]
      req(!is.null(ind_cfg))
      popup_score   <- NA_character_
      popup_comp    <- input$component
      popup_fac     <- input$factor
      popup_ind     <- input$indicator
      popup_rawlbl  <- ind_cfg$raw_label
      raw_col_name  <- ind_cfg$raw_col
    } else {
      popup_score   <- input$score_view
      popup_comp    <- NA_character_
      popup_fac     <- NA_character_
      popup_ind     <- NA_character_
      popup_rawlbl  <- NA_character_
      raw_col_name  <- NULL
    }
    
    df        <- as.data.frame(plot_data)
    plot_vals <- df[[col]]
    raw_vals  <- if (is_ind && !is.null(raw_col_name)) df[[raw_col_name]] else rep(NA_real_, nrow(df))
    
    # sort descending by the value driving radius, BEFORE building dependent vectors
    ord       <- order(plot_vals, decreasing = TRUE)
    plot_data <- plot_data[ord, ]
    plot_vals <- plot_vals[ord]
    raw_vals  <- raw_vals[ord]
    df        <- df[ord, ]
    
    radius_vec <- if (is_inverted) {
      ifelse(is.na(plot_vals), 3,
             pmax(2, pmin(12, scales::rescale(plot_vals, to=c(15,5), from=pal_dom))))
    } else {
      ifelse(is.na(plot_vals), 3,
             pmax(2, pmin(12, scales::rescale(plot_vals, to=c(5,15), from=pal_dom))))
    }
    fill_vec   <- pal(plot_vals)
    
    popup_vec <- unlist(Map(
      create_popup,
      raw_value = raw_vals, col_value = plot_vals,
      vuln_rank = df[["VULNERABILITY_rank"]],
      exp_rank = df[["EXPOSURE_rank"]], sen_rank = df[["SENSITIVITY_rank"]],
      priority = df[["priority_group"]], flag_fire = df[["flag_fire"]],
      flag_flood = df[["flag_flood"]], flag_slr = df[["flag_slr"]], flag_drought = df[["flag_drought"]],
      wsd_source_id = df[["wsd_source_id"]], park_unit = df[["park_unit"]],
      park_name = df[["park_name"]], water_system_name = df[["water_system_name"]],
      source_type = df[["source_type"]], description = df[["description"]],
      state = df[["state"]],
      MoreArgs = list(view_mode = input$view_mode,
                      score_label = popup_score, component = popup_comp,
                      factor_name = popup_fac, indicator_name = popup_ind,
                      raw_label = popup_rawlbl)
    ))
    
    # force legend to show legend points sized and colored by var
    breaks <- pretty(pal_dom, n = 5)
    breaks <- breaks[breaks >= pal_dom[1] & breaks <= pal_dom[2]]
    
    legend_radius <- if (is_inverted) {
      pmax(4, pmin(16, scales::rescale(breaks, to = c(15, 5), from = pal_dom)))
    } else {
      pmax(4, pmin(16, scales::rescale(breaks, to = c(5, 15), from = pal_dom)))
    }
    legend_colors <- pal(breaks)
    
    legend_rows <- paste0(
      "<div style='display:flex;align-items:center;margin:3px 0;'>",
      "<div style='width:", legend_radius * 2, "px;height:", legend_radius * 2, "px;",
      "border-radius:50%;background:", legend_colors, ";border:1px solid #1D3557;",
      "margin-right:8px;flex-shrink:0;'></div>",
      "<span>", round(breaks, 2), "</span>",
      "</div>",
      collapse = ""
    )
    
    legend_html <- paste0(
      "<div style='background:white;padding:8px 10px;border-radius:6px;",
      "box-shadow:0 2px 8px rgba(0,0,0,0.2);font-size:12px;max-width:180px;'>",
      "<div style='font-weight:600;margin-bottom:6px;'>", gsub("\n", "<br/>", legend_title), "</div>",
      legend_rows,
      "</div>"
    )
    
    # ADD DATA POINTS ------------
    leafletProxy("map") %>%
      removeControl("legend") %>% 
      clearGroup(c("data_points", "highlight")) %>%
      addCircleMarkers(
        data = plot_data,
        group = "data_points",
        radius = radius_vec,
        color = "#1D3557",
        fillColor = fill_vec,
        fillOpacity = 0.75,
        stroke = TRUE,
        weight = 1,
        popup = popup_vec,
        label = as.data.frame(plot_data)[["park_name"]],
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", "font-size" = "12px"),
          textsize = "12px",
          direction = "auto"
        ),
        options = pathOptions(pane = "markers"),
        layerId = as.data.frame(plot_data)[["wsd_source_id"]]
      ) %>%
      addControl(html = legend_html, position = "bottomright", layerId = "legend")
    # addLegend(
    #   pal = pal,
    #   values = na.omit(vals),
    #   title = legend_title,
    #   na.label = "No Data",
    #   position = "bottomright",
    #   opacity = 1,
    #   layerId = "legend"
    # )
  })
  

  
  ## Marker Click: Site Chart Modal ----
  clicked_site <- reactiveVal(NULL)
  
  
  # ── Marker click — just track the site ──────────────────────────────────
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    req(!is.null(click$id))
    clicked_site(click$id)
  })
  
  ## Raw Indicator Distribution Chart (lower-left map overlay) ----
  # National histogram always shows. A second, filtered histogram appears
  # below it once any active filter narrows the data below the full national
  # set. Both use identical bin edges (computed from the national range) so
  # they're directly comparable on the same x-axis. A vertical line marks
  # the last-clicked water supply's raw value when it's available.
  output$indicator_distribution_chart <- renderPlotly({
    req(input$view_mode == "indicator")
    req(input$component, input$factor, input$indicator)
    cfg <- indicator_config[[input$component]][[input$factor]][[input$indicator]]
    req(!is.null(cfg))
    raw_col <- cfg$raw_col
    
    nat_df <- as.data.frame(combined_data)
    req(raw_col %in% names(nat_df))
    nat_vals <- suppressWarnings(as.numeric(nat_df[[raw_col]]))
    nat_vals <- nat_vals[is.finite(nat_vals)]
    req(length(nat_vals) > 0)
    
    x_range <- range(nat_vals)
    pad <- diff(x_range) * 0.04
    if (pad == 0) pad <- 0.5
    x_range <- c(x_range[1] - pad, x_range[2] + pad)
    
    n_bins    <- 20
    bin_edges <- seq(x_range[1], x_range[2], length.out = n_bins + 1)
    bin_size  <- diff(bin_edges)[1]
    
    filt_df     <- as.data.frame(filtered_data_display())
    is_filtered <- nrow(filt_df) < nrow(nat_df)
    filt_vals   <- if (is_filtered && raw_col %in% names(filt_df)) {
      v <- suppressWarnings(as.numeric(filt_df[[raw_col]])); v[is.finite(v)]
    } else {
      numeric(0)
    }
    
    # Selected water supply (last clicked marker). Looked up nationally so
    # its value still resolves even if filters have since excluded it from
    # the current view.
    sel_id  <- clicked_site()
    sel_val <- NA_real_
    sel_in_filtered <- FALSE
    if (!is.null(sel_id)) {
      row <- nat_df[nat_df$wsd_source_id == sel_id, ]
      if (nrow(row) > 0) {
        v <- suppressWarnings(as.numeric(row[[raw_col]][1]))
        if (is.finite(v)) sel_val <- v
        sel_in_filtered <- sel_id %in% filt_df$wsd_source_id
      }
    }
    
    x_title <- str_wrap(cfg$raw_label, 40)
    
    highlight_shape <- function(x0) {
      list(type = "line", x0 = x0, x1 = x0, y0 = 0, y1 = 1, yref = "paper",
           line = list(color = "#C05235", width = 2))
    }
    
    # Pre-bin manually (rather than letting plotly's histogram trace bin
    # client-side) so each bar's hover text can show its exact value range
    # above the site count.
    bin_hover_data <- function(vals, edges) {
      n_edges <- length(edges) - 1
      idx     <- cut(vals, breaks = edges, include.lowest = TRUE, right = FALSE, labels = FALSE)
      counts  <- as.integer(table(factor(idx, levels = seq_len(n_edges))))
      centers <- (head(edges, -1) + tail(edges, -1)) / 2
      lo      <- signif(head(edges, -1), 3)
      hi      <- signif(tail(edges, -1), 3)
      hover   <- paste0(lo, " \u2013 ", hi, "<br>", counts, " sites")
      list(x = centers, y = counts, hover = hover)
    }
    
    nat_bins <- bin_hover_data(nat_vals, bin_edges)
    
    p_nat <- plot_ly() %>%
      add_trace(x = nat_bins$x, y = nat_bins$y, type = "bar", width = bin_size,
                marker = list(color = "#457B9D", line = list(color = "white", width = 0.5)),
                hovertext = nat_bins$hover, hoverinfo = "text", name = "National") %>%
      layout(
        annotations = list(list(
          text = paste0("National (n=", length(nat_vals), ")"),
          x = 0, y = 1, xref = "paper", yref = "paper", xanchor = "left", yanchor = "bottom",
          showarrow = FALSE, font = list(size = 11, color = "#1D3557")
        )),
        yaxis = list(title = "", showticklabels = FALSE),
        xaxis = list(title = "", range = x_range),
        bargap = 0.05, margin = list(t = 18, b = 5, l = 5, r = 5)
      )
    if (is.finite(sel_val)) p_nat <- p_nat %>% layout(shapes = list(highlight_shape(sel_val)))
    
    # No active filter (or filter didn't narrow anything) -- national chart only
    if (!is_filtered || length(filt_vals) == 0) {
      return(p_nat %>% layout(xaxis = list(title = x_title, range = x_range), showlegend = FALSE))
    }
    
    filt_bins <- bin_hover_data(filt_vals, bin_edges)
    
    p_filt <- plot_ly() %>%
      add_trace(x = filt_bins$x, y = filt_bins$y, type = "bar", width = bin_size,
                marker = list(color = "#386150", line = list(color = "white", width = 0.5)),
                hovertext = filt_bins$hover, hoverinfo = "text", name = "Filtered") %>%
      layout(
        annotations = list(list(
          text = paste0("Filtered View (n=", length(filt_vals), ")"),
          x = 0, y = 1, xref = "paper", yref = "paper", xanchor = "left", yanchor = "bottom",
          showarrow = FALSE, font = list(size = 11, color = "#386150")
        )),
        yaxis = list(title = "", showticklabels = FALSE),
        xaxis = list(title = x_title, range = x_range),
        bargap = 0.05, margin = list(t = 18, b = 32, l = 5, r = 5)
      )
    if (is.finite(sel_val) && sel_in_filtered) {
      p_filt <- p_filt %>% layout(shapes = list(highlight_shape(sel_val)))
    }
    
    subplot(p_nat, p_filt, nrows = 2, shareX = TRUE, titleX = TRUE, margin = 0.05) %>%
      layout(showlegend = FALSE)
  })
  
  ## Distribution Chart Text Alternative (screen-reader only) ----
  # The plotly chart above conveys shape visually; this gives an equivalent
  # numeric summary for anyone who can't see or interact with the chart.
  output$indicator_distribution_srsummary <- renderUI({
    req(input$view_mode == "indicator")
    req(input$component, input$factor, input$indicator)
    cfg <- indicator_config[[input$component]][[input$factor]][[input$indicator]]
    req(!is.null(cfg))
    raw_col <- cfg$raw_col
    
    nat_df <- as.data.frame(combined_data)
    req(raw_col %in% names(nat_df))
    nat_vals <- suppressWarnings(as.numeric(nat_df[[raw_col]]))
    nat_vals <- nat_vals[is.finite(nat_vals)]
    req(length(nat_vals) > 0)
    
    nat_summary <- sprintf(
      "National distribution of %s across %d sites: minimum %s, maximum %s, mean %s.",
      cfg$raw_label, length(nat_vals), signif(min(nat_vals), 3), signif(max(nat_vals), 3), signif(mean(nat_vals), 3)
    )
    
    filt_df   <- as.data.frame(filtered_data_display())
    filt_vals <- if (nrow(filt_df) < nrow(nat_df) && raw_col %in% names(filt_df)) {
      v <- suppressWarnings(as.numeric(filt_df[[raw_col]])); v[is.finite(v)]
    } else {
      numeric(0)
    }
    
    filt_summary <- if (length(filt_vals) > 0) {
      sprintf(" Filtered distribution across %d sites: minimum %s, maximum %s, mean %s.",
              length(filt_vals), signif(min(filt_vals), 3), signif(max(filt_vals), 3), signif(mean(filt_vals), 3))
    } else {
      ""
    }
    
    tags$p(class = "sr-only", paste0(nat_summary, filt_summary))
  })
  
  ## Distribution Chart Caption: names the highlighted site ----
  output$indicator_distribution_caption <- renderUI({
    req(input$view_mode == "indicator")
    sel_id <- clicked_site()
    if (is.null(sel_id)) {
      return(tags$div(style = "font-size:1rem; color:#696969; margin-top:2px;",
                      "Click a water supply on the map to highlight its value."))
    }
    req(input$component, input$factor, input$indicator)
    cfg <- indicator_config[[input$component]][[input$factor]][[input$indicator]]
    req(!is.null(cfg))
    nat_df <- as.data.frame(combined_data)
    row <- nat_df[nat_df$wsd_source_id == sel_id, ]
    if (nrow(row) == 0) return(NULL)
    val   <- suppressWarnings(as.numeric(row[[cfg$raw_col]][1]))
    label <- row$park_unit[1]
    if (!is.finite(val)) {
      tags$div(style = "font-size:1rem; color:#696969; margin-top:2px;",
               paste0(label, " \u2013 value not available for this indicator"))
    } else {
      tags$div(style = "font-size:1rem; color:#C05235; font-weight:600; margin-top:2px;",
               paste0("\u25CF ", label, ": ", signif(val, 3)))
    }
  })
  
  ## Table Row Click: Zoom to Site ----
  observeEvent(input$data_table_rows_selected, {
    row_idx <- input$data_table_rows_selected
    req(length(row_idx) > 0)
    
    # Use the cached ID vector (same row order DT received)
    ids <- dt_ids()
    req(length(ids) >= row_idx)
    selected_id <- ids[row_idx]
    req(!is.null(selected_id), !is.na(selected_id))
    
    # Look up coordinates from the pre-computed global lookup (no reactive dependency)
    site_coords <- coord_lookup[coord_lookup$wsd_source_id == selected_id, ]
    req(nrow(site_coords) > 0)
    
    lng <- site_coords$lng[1]
    lat <- site_coords$lat[1]
    req(is.finite(lng), is.finite(lat))
    
    leafletProxy("map") %>%
      clearGroup("highlight") %>%
      setView(lng = lng, lat = lat, zoom = 12) %>%
      addCircleMarkers(lng = lng, lat = lat,
                       radius = 18, color = "aquamarine", fillColor = "transparent",
                       fillOpacity = 0, stroke = TRUE, weight = 4, opacity = 1,
                       group = "highlight",
                       options = pathOptions(pane = "highlights"))
  })
  
  ## Score contribution chart -------------
  observeEvent(input$show_chart_btn, {
    site_id <- input$show_chart_btn
    df_all  <- as.data.frame(filtered_data())
    req(site_id %in% df_all$wsd_source_id)
    
    site_row <- df_all[df_all$wsd_source_id == site_id, ]
    site_row <- site_row[1, , drop = FALSE]
    src_type <- site_row$source_type[1]
    is_rainwater <- !is.na(src_type) && src_type == "rainwater"
    is_ocean <- !is.na(src_type) && src_type == "ocean"
    
    EXP_COLOR  <- "#5B8C6E"
    SEN_COLOR  <- "#7C6FAD"
    VULN_COLOR <- "#C05235"
    IND_COLOR  <- "#B7B7B7"
    NA_COLOR   <- "#c9c9c9"
    
    site_title  <- paste0(site_row$park_unit[1], " \u2013 ", site_row$wsd_source_id[1])
    scope_label <- if (length(input$filter_state) > 0) paste("State:", paste(input$filter_state, collapse = ", "))
    else if (length(input$filter_region) > 0) paste("Region:", paste(input$filter_region, collapse = ", "))
    else "National (CONUS)"
    
    # -----------------------------------------------------------------
    # Shared factor definitions (used by both tabs). safe_div guards
    # two real zero-denominator cases found in the data (a site with
    # SENSITIVITY==0 exactly, and vector-numerator division where ifelse()
    # silently truncates to the length of its test argument -- do not use
    # ifelse() for this, use the mask-assignment form instead).
    # -----------------------------------------------------------------
    factor_defs <- list(
      list(id="factor_exp_runoff",   comp="Exposure",    label="Runoff",   type="single"),
      list(id="factor_exp_precip",   comp="Exposure",    label="Precip",   type="single"),
      list(id="factor_exp_drought",  comp="Exposure",    label="Drought",  type="single"),
      list(id="factor_exp_slr",      comp="Exposure",    label="Sea Level Rise",      type="euclidean",
           indicators=list(
             list(norm_col="norm_exp_inundation_slr", label="Inundation"),
             list(norm_col="norm_exp_swi",             label="Saltwater Intrusion"),
             list(norm_col="norm_exp_storm_surge",     label="Storm Surge")
           )),
      list(id="factor_exp_wildfire", comp="Exposure",    label="Wildfire", type="single"),
      list(id="factor_exp_demand", comp="Exposure", label="Demand", type="single"),
      list(id="factor_sen_demand",   comp="Sensitivity", label="Demand",   type="euclidean",
           indicators=list(
             list(norm_col="norm_sen_visitation_trend", label="Visitation Trend"),
             list(norm_col="norm_sen_competition",       label="Competition")
           )),
      list(id="factor_sen_wildfire", comp="Sensitivity", label="Wildfire", type="single"),
      list(id="factor_sen_flood",    comp="Sensitivity", label="Flood",    type="single"),
      list(id="factor_sen_slr",      comp="Sensitivity", label="Sea Level Rise",      type="single"),
      list(id="factor_sen_runoff",   comp="Sensitivity", label="Runoff",   type="single"),
      list(id="factor_sen_precip",   comp="Sensitivity", label="Precip",   type="single"),
      list(id="factor_sen_drought",  comp="Sensitivity", label="Drought", type="single")
    )
    na_flag_for <- function(id) {
      if (is_ocean) {
        grepl("runoff", id) | grepl("precip", id)
      } else if (is_rainwater) {
        grepl("runoff", id)
      } else {
        grepl("precip", id)
      }
    }
    
    
    safe_div <- function(num, den) { out <- num / den; out[den == 0] <- 0; out }
    
    
    # ===================================================================
    # TAB 1: "Component Contribution" -- factor-level bar chart
    # ===================================================================
    build_hover <- function(d, header) {
      if (d$type == "euclidean") {
        norm_vals <- vapply(d$indicators, function(ind) site_row[[ind$norm_col]][1], numeric(1))
        shares <- round(100 * norm_vals^2 / sum(norm_vals^2, na.rm = TRUE))
        lines <- vapply(seq_along(d$indicators), function(j) {
          sprintf("%s: %d%%", d$indicators[[j]]$label, shares[j])
        }, character(1))
        paste(c(header, lines), collapse = "<br>")
      } else {
        paste(c(header, "(Single Indicator)"), collapse = "<br>")
      }
    }
    
    build_component_df <- function(comp_name) {
      defs     <- Filter(function(d) d$comp == comp_name, factor_defs)
      fac_cols <- vapply(defs, function(d) d$id, character(1))
      vals     <- as.numeric(site_row[1, fac_cols])
      contrib  <- 100 * safe_div(vals^2, sum(vals^2, na.rm = TRUE))
      suffix   <- if (comp_name == "Exposure") " (E)" else " (S)"
      
      rows <- lapply(seq_along(defs), function(i) {
        d <- defs[[i]]
        status  <- if (na_flag_for(d$id)) "n/a" else if (is.na(vals[i])) "missing" else "scored"
        lbl     <- paste0(d$label, suffix)
        if (status == "scored") {
          header   <- sprintf("%s \u2014 %.0f%%", lbl, contrib[i])
          hover    <- build_hover(d, header)
          bar_text <- sprintf("%.0f%%", contrib[i])
        } else {
          hover    <- if (status == "n/a") paste0(lbl, "<br>N/A for this system type")
          else paste0(lbl, "<br>No data available")
          bar_text <- if (status == "n/a") "N/A for this system type" else "No data available"
        }
        data.frame(display_label = lbl, status = status,
                   contrib = if (status == "scored") contrib[i] else NA_real_,
                   bar_text = bar_text, hover = hover,
                   component = comp_name, stringsAsFactors = FALSE)
      })
      d_out <- do.call(rbind, rows)
      status_rank <- match(d_out$status, c("scored", "n/a", "missing"))
      d_out[order(status_rank, -ifelse(is.na(d_out$contrib), -Inf, d_out$contrib)), ]
    }
    
    fac_df <- rbind(build_component_df("Exposure"), build_component_df("Sensitivity"))
    fac_df$bar_x <- ifelse(fac_df$status == "scored", fac_df$contrib, 0)
    exp_mask <- fac_df$component == "Exposure"    & fac_df$status == "scored"
    sen_mask <- fac_df$component == "Sensitivity" & fac_df$status == "scored"
    na_mask  <- fac_df$status != "scored"
    
    p_bars <- plot_ly() %>%
      add_trace(x = fac_df$bar_x[exp_mask], y = fac_df$display_label[exp_mask],
                type = "bar", orientation = "h", name = "Exposure",
                marker = list(color = EXP_COLOR, line = list(color = "rgba(0,0,0,0.1)", width = 0.5)),
                text = fac_df$bar_text[exp_mask], textposition = "outside",
                textfont = list(size = 10.5, color = "#444"),
                hovertemplate = paste0(fac_df$hover[exp_mask], "<extra></extra>")) %>%
      add_trace(x = fac_df$bar_x[sen_mask], y = fac_df$display_label[sen_mask],
                type = "bar", orientation = "h", name = "Sensitivity",
                marker = list(color = SEN_COLOR, line = list(color = "rgba(0,0,0,0.1)", width = 0.5)),
                text = fac_df$bar_text[sen_mask], textposition = "outside",
                textfont = list(size = 10.5, color = "#444"),
                hovertemplate = paste0(fac_df$hover[sen_mask], "<extra></extra>")) %>%
      add_trace(x = fac_df$bar_x[na_mask], y = fac_df$display_label[na_mask],
                type = "bar", orientation = "h", name = "N/A / no data",
                marker = list(color = NA_COLOR),
                text = fac_df$bar_text[na_mask], textposition = "outside",
                textfont = list(size = 10.5, color = "#999"),
                hovertemplate = paste0(fac_df$hover[na_mask], "<extra></extra>")) %>%
      layout(
        title = list(
          text = paste0(site_title,
                        "<br><span style='font-size:11px;color:#696969;font-weight:400;'>",
                        "% share of Exposure / Sensitivity score \u00b7 ", scope_label,
                        " \u00b7 hover a bar for its indicator breakdown</span>"),
          font = list(size = 14, color = "#1D3557"), x = 0, xanchor = "left"),
        xaxis = list(title = list(text = "% Contribution to Component Score", standoff = 15),
                     range = c(0, 112), tickfont = list(size = 10), zeroline = FALSE, gridcolor = "#eee"),
        yaxis = list(title = "", tickfont = list(size = 11),
                     categoryorder = "array", categoryarray = rev(fac_df$display_label)),
        margin = list(t = 60, l = 10, r = 20, b = 70),
        legend = list(orientation = "h", y = -0.22, font = list(size = 11)),
        bargap = 0.3, plot_bgcolor = "white", paper_bgcolor = "white"
      )
    
    # ===================================================================
    # TAB 2: "All Levels Contribution" -- Indicator -> Factor -> Component
    # -> Vulnerability, as a nested icicle chart (indicators on the left,
    # Vulnerability on the right, via tiling$flip="x"). 
    # -----------------------------------------------------------------
    
    ic_ids <- character(0); ic_labels <- character(0); ic_parents <- character(0)
    ic_values <- numeric(0); ic_colors <- character(0)
    ic_add <- function(id, label, parent, value, color) {
      ic_ids     <<- c(ic_ids, id)
      ic_labels  <<- c(ic_labels, label)
      ic_parents <<- c(ic_parents, parent)
      ic_values  <<- c(ic_values, value)
      ic_colors  <<- c(ic_colors, color)
    }
    
    exp_defs <- Filter(function(d) d$comp == "Exposure", factor_defs)
    sen_defs <- Filter(function(d) d$comp == "Sensitivity", factor_defs)
    exp_vals <- as.numeric(site_row[1, sapply(exp_defs, function(d) d$id)])
    sen_vals <- as.numeric(site_row[1, sapply(sen_defs, function(d) d$id)])
    exp_denom <- sum(exp_vals^2, na.rm = TRUE)
    sen_denom <- sum(sen_vals^2, na.rm = TRUE)
    vuln_denom <- site_row$EXPOSURE[1]^2 + site_row$SENSITIVITY[1]^2
    exp_share_vuln <- 100 * safe_div(site_row$EXPOSURE[1]^2, vuln_denom)
    sen_share_vuln <- 100 * safe_div(site_row$SENSITIVITY[1]^2, vuln_denom)
    
    ic_add("Vulnerability", "Vulnerability", "", 100, VULN_COLOR)
    ic_add("Exposure", "Exposure", "Vulnerability", exp_share_vuln, EXP_COLOR)
    ic_add("Sensitivity", "Sensitivity", "Vulnerability", sen_share_vuln, SEN_COLOR)
    
    ic_place_component <- function(defs, vals, denom, comp_id, comp_share_vuln, color) {
      scored <- !sapply(defs, function(d) na_flag_for(d$id)) & !is.na(vals)
      for (i in seq_along(defs)) {
        if (!scored[i]) next
        d <- defs[[i]]
        suffix <- if (comp_id == "Exposure") " (E)" else " (S)"
        lbl <- paste0(d$label, suffix)
        fid <- paste0(comp_id, "/", lbl)
        factor_share <- safe_div(vals[i]^2, denom)
        factor_vuln  <- factor_share * comp_share_vuln
        ic_add(fid, lbl, comp_id, factor_vuln, color)
        if (d$type == "euclidean") {
          norm_vals <- vapply(d$indicators, function(ind) site_row[[ind$norm_col]][1], numeric(1))
          ind_share <- safe_div(norm_vals^2, sum(norm_vals^2, na.rm = TRUE))
          for (j in seq_along(d$indicators)) {
            ind <- d$indicators[[j]]
            ic_add(paste0(fid, "/", ind$label), ind$label, fid, ind_share[j] * factor_vuln, IND_COLOR)
          }
        }
      }
    }
    ic_place_component(exp_defs, exp_vals, exp_denom, "Exposure",    exp_share_vuln, EXP_COLOR)
    ic_place_component(sen_defs, sen_vals, sen_denom, "Sensitivity", sen_share_vuln, SEN_COLOR)
    
    ic_text <- sprintf("%s<br>%.1f%%", ic_labels, ic_values)
    
    p_icicle <- plot_ly(
      type = "icicle", ids = ic_ids, labels = ic_labels, parents = ic_parents,
      values = ic_values, branchvalues = "total",
      text = ic_text, textinfo = "text", textfont = list(size = 13),
      hovertemplate = paste0(ic_text, "<extra></extra>"),
      marker = list(colors = ic_colors, line = list(width = 1, color = "white")),
      tiling = list(orientation = "h", flip = "x")
    ) %>% layout(
      title = list(text = paste0(site_title, " \u00b7 ", scope_label), font = list(size = 13, color = "#1D3557")),
      margin = list(t = 40, l = 10, r = 10, b = 10)
    )
    
    # ===================================================================
    # Modal with both tabs
    # ===================================================================
    showModal(modalDialog(
      title = paste0("Score Contribution Breakdown \u2013 ", site_title),
      tabsetPanel(
        tabPanel("Component Contribution",
                 renderPlotly(p_bars),
                 tags$p(
                   HTML(paste0(
                     "Bars show each factor's <b>share of this site's Exposure or Sensitivity score</b> ",
                     "&mdash; calculated relative to the current comparison group: <b>", scope_label, "</b>. ",
                     "Hover a bar to see its underlying indicator(s). Grey bars are not scored: either not ",
                     "applicable to this water system's source type, or no data currently available."
                   )),
                   style = "font-size:11.5px; color:#4f4f4f; margin-top:6px; margin-bottom:0;"
                 )
        ),
        tabPanel("All Levels Contribution",
                 renderPlotly(p_icicle),
                 tags$p(
                   HTML(paste0(
                     "Nested boxes show each item's <b>share of ", site_title, "'s overall Vulnerability score</b>, ",
                     "from raw indicators (left) through factors and components to the final index (right). ",
                     "Hover any box for its exact percentage. Only scored indicators/factors are shown; see the ",
                     "Component Contribution tab for N/A and missing-data flags."
                   )),
                   style = "font-size:11.5px; color:#4f4f4f; margin-top:6px; margin-bottom:0;"
                 )
        )
      ),
      size      = "l",
      easyClose = TRUE,
      footer    = modalButton("Close")
    ))
  })
  
  ## Generate Report ----
  observeEvent(input$generate_report_btn, {
    site_id <- input$generate_report_btn
    req(site_id %in% combined_raw$wsd_source_id)
    
    notif_id <- showNotification(
      "Generating report...", duration = NULL, closeButton = FALSE, type = "message"
    )
    
    tryCatch({
      ### Site Metadata ----
      site_meta  <- combined_raw[combined_raw$wsd_source_id == site_id, ]
      site_park  <- site_meta$park_unit[1]
      site_state <- site_meta$state[1]
      site_region <- site_meta$region[1]
      sys_type   <- ifelse(is.na(site_meta$source_type[1]), "N/A", site_meta$source_type[1])
      desc_text  <- ifelse(is.na(site_meta$description[1]) || nchar(trimws(site_meta$description[1])) == 0,
                           "", site_meta$description[1])
      
      ### Helper: Extract Ranks ----
      extract_ranks <- function(scored_df, sid) {
        row <- scored_df[scored_df$wsd_source_id == sid, ]
        if (nrow(row) == 0) return(list(vuln_rank = NA, exp_rank = NA, sen_rank = NA,
                                        n = nrow(scored_df), priority = NA))
        list(
          vuln_rank = round(row$VULNERABILITY_rank[1], 1),
          exp_rank  = round(row$EXPOSURE_rank[1], 1),
          sen_rank  = round(row$SENSITIVITY_rank[1], 1),
          n         = nrow(scored_df),
          priority  = isTRUE(row$priority_group[1])
        )
      }
      
      ### National Ranks (Reuse Combined Data) ----
      national_df <- as.data.frame(combined_data)
      national    <- extract_ranks(national_df, site_id)
      
      site_scored <- national_df[national_df$wsd_source_id == site_id, ]
      
      ### Regional, State & Park Ranks (Recalculated) ----
      regional <- if (nrow(combined_raw[combined_raw$region == site_region, ]) >= 2) {
        extract_ranks(calc_vulnerability_index(combined_raw[combined_raw$region == site_region, ]), site_id)
      } else { list(vuln_rank = NA, exp_rank = NA, sen_rank = NA, n = 1, priority = NA) }
      
      state_ranks <- if (nrow(combined_raw[combined_raw$state == site_state, ]) >= 2) {
        extract_ranks(calc_vulnerability_index(combined_raw[combined_raw$state == site_state, ]), site_id)
      } else { list(vuln_rank = NA, exp_rank = NA, sen_rank = NA, n = 1, priority = NA) }
      
      park_ranks <- if (nrow(combined_raw[combined_raw$park_unit == site_park, ]) >= 2) {
        extract_ranks(calc_vulnerability_index(combined_raw[combined_raw$park_unit == site_park, ]), site_id)
      } else { list(vuln_rank = NA, exp_rank = NA, sen_rank = NA, n = 1, priority = NA) }
      
      ### Component Contribution & All Levels Contribution Charts (Base64 PNG) ----
      # Static counterparts of the two interactive charts in the score
      # breakdown popup (same underlying math, same colors), since the
      # report embeds images rather than live Plotly widgets. National
      # scope throughout, matching the rank table above -- NOT whatever
      # map filter happens to be active when the button is clicked, so the
      # report is reproducible regardless of transient UI state.
      EXP_COLOR <- "#5B8C6E"; SEN_COLOR <- "#7C6FAD"; VULN_COLOR <- "#C05235"
      IND_COLOR <- "#B7B7B7"; CHART_NA_COLOR <- "#c9c9c9"
      is_rainwater <- !is.na(site_meta$source_type[1]) && site_meta$source_type[1] == "rainwater"
      is_ocean <- !is.na(site_meta$source_type[1]) && site_meta$source_type[1] == "ocean"
      
      rpt_factor_defs <- list(
        list(id="factor_exp_runoff",   comp="Exposure",    label="Runoff",   type="single"),
        list(id="factor_exp_precip",   comp="Exposure",    label="Precip",   type="single"),
        list(id="factor_exp_drought",  comp="Exposure",    label="Drought",  type="single"),
        list(id="factor_exp_drought",  comp="Exposure",    label="Demand",   type="single"),
        list(id="factor_exp_slr",      comp="Exposure",    label="Sea Level Rise", type="euclidean",
             indicators=list(
               list(norm_col="norm_exp_inundation_slr", label="Inundation"),
               list(norm_col="norm_exp_swi",             label="Saltwater Intrusion"),
               list(norm_col="norm_exp_storm_surge",     label="Storm Surge")
             )),
        list(id="factor_exp_wildfire", comp="Exposure",    label="Wildfire", type="single"),
        list(id="factor_sen_demand",   comp="Sensitivity", label="Demand",   type="euclidean",
             indicators=list(
               list(norm_col="norm_sen_visitation_trend", label="Visitation Trend"),
               list(norm_col="norm_sen_competition",       label="Competition")
             )),
        list(id="factor_sen_wildfire", comp="Sensitivity", label="Wildfire", type="single"),
        list(id="factor_sen_flood",    comp="Sensitivity", label="Flood",    type="single"),
        list(id="factor_sen_slr",      comp="Sensitivity", label="Sea Level Rise", type="single"),
        list(id="factor_sen_runoff",   comp="Sensitivity", label="Runoff",   type="single"),
        list(id="factor_sen_precip",   comp="Sensitivity", label="Precip",   type="single"),
        list(id="factor_sen_drought",  comp='Sensitivity', label="Drought",  type = "single")
      )
      rpt_na_flag_for <- function(id) {
        if (is_ocean) {
          grepl("runoff", id) | grepl("precip", id)
        } else if (is_rainwater) {
          grepl("runoff", id)
        } else {
          grepl("precip", id)
        }
      }
      rpt_safe_div <- function(num, den) { out <- num / den; out[den == 0] <- 0; out }
      
      #### Component Contribution bar chart ----
      rpt_build_component_df <- function(comp_name) {
        defs     <- Filter(function(d) d$comp == comp_name, rpt_factor_defs)
        fac_cols <- vapply(defs, function(d) d$id, character(1))
        vals     <- as.numeric(site_scored[1, fac_cols])
        contrib  <- 100 * rpt_safe_div(vals^2, sum(vals^2, na.rm = TRUE))
        suffix   <- if (comp_name == "Exposure") " (E)" else " (S)"
        rows <- lapply(seq_along(defs), function(i) {
          d <- defs[[i]]
          status <- if (rpt_na_flag_for(d$id)) "n/a" else if (is.na(vals[i])) "missing" else "scored"
          data.frame(display_label = paste0(d$label, suffix), status = status,
                     contrib = if (status == "scored") contrib[i] else NA_real_,
                     component = comp_name, stringsAsFactors = FALSE)
        })
        d_out <- do.call(rbind, rows)
        status_rank <- match(d_out$status, c("scored", "n/a", "missing"))
        d_out[order(status_rank, -ifelse(is.na(d_out$contrib), -Inf, d_out$contrib)), ]
      }
      
      contrib_df <- rbind(rpt_build_component_df("Exposure"), rpt_build_component_df("Sensitivity"))
      contrib_df$bar_x <- ifelse(contrib_df$status == "scored", contrib_df$contrib, 0)
      contrib_df$fill_color <- ifelse(contrib_df$status != "scored", CHART_NA_COLOR,
                                      ifelse(contrib_df$component == "Exposure", EXP_COLOR, SEN_COLOR))
      contrib_df$bar_label <- ifelse(contrib_df$status == "scored", sprintf("%.0f%%", contrib_df$contrib),
                                     ifelse(contrib_df$status == "n/a", "N/A", "No data"))
      contrib_df$order_key <- with(contrib_df,
                                   ifelse(component == "Exposure", 1, 2) * 1000 - ifelse(is.na(bar_x), 0, bar_x))
      contrib_df <- contrib_df[order(contrib_df$order_key), ]
      contrib_df$display_label <- factor(contrib_df$display_label, levels = rev(contrib_df$display_label))
      
      p_contrib <- ggplot2::ggplot(contrib_df, ggplot2::aes(x = display_label, y = bar_x, fill = fill_color)) +
        ggplot2::geom_col(width = 0.65) +
        ggplot2::geom_text(ggplot2::aes(label = bar_label), hjust = -0.1, size = 3.2, color = "#333333") +
        ggplot2::scale_fill_identity() +
        ggplot2::scale_y_continuous(limits = c(0, max(contrib_df$bar_x, na.rm = TRUE) * 1.25),
                                    expand = c(0, 0)) +
        ggplot2::coord_flip() +
        ggplot2::labs(x = NULL, y = "% Contribution to Component Score") +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
          panel.grid.major.y = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_text(size = 9),
          plot.background = ggplot2::element_rect(fill = "white", color = NA),
          panel.background = ggplot2::element_rect(fill = "white", color = NA)
        )
      
      contrib_chart_file <- tempfile(fileext = ".png")
      ggplot2::ggsave(contrib_chart_file, p_contrib, width = 7.5, height = 4.5, dpi = 150, bg = "white")
      contrib_chart_b64 <- base64enc::base64encode(contrib_chart_file)
      
      #### All Levels Contribution icicle chart ----
      # Recursive partition layout: no ggplot2 geom does true left-to-right
      # icicle bands natively, so rectangle geometry is computed by hand
      # from the same tree structure the interactive version builds.
      rpt_ic_ids <- character(0); rpt_ic_labels <- character(0); rpt_ic_parents <- character(0)
      rpt_ic_values <- numeric(0); rpt_ic_colors <- character(0); rpt_ic_level <- integer(0)
      rpt_ic_add <- function(id, label, parent, value, color, level) {
        rpt_ic_ids     <<- c(rpt_ic_ids, id); rpt_ic_labels <<- c(rpt_ic_labels, label)
        rpt_ic_parents <<- c(rpt_ic_parents, parent); rpt_ic_values <<- c(rpt_ic_values, value)
        rpt_ic_colors  <<- c(rpt_ic_colors, color); rpt_ic_level <<- c(rpt_ic_level, level)
      }
      
      rpt_exp_defs <- Filter(function(d) d$comp == "Exposure", rpt_factor_defs)
      rpt_sen_defs <- Filter(function(d) d$comp == "Sensitivity", rpt_factor_defs)
      rpt_exp_vals <- as.numeric(site_scored[1, sapply(rpt_exp_defs, function(d) d$id)])
      rpt_sen_vals <- as.numeric(site_scored[1, sapply(rpt_sen_defs, function(d) d$id)])
      rpt_exp_denom <- sum(rpt_exp_vals^2, na.rm = TRUE)
      rpt_sen_denom <- sum(rpt_sen_vals^2, na.rm = TRUE)
      rpt_vuln_denom <- site_scored$EXPOSURE[1]^2 + site_scored$SENSITIVITY[1]^2
      rpt_exp_share <- 100 * rpt_safe_div(site_scored$EXPOSURE[1]^2, rpt_vuln_denom)
      rpt_sen_share <- 100 * rpt_safe_div(site_scored$SENSITIVITY[1]^2, rpt_vuln_denom)
      
      rpt_ic_add("Vulnerability", "Vulnerability", "", 100, VULN_COLOR, 0)
      rpt_ic_add("Exposure", "Exposure", "Vulnerability", rpt_exp_share, EXP_COLOR, 1)
      rpt_ic_add("Sensitivity", "Sensitivity", "Vulnerability", rpt_sen_share, SEN_COLOR, 1)
      
      rpt_place_component <- function(defs, vals, denom, comp_id, comp_share, color) {
        scored <- !sapply(defs, function(d) rpt_na_flag_for(d$id)) & !is.na(vals)
        for (i in seq_along(defs)) {
          if (!scored[i]) next
          d <- defs[[i]]
          suffix <- if (comp_id == "Exposure") " (E)" else " (S)"
          lbl <- paste0(d$label, suffix); fid <- paste0(comp_id, "/", lbl)
          fv <- rpt_safe_div(vals[i]^2, denom) * comp_share
          rpt_ic_add(fid, lbl, comp_id, fv, color, 2)
          if (d$type == "euclidean") {
            nv <- vapply(d$indicators, function(ind) site_scored[[ind$norm_col]][1], numeric(1))
            sh <- rpt_safe_div(nv^2, sum(nv^2, na.rm = TRUE))
            for (j in seq_along(d$indicators)) {
              ind <- d$indicators[[j]]
              rpt_ic_add(paste0(fid, "/", ind$label), ind$label, fid, sh[j] * fv, IND_COLOR, 3)
            }
          }
        }
      }
      rpt_place_component(rpt_exp_defs, rpt_exp_vals, rpt_exp_denom, "Exposure",    rpt_exp_share, EXP_COLOR)
      rpt_place_component(rpt_sen_defs, rpt_sen_vals, rpt_sen_denom, "Sensitivity", rpt_sen_share, SEN_COLOR)
      
      icicle_tree <- data.frame(id = rpt_ic_ids, label = rpt_ic_labels, parent = rpt_ic_parents,
                                value = rpt_ic_values, color = rpt_ic_colors, level = rpt_ic_level,
                                stringsAsFactors = FALSE)
      icicle_tree$y0 <- NA_real_; icicle_tree$y1 <- NA_real_
      root_idx <- which(icicle_tree$parent == "")
      icicle_tree$y0[root_idx] <- 0; icicle_tree$y1[root_idx] <- 100
      
      rpt_recurse_layout <- function(tree, parent_id) {
        kids <- which(tree$parent == parent_id)
        if (length(kids) == 0) return(tree)
        p_y0 <- tree$y0[tree$id == parent_id]; p_y1 <- tree$y1[tree$id == parent_id]
        total <- sum(tree$value[kids])
        cursor <- p_y0
        kids <- rev(kids)  # first-built child renders at the TOP (y increases upward)
        for (k in kids) {
          h <- if (total == 0) 0 else (tree$value[k] / total) * (p_y1 - p_y0)
          tree$y0[k] <- cursor; tree$y1[k] <- cursor + h
          cursor <- cursor + h
          tree <- rpt_recurse_layout(tree, tree$id[k])
        }
        tree
      }
      icicle_tree <- rpt_recurse_layout(icicle_tree, icicle_tree$id[root_idx])
      icicle_tree$y0 <- pmin(pmax(icicle_tree$y0, 0), 100)  # clamp float overshoot at scale boundary
      icicle_tree$y1 <- pmin(pmax(icicle_tree$y1, 0), 100)
      
      rpt_col_bounds <- list("0" = c(0.78, 1.0), "1" = c(0.52, 0.76), "2" = c(0.26, 0.50), "3" = c(0.0, 0.24))
      rpt_bounds <- do.call(rbind, rpt_col_bounds[as.character(icicle_tree$level)])
      icicle_tree$xmin <- rpt_bounds[, 1]; icicle_tree$xmax <- rpt_bounds[, 2]
      icicle_tree$box_height <- icicle_tree$y1 - icicle_tree$y0
      icicle_tree$show_label <- icicle_tree$box_height >= 3.5
      icicle_tree$label_only <- ifelse(
        icicle_tree$box_height >= 8, sprintf("%s\n%.1f%%", icicle_tree$label, icicle_tree$value),
        ifelse(icicle_tree$show_label, sprintf("%s %.0f%%", icicle_tree$label, icicle_tree$value), "")
      )
      
      p_icicle_static <- ggplot2::ggplot(icicle_tree) +
        ggplot2::geom_rect(ggplot2::aes(xmin = xmin, xmax = xmax, ymin = y0, ymax = y1, fill = color),
                           color = "white", linewidth = 0.6) +
        ggplot2::geom_text(data = subset(icicle_tree, show_label),
                           ggplot2::aes(x = (xmin + xmax) / 2, y = (y0 + y1) / 2, label = label_only),
                           size = 2.8, color = "white", lineheight = 0.85, fontface = "bold") +
        ggplot2::scale_fill_identity() +
        ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0),
                                    breaks = c(0.12, 0.38, 0.64, 0.89),
                                    labels = c("Indicators", "Factors", "Components", "Vulnerability")) +
        ggplot2::scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
        ggplot2::labs(x = NULL, y = NULL) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(size = 9, face = "bold", color = "#1D3557"),
          axis.text.y = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
          panel.grid = ggplot2::element_blank(),
          plot.background = ggplot2::element_rect(fill = "white", color = NA),
          panel.background = ggplot2::element_rect(fill = "white", color = NA)
        )
      
      icicle_chart_file <- tempfile(fileext = ".png")
      ggplot2::ggsave(icicle_chart_file, p_icicle_static, width = 8, height = 4, dpi = 150, bg = "white")
      icicle_chart_b64 <- base64enc::base64encode(icicle_chart_file)
      
      #### Companion table for the icicle chart ----
      # Static images have no hover -- this ensures every indicator/factor's
      # exact percentage is available even when its box is too small to
      # carry a visible label (e.g. Storm Surge at a few hundredths of 1%).
      icicle_table_rows <- icicle_tree[icicle_tree$level %in% c(2, 3), ]
      icicle_table_rows <- icicle_table_rows[
        order(match(icicle_table_rows$parent, c("Exposure", "Sensitivity")), -icicle_table_rows$value), ]
      icicle_table_html <- paste0(
        "<table style='width:100%;max-width:420px;border-collapse:collapse;font-size:12px;margin:10px 0;'>",
        "<thead><tr style='border-bottom:2px solid #1D3557;'>",
        "<th style='padding:4px 8px;text-align:left;'>Factor / Indicator</th>",
        "<th style='padding:4px 8px;text-align:right;'>% of Vulnerability</th></tr></thead><tbody>",
        paste0(
          "<tr><td style='padding:4px 8px;",
          ifelse(icicle_table_rows$level == 3, "padding-left:24px;color:#666;", "font-weight:600;"),
          "'>", htmltools::htmlEscape(icicle_table_rows$label), "</td>",
          "<td style='padding:4px 8px;text-align:right;'>", sprintf("%.2f%%", icicle_table_rows$value),
          "</td></tr>", collapse = ""
        ),
        "</tbody></table>"
      )
      
      
      ### Ranking Boxes HTML ----
      make_rank_box <- function(label, ranks, color) {
        rank_val <- if (is.na(ranks$vuln_rank)) "N/A" else paste0(ranks$vuln_rank, "%")
        priority_badge <- if (isTRUE(ranks$priority))
          "<span style='background:#9B2226;color:white;padding:2px 6px;border-radius:3px;font-size:10px;'>TOP PRIORITY</span>" else ""
        paste0(
          "<div style='flex:1;background:white;border-radius:8px;padding:14px 12px;border-top:4px solid ", color, ";",
          "box-shadow:0 2px 6px rgba(0,0,0,0.08);text-align:center;min-width:140px;'>",
          "<div style='font-size:11px;color:#696969;text-transform:uppercase;letter-spacing:0.05em;margin-bottom:4px;'>", label, "</div>",
          "<div style='font-size:28px;font-weight:800;color:", color, ";'>", rank_val, "</div>",
          "<div style='font-size:11px;color:#999;margin-top:2px;'>n = ", ranks$n, " sites</div>",
          priority_badge,
          "</div>"
        )
      }
      
      rank_boxes <- paste0(
        "<div style='display:flex;gap:12px;flex-wrap:wrap;margin:16px 0;'>",
        make_rank_box(paste0("Within Park (", site_park, ")"), park_ranks, "#2a7f7f"),
        make_rank_box(paste0("Within State (", site_state, ")"), state_ranks, "#457B9D"),
        make_rank_box(paste0("Within Region (", site_region, ")"), regional, "#1D3557"),
        make_rank_box("Nationally", national, "#386150"),
        "</div>"
      )
      
      ### Sub-Component Ranks Table ----
      make_rank_row <- function(label, ranks) {
        fmt <- function(v) if (is.na(v)) "--" else paste0(v, "%")
        paste0("<tr><td style='padding:6px 10px;font-weight:600;'>", label, "</td>",
               "<td style='padding:6px 10px;text-align:center;'>", fmt(ranks$vuln_rank), "</td>",
               "<td style='padding:6px 10px;text-align:center;color:#457B9D;'>", fmt(ranks$exp_rank), "</td>",
               "<td style='padding:6px 10px;text-align:center;color:#C05235;'>", fmt(ranks$sen_rank), "</td>",
               "<td style='padding:6px 10px;text-align:center;'>", ranks$n, "</td></tr>")
      }
      
      rank_table <- paste0(
        "<table style='width:100%;border-collapse:collapse;font-size:13px;margin:12px 0;'>",
        "<thead><tr style='background:#f0f5f8;border-bottom:2px solid #1D3557;'>",
        "<th style='padding:8px 10px;text-align:left;color:#1D3557;'>Scope</th>",
        "<th style='padding:8px 10px;text-align:center;color:#386150;'>Vulnerability</th>",
        "<th style='padding:8px 10px;text-align:center;color:#457B9D;'>Exposure</th>",
        "<th style='padding:8px 10px;text-align:center;color:#C05235;'>Sensitivity</th>",
        "<th style='padding:8px 10px;text-align:center;color:#666;'>N Sites</th>",
        "</tr></thead><tbody>",
        make_rank_row(paste0("Within Park (", site_park, ")"), park_ranks),
        make_rank_row(paste0("Within State (", site_state, ")"), state_ranks),
        make_rank_row(paste0("Within Region (", site_region, ")"), regional),
        make_rank_row("Nationally", national),
        "</tbody></table>"
      )
      
      ### Hazard Flags ----
      flags <- c(
        if (isTRUE(site_scored$flag_fire[1]))    "\U0001F525 Fire"    else NULL,
        if (isTRUE(site_scored$flag_flood[1]))   "\U0001F4A7 Flood"   else NULL,
        if (isTRUE(site_scored$flag_slr[1]))     "\U0001F30A Sea Level Rise"     else NULL,
        if (isTRUE(site_scored$flag_drought[1])) "\u2600\uFE0F Drought" else NULL
      )
      
      ### Hazard Flags HTML ----
      flag_colors <- c("\U0001F525 Fire" = "#C05235", "\U0001F4A7 Flood" = "#457B9D",
                       "\U0001F30A Sea Level Rise" = "#1D3557", "\u2600\uFE0F Drought" = "#8B6914")
      flags_html <- if (length(flags) > 0) {
        badges <- sapply(flags, function(f) {
          col <- ifelse(f %in% names(flag_colors), flag_colors[f], "#666")
          paste0("<span style='background:", col, ";color:white;padding:3px 10px;border-radius:4px;",
                 "font-size:12px;margin-right:4px;'>", f, "</span>")
        })
        paste0("<div style='margin:10px 0;'>", paste(badges, collapse = " "), "</div>")
      } else ""
      
      ### Raw Scores ----
      vuln_score <- round(site_scored$VULNERABILITY[1], 3)
      exp_score  <- round(site_scored$EXPOSURE[1], 3)
      sen_score  <- round(site_scored$SENSITIVITY[1], 3)
      
      ### Description Section ----
      desc_html <- if (nchar(desc_text) > 0) {
        paste0("<div style='background:#f7fafc;border-left:3px solid #2a7f7f;padding:10px 14px;",
               "border-radius:4px;margin:10px 0;font-size:13px;color:#333;line-height:1.5;'>",
               "<strong>System Description:</strong> ", htmltools::htmlEscape(desc_text), "</div>")
      } else ""
      
      ### Assemble Full HTML ----
      html_content <- paste0(
        "<!DOCTYPE html><html><head><meta charset='utf-8'>",
        "<title>Vulnerability Report - ", htmltools::htmlEscape(site_meta$park_name[1]), "</title>",
        "<style>",
        "body{font-family:Arial,Helvetica,sans-serif;max-width:850px;margin:0 auto;padding:30px 40px;color:#333;line-height:1.5;}",
        "h1{color:#1D3557;font-size:24px;margin:0 0 4px 0;}",
        "h2{color:#1D3557;font-size:17px;border-bottom:2px solid #2a7f7f;padding-bottom:4px;margin-top:28px;}",
        ".subtitle{color:#386150;font-size:16px;font-weight:600;margin:0 0 2px 0;}",
        ".meta{color:#696969;font-size:13px;margin:0 0 8px 0;}",
        ".accent-line{height:3px;background:linear-gradient(90deg,#1D3557,#2a7f7f,#A8DADC);border-radius:2px;margin:12px 0 20px 0;}",
        "table{border-collapse:collapse;width:100%;}",
        "thead tr{border-bottom:2px solid #1D3557;}",
        "tbody tr{border-bottom:1px solid #eee;}",
        "tbody tr:hover{background:#f8fbfd;}",
        ".info-table td{padding:5px 10px;font-size:13px;}",
        ".info-table td:first-child{font-weight:600;color:#1D3557;width:140px;}",
        ".score-row{display:flex;gap:16px;margin:12px 0;flex-wrap:wrap;}",
        ".score-box{background:#f0f5f8;border-radius:6px;padding:10px 16px;text-align:center;flex:1;min-width:120px;}",
        ".score-box .label{font-size:11px;color:#696969;text-transform:uppercase;letter-spacing:0.05em;}",
        ".score-box .value{font-size:22px;font-weight:700;}",
        ".footer{margin-top:30px;padding-top:12px;border-top:1px solid #ddd;text-align:center;font-size:11px;color:#999;}",
        "@media print{body{padding:20px;}}",
        "</style></head><body>",
        
        # Header
        "<h1>Water Supply Vulnerability Report</h1>",
        "<p class='subtitle'>", htmltools::htmlEscape(site_meta$park_name[1]), "</p>",
        "<p class='meta'>", htmltools::htmlEscape(site_meta$water_system_name[1]),
        " &nbsp;|&nbsp; ", site_id,
        " &nbsp;|&nbsp; Generated ", format(Sys.Date(), "%B %d, %Y"), "</p>",
        "<div class='accent-line'></div>",
        
        # Site Info
        "<h2>Site Information</h2>",
        "<table class='info-table'>",
        "<tr><td>Water System</td><td>", htmltools::htmlEscape(site_meta$water_system_name[1]), "</td></tr>",
        "<tr><td>Park Unit</td><td>", htmltools::htmlEscape(site_park), "</td></tr>",
        "<tr><td>Park Name</td><td>", htmltools::htmlEscape(site_meta$park_name[1]), "</td></tr>",
        "<tr><td>State</td><td>", htmltools::htmlEscape(site_state), "</td></tr>",
        "<tr><td>Region</td><td>", htmltools::htmlEscape(site_region), "</td></tr>",
        "<tr><td>Source Type</td><td>", htmltools::htmlEscape(sys_type), "</td></tr>",
        "</table>",
        desc_html,
        flags_html,
        
        # # Raw scores
        # "<div class='score-row'>",
        # "<div class='score-box'><div class='label'>Vulnerability</div><div class='value' style='color:#386150;'>", vuln_score, "</div></div>",
        # "<div class='score-box'><div class='label'>Exposure</div><div class='value' style='color:#457B9D;'>", exp_score, "</div></div>",
        # "<div class='score-box'><div class='label'>Sensitivity</div><div class='value' style='color:#C05235;'>", sen_score, "</div></div>",
        # "</div>",
        
        # Priority Rankings
        "<h2>Priority Rankings <em>(higher percentile = higher relative risk)</em></h2>",
        "<p style='font-size:12px;color:#696969;margin-top:-4px;'>Vulnerability percentile rank at four geographic scopes. A rank of 90 means the supply scores higher than 90% of the comparison group.</p>",
        rank_boxes,
        rank_table,
        
        # Component Contribution
        "<h2>Component Contribution</h2>",
        "<p style='font-size:12px;color:#696969;margin-top:-4px;'>Each factor's share of this site's Exposure or Sensitivity score, calculated nationally. Green = Exposure, Purple = Sensitivity.</p>",
        "<img src='data:image/png;base64,", contrib_chart_b64, "' style='width:100%;max-width:750px;display:block;margin:12px auto;' />",
        
        # All Levels Contribution
        "<h2>All Levels Contribution</h2>",
        "<p style='font-size:12px;color:#696969;margin-top:-4px;'>Nested view from raw indicators through factors and components to the overall Vulnerability score.</p>",
        "<img src='data:image/png;base64,", icicle_chart_b64, "' style='width:100%;max-width:750px;display:block;margin:12px auto;' />",
        icicle_table_html,
        
        # Footer
        "<div class='footer'>",
        "NPS Water Supply Vulnerability Assessment Tool<br>",
        "Scores calculated using the Michalak et al. (2026) Euclidean distance framework<br>",
        "Colorado State University Geospatial Centroid &nbsp;|&nbsp; ", format(Sys.Date(), "%B %Y"),
        "</div>",
        
        "</body></html>"
      )
      
      ### Write HTML File ----
      report_path <- file.path(tempdir(), paste0("report_", site_id, ".html"))
      writeLines(html_content, report_path)
      
      removeNotification(notif_id)
      
      output$download_report <- downloadHandler(
        filename = function() paste0("vulnerability_report_", site_id, ".html"),
        content  = function(file) file.copy(report_path, file)
      )
      
      showModal(modalDialog(
        title = paste0("Report Ready: ", site_meta$park_name[1], " \u2013 ", site_id),
        tags$p("Your vulnerability report has been generated. Open the downloaded file in a browser and use Print > Save as PDF if you need a PDF.",
               style = "font-size:13px; color:#333;"),
        downloadButton("download_report", "Download Report",
                       style = "background:#2a7f7f; color:white; border:none; border-radius:4px; 
                                padding:8px 16px; font-size:13px; font-weight:600;"),
        size      = "m",
        easyClose = TRUE,
        footer    = modalButton("Close")
      ))
      
    }, error = function(e) {
      removeNotification(notif_id)
      showNotification(
        paste0("Report generation failed: ", e$message),
        type = "error", duration = 8
      )
    })
  })
  
  # # ── Clicked site reactive ───────────────────────────────────────────────
  # clicked_site <- reactiveVal(NULL)
  # 
  # observeEvent(input$map_marker_click, {
  #   click <- input$map_marker_click
  #   if (!is.null(click$id)) {
  #     clicked_site(click$id)
  #   }
  # })
  # 
  # # ── Site selected flag for conditionalPanel ──────────────────────────────
  # output$site_selected <- reactive({ !is.null(clicked_site()) })
  # outputOptions(output, "site_selected", suspendWhenHidden = FALSE)
  # 
  # # ── Individual site score breakdown chart ───────────────────────────────
  # output$site_chart <- renderPlotly({
  #   req(clicked_site())
  #   
  #   df_all  <- as.data.frame(filtered_data())
  #   site_id <- clicked_site()
  #   req(site_id %in% df_all$wsd_source_id)
  #   
  #   site_row <- df_all[df_all$wsd_source_id == site_id, ]
  #   
  #   fac_cols <- names(factor_labels)
  #   fac_cols <- fac_cols[fac_cols %in% names(df_all)]
  #   
  #   # Regional means
  #   means <- colMeans(df_all[, fac_cols, drop = FALSE], na.rm = TRUE)
  #   # Site values
  #   site_vals <- as.numeric(site_row[1, fac_cols])
  #   
  #   bar_order <- factor_labels[fac_cols]
  #   chart_df <- data.frame(
  #     factor    = factor(bar_order, levels = bar_order),
  #     regional  = round(means, 3),
  #     site      = round(site_vals, 3),
  #     component = ifelse(grepl("Exp", bar_order), "Exposure", "Sensitivity")
  #   )
  #   
  #   site_title <- paste0(site_row$park_unit[1], " \u2013 ", site_row$wsd_source_id[1])
  #   
  #   plot_ly(chart_df, x = ~factor) %>%
  #     add_bars(y = ~regional, name = "Regional Mean",
  #              marker = list(color = "rgba(180,180,180,0.5)"),
  #              hovertemplate = "%{x}<br>Regional mean: %{y:.3f}<extra></extra>") %>%
  #     add_bars(y = ~site, name = "Selected Site",
  #              marker = list(color = ifelse(chart_df$component == "Exposure",
  #                                           "#457B9D", "#C05235")),
  #              showlegend = FALSE,
  #              hovertemplate = "%{x}<br>Site score: %{y:.3f}<extra></extra>") %>%
  #     layout(
  #       barmode = "group",
  #       title = list(text = site_title, font = list(size = 13, color = "#1D3557"),
  #                    x = 0, xanchor = "left"),
  #       xaxis = list(title = "", tickfont = list(size = 10),
  #                    categoryorder = "array", categoryarray = bar_order),
  #       yaxis = list(title = "Score (0-1)", range = c(0, 1),
  #                    tickfont = list(size = 10)),
  #       margin = list(t = 35, b = 60),
  #       legend = list(orientation = "h", y = -0.25),
  #       plot_bgcolor  = "white",
  #       paper_bgcolor = "white"
  #     )
  # })
  
  ## Export Current Map ----
  observeEvent(input$export_map_btn, {
    notif_id <- showNotification(
      "Generating PDF export...", duration = NULL, closeButton = FALSE, type = "message"
    )
    tryCatch({
      col       <- active_column()
      df_sf     <- filtered_data_display()
      df        <- as.data.frame(df_sf)
      n_sites   <- nrow(df)
      
      # ---- Current map viewport bounds ----
      bounds    <- input$map_bounds
      xlim_use  <- if (!is.null(bounds)) c(bounds$west, bounds$east)  else c(-170, -65)
      ylim_use  <- if (!is.null(bounds)) c(bounds$south, bounds$north) else c(17, 72)
      
      # ---- Metadata strings ----
      if (input$view_mode == "score") {
        map_title    <- input$score_view
        map_subtitle <- if (input$score_metric == "rank") "Percentile Rank" else "Raw Score"
        comp_color   <- if (grepl("Exposure", map_title)) "#457B9D"
        else if (grepl("Sensitivity", map_title)) "#C05235" else "#386150"
      } else {
        req(input$component, input$factor, input$indicator)
        cfg          <- indicator_config[[input$component]][[input$factor]][[input$indicator]]
        req(!is.null(cfg))
        map_title    <- paste0(input$component, " Indicator: ", input$indicator)
        map_subtitle <- paste0("Factor: ", input$factor, " | Raw: ", cfg$raw_label)
        comp_color   <- if (input$component == "Exposure") "#457B9D" else "#C05235"
      }
      
      filter_parts <- c(
        if (length(input$filter_region) > 0) paste0("Region: ", paste(input$filter_region, collapse=", ")),
        if (length(input$filter_state)  > 0) paste0("State: ",  paste(input$filter_state,  collapse=", ")),
        if (length(input$filter_park)   > 0) paste0("Park: ",   paste(input$filter_park,   collapse=", ")),
        if (length(input$filter_source_type) > 0) paste0("Source Type: ", paste(input$filter_source_type, collapse=", ")),
        if (isTRUE(input$filter_priority)) "Priority: High Priority Only"
      )
      filter_str  <- if (length(filter_parts) > 0) paste(filter_parts, collapse = " | ") else "None (national)"
      scope_label <- if (length(input$filter_state)  > 0) paste("State:", paste(input$filter_state,  collapse=", "))
      else if (length(input$filter_region) > 0) paste("Region:", paste(input$filter_region, collapse=", "))
      else "National (CONUS)"
      
      # ---- Color scale ----
      vals       <- suppressWarnings(as.numeric(df[[col]]))
      val_finite <- vals[is.finite(vals)]
      
      invert_raw_cols <- c("exp_runoff_change","exp_precip_change","exp_drought_change",
                           "sen_runoff_trend","sen_precip_trend")
      is_inverted <- (input$view_mode == "indicator") && col %in% invert_raw_cols
      
      pal_colors <- if (is_inverted) rev(c("#FFF3D6","#F0C75E","#DD8844","#C05235","#9B2226"))
      else                  c("#FFF3D6","#F0C75E","#DD8844","#C05235","#9B2226")
      
      is_rank    <- (input$view_mode == "score" && isTRUE(input$score_metric == "rank"))
      clr_range  <- if (is_rank) c(0, 100) else range(val_finite, na.rm=TRUE)
      if (diff(clr_range) == 0) clr_range <- c(clr_range[1] - 0.001, clr_range[2] + 0.001)
      
      df_sf$plot_val <- vals
      legend_lbl  <- if (is_rank) paste0(map_title, "\n(Percentile Rank)") else map_title
      
      # ---- Basemap from maps package ----
      world_sf  <- tryCatch({
        w <- maps::map("world", fill = TRUE, plot = FALSE, resolution = 0)
        sf::st_as_sf(w) %>% sf::st_set_crs(4326)
      }, error = function(e) NULL)
      
      states_sf <- tryCatch({
        s <- maps::map("state", fill = TRUE, plot = FALSE)
        sf::st_as_sf(s) %>% sf::st_set_crs(4326)
      }, error = function(e) NULL)
      
      # ---- Build ggplot2 map ----
      p_map <- ggplot2::ggplot()
      
      if (!is.null(world_sf)) {
        p_map <- p_map +
          ggplot2::geom_sf(data = world_sf, fill = "#edf2f6", color = "#c4cfd7",
                           linewidth = 0.2, inherit.aes = FALSE)
      }
      if (!is.null(states_sf)) {
        p_map <- p_map +
          ggplot2::geom_sf(data = states_sf, fill = NA, color = "#b0bcc5",
                           linewidth = 0.25, inherit.aes = FALSE)
      }
      
      p_map <- p_map +
        ggplot2::geom_sf(data = park_boundaries, fill = "#A8DADC", alpha = 0.2,
                         color = "#1D3557", linewidth = 0.25, inherit.aes = FALSE) +
        ggplot2::geom_sf(data = df_sf[is.na(df_sf$plot_val), ],
                         color = "lightgrey", size = 1.2, alpha = 0.6, inherit.aes = FALSE) +
        ggplot2::geom_sf(data = df_sf[!is.na(df_sf$plot_val), ],
                         ggplot2::aes(color = plot_val, size = plot_val),
                         alpha = 0.8, inherit.aes = FALSE) +
        ggplot2::scale_color_gradientn(
          colors = pal_colors, limits = clr_range, name = legend_lbl, na.value = "lightgrey",
          guide = ggplot2::guide_colorbar(barwidth = 0.8, barheight = 8,
                                          title.position = "top", title.hjust = 0.5)
        ) +
        ggplot2::scale_size_continuous(range = c(1, 4), guide = "none") +
        ggplot2::coord_sf(xlim = xlim_use, ylim = ylim_use, expand = FALSE) +
        ggplot2::labs(
          title    = map_title,
          subtitle = paste0(
            map_subtitle, "  |  Scope: ", scope_label, "  |  Sites: ", n_sites,
            if (filter_str != "None (national)") paste0("\nFilters: ", filter_str) else "",
            "\nData: ", DATA_LAST_UPDATED, "  |  Exported: ", format(Sys.Date(), "%B %d, %Y")
          ),
          caption  = "NPS Water Supply Vulnerability Assessment Tool | Colorado State University Geospatial Centroid"
        ) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
          plot.title       = ggplot2::element_text(color = comp_color, face = "bold", size = 14),
          plot.subtitle    = ggplot2::element_text(color = "#555", size = 8.5, lineheight = 1.3),
          plot.caption     = ggplot2::element_text(color = "#888", size = 8, hjust = 0.5),
          legend.title     = ggplot2::element_text(size = 9, face = "bold"),
          legend.text      = ggplot2::element_text(size = 8),
          legend.position  = "right",
          panel.grid.major = ggplot2::element_line(color = "#ddd", linewidth = 0.3),
          panel.border     = ggplot2::element_rect(color = "#bbb", fill = NA, linewidth = 0.4),
          plot.background  = ggplot2::element_rect(fill = "white", color = NA),
          panel.background = ggplot2::element_rect(fill = "#f5f9fb", color = NA)
        )
      
      # ---- Save as PDF ----
      pdf_file <- tempfile(fileext = ".pdf")
      ggplot2::ggsave(pdf_file, p_map, width = 11, height = 8.5, device = "pdf")
      
      removeNotification(notif_id)
      
      output$download_map_export <- downloadHandler(
        filename = function() paste0("nps_wsva_map_", format(Sys.Date(), "%Y%m%d"), ".pdf"),
        content  = function(file) file.copy(pdf_file, file)
      )
      
      showModal(modalDialog(
        title = "Map Export Ready",
        tags$p("Your map has been exported as a PDF matching your current map view.",
               style = "font-size:13px; color:#333;"),
        tags$p(
          style = "font-size:11px; color:#888; margin-top:4px;",
          paste0("View: ", map_title, " | Scope: ", scope_label, " | Sites: ", n_sites)
        ),
        downloadButton("download_map_export", "Download PDF",
                       style = "background:#2a7f7f; color:white; border:none; border-radius:4px;
                                padding:8px 16px; font-size:13px; font-weight:600;"),
        size      = "m",
        easyClose = TRUE,
        footer    = modalButton("Close")
      ))
      
    }, error = function(e) {
      removeNotification(notif_id)
      showNotification(paste0("Export failed: ", e$message), type = "error", duration = 8)
    })
  })
  
  
  ## Coordinate Lookup Table ----
  # This never changes, so no reactive needed — avoids triggering any chain
  coord_lookup <- as.data.frame(st_coordinates(combined_data))
  coord_lookup$wsd_source_id <- combined_data$wsd_source_id
  names(coord_lookup) <- c("lng", "lat", "wsd_source_id")
  
  # Track the wsd_source_id vector in the same row order as the DT receives
  
  dt_ids <- reactiveVal(character(0))
  
  ## Data Table Output ----
  output$data_table <- renderDT(server = TRUE, {
    
    req(map_bounds_debounced())
    bounds <- map_bounds_debounced()
    
    base_data <- filtered_data_display()
    coords    <- st_coordinates(base_data)
    in_view   <- coords[, 1] >= bounds$west  & coords[, 1] <= bounds$east &
      coords[, 2] >= bounds$south & coords[, 2] <= bounds$north
    
    df <- as.data.frame(base_data[in_view, ]) %>%
      select(wsd_source_id, water_system_name,
             park_unit, park_name, state, region,
             source_type,
             VULNERABILITY, VULNERABILITY_rank, EXPOSURE, EXPOSURE_rank,
             SENSITIVITY, SENSITIVITY_rank,
             vulnerability_quartile,
             any_of(names(factor_labels)),
             starts_with("norm_"),
             starts_with("exp_"), starts_with("sen_"),
             priority_group, flag_fire, flag_flood, flag_slr, flag_drought) %>%
      mutate(
        across(where(is.numeric), ~round(., 3)),
        # Build combined priority flags column
        priority_flags = paste0(
          ifelse(!is.na(priority_group) & priority_group, "<span style='background:#9B2226;color:white;padding:1px 5px;border-radius:3px;font-size:10px;margin-right:2px;'>HIGH PRIORITY</span>", ""),
          ifelse(!is.na(flag_fire)    & flag_fire,    "<span style='background:#C05235;color:white;padding:1px 5px;border-radius:3px;font-size:10px;margin-right:2px;'><span aria-hidden='true'>&#x1F525;</span> Fire</span>", ""),
          ifelse(!is.na(flag_flood)   & flag_flood,   "<span style='background:#457B9D;color:white;padding:1px 5px;border-radius:3px;font-size:10px;margin-right:2px;'><span aria-hidden='true'>&#x1F4A7;</span> Flood</span>", ""),
          ifelse(!is.na(flag_slr)     & flag_slr,     "<span style='background:#1D3557;color:white;padding:1px 5px;border-radius:3px;font-size:10px;margin-right:2px;'><span aria-hidden='true'>&#x1F30A;</span> Sea Level Rise</span>", ""),
          ifelse(!is.na(flag_drought) & flag_drought,  "<span style='background:#8B6914;color:white;padding:1px 5px;border-radius:3px;font-size:10px;margin-right:2px;'><span aria-hidden='true'>&#x2600;&#xFE0F;</span> Drought</span>", "")
        )
      ) %>%
      select(-priority_group, -flag_fire, -flag_flood, -flag_slr, -flag_drought)
    
    #vuln_max <- max(df$VULNERABILITY, na.rm = TRUE)
    
    # Move priority_flags to be right after the score columns, before indicator columns
    df <- df %>%
      relocate(priority_flags, .before = VULNERABILITY)
    
    # Store the wsd_source_id vector in the SAME row order DT will receive
    # (server=TRUE means DT row indices map to this ordering)
    dt_ids(df$wsd_source_id)
    
    datatable(df,
              caption = "Water supply vulnerability data for sites currently visible on the map. Sortable, filterable, and exportable as CSV or Excel using the buttons above the table.",
              selection  = "single",
              rownames   = FALSE,
              escape     = FALSE,
              extensions = "Buttons",
              options    = list(
                paging         = TRUE,
                pageLength = 50,
                scrollY        = "400px",
                scrollX        = TRUE,
                scrollCollapse = TRUE,
                order          = list(list(which(names(df) == "VULNERABILITY") - 1, "desc")),                dom            = "Bfrtip",
                buttons        = list("csv", "excel"),
                columnDefs     = list(
                  list(className = "dt-center", targets = "_all"),
                  list(width = "120px", targets = 0:5),   # ID/name cols
                  list(width = "80px",  targets = 6:12),  # score cols
                  list(width = "300px", targets = 13)     # flags col
                )
              )) #%>% 
    # formatStyle("VULNERABILITY",
    #             background         = styleColorBar(c(0, vuln_max), "#C05235"),
    #             backgroundSize     = "100% 80%",
    #             backgroundRepeat   = "no-repeat",
    #             backgroundPosition = "center")
  })
  
  ## Popup Builder Function ----
  create_popup <- function(view_mode, score_label, component, factor_name,
                           indicator_name, raw_value, raw_label, col_value,
                           vuln_rank, exp_rank, sen_rank,
                           priority, flag_fire, flag_flood, flag_slr, flag_drought,
                           wsd_source_id, park_unit, park_name, water_system_name,
                           source_type, description, state) {
    
    top_section <- if (view_mode == "score") {
      paste0("<b style='color:#386150; font-size:14px;'>", score_label, "</b><br>",
             "<b>Value:</b> <span style='font-size:14px; font-weight:bold;'>",
             round(col_value, 3), "</span>")
    } else {
      raw_row <- if (!is.na(raw_value)) {
        paste0(
          "<div style='margin-top:6px; padding:8px 10px; background:#edf3f8; border-left:3px solid #1D3557; border-radius:4px;'>",
          "<span style='font-size:10px; color:#666; text-transform:uppercase; letter-spacing:0.05em;'>", raw_label, "</span><br>",
          "<span style='font-size:18px; font-weight:800; color:#1D3557;'>", round(raw_value, 3), "</span>",
          "</div>"
        )
      } else {
        paste0(
          "<div style='margin-top:6px; padding:8px 10px; background:#f5f5f5; border-left:3px solid #ccc; border-radius:4px;'>",
          "<span style='font-size:10px; color:#666; text-transform:uppercase; letter-spacing:0.05em;'>", raw_label, "</span><br>",
          "<span style='font-size:13px; color:#999; font-style:italic;'>No data</span>",
          "</div>"
        )
      }
      paste0("<b style='color:#1D3557; font-size:14px;'>", indicator_name, "</b><br>",
             "<span style='font-size:11px; color:#666;'>", component, " \u203a ", factor_name,
             "</span>", raw_row)
    }
    
    score_section <- if (view_mode == "score") {
      paste0(
        "<hr style='margin:6px 0; border-color:#ddd;'>",
        "<table style='font-size:11px; width:100%;'><tr>",
        "<td><b>Vulnerability</b></td><td><b>Exposure</b></td><td><b>Sensitivity</b></td>",
        "</tr><tr>",
        "<td style='color:#386150; font-weight:bold;'>", round(vuln_rank, 1), "%</td>",
        "<td style='color:#457B9D; font-weight:bold;'>", round(exp_rank, 1), "%</td>",
        "<td style='color:#C05235; font-weight:bold;'>", round(sen_rank, 1), "%</td>",
        "</tr></table>",
        "<span style='font-size:9px; color:#999;'>Percentile rank (higher = more vulnerable)</span>"
      )
    } else ""
    
    flags <- c(
      if (isTRUE(priority))    "<span style='background:#9B2226;color:white;padding:1px 5px;border-radius:3px;font-size:10px;'>HIGH PRIORITY</span>" else NULL,
      if (isTRUE(flag_fire))   "<span style='background:#C05235;color:white;padding:1px 5px;border-radius:3px;font-size:10px;'><span aria-hidden='true'>&#x1F525;</span> Fire</span>"   else NULL,
      if (isTRUE(flag_flood))  "<span style='background:#457B9D;color:white;padding:1px 5px;border-radius:3px;font-size:10px;'><span aria-hidden='true'>&#x1F4A7;</span> Flood</span>"  else NULL,
      if (isTRUE(flag_slr))    "<span style='background:#1D3557;color:white;padding:1px 5px;border-radius:3px;font-size:10px;'><span aria-hidden='true'>&#x1F30A;</span> Sea Level Rise</span>"    else NULL,
      if (isTRUE(flag_drought)) "<span style='background:#8B6914;color:white;padding:1px 5px;border-radius:3px;font-size:10px;'><span aria-hidden='true'>&#x2600;&#xFE0F;</span> Drought</span>" else NULL
    )
    flag_section <- if (length(flags) > 0)
      paste0("<hr style='margin:6px 0; border-color:#ddd;'>", paste(flags, collapse=" "))
    else ""
    
    site_section <- paste0(
      "<hr style='margin:6px 0; border-color:#ddd;'>",
      "<b>Water System:</b> ",    water_system_name, "<br>",
      "<b>Water Supply ID:</b> ", wsd_source_id, "<br>",
      "<b>Park Unit:</b> ",       park_unit,      "<br>",
      "<b>Park Name:</b> ",       park_name,      "<br>",
      "<b>Source Type:</b> ",     ifelse(is.na(source_type), "<span style='color:#999;'>N/A</span>", source_type), "<br>",
      "<b>State:</b> ",           state,           "<br>",
      if (!is.na(description) && nchar(trimws(description)) > 0)
        paste0("<a href='#' onclick=\"Shiny.setInputValue('show_description_btn', '",
               wsd_source_id, "', {priority:'event'}); return false;\" ",
               "style='font-size:11px; color:#457B9D;'><span aria-hidden='true'>&#x1F4C4;</span> View Full Description</a><br>")
      else paste0("<span style='font-size:11px; color:#999; font-style:italic;'>",
                  "No water supply description available.</span><br>"),
      "<hr style='margin:6px 0; border-color:#ddd;'>",
      "<button onclick=\"Shiny.setInputValue('show_chart_btn', '", wsd_source_id,
      "', {priority: 'event'});\" ",
      "style='background:#1D3557;color:white;border:none;border-radius:4px;",
      "padding:5px 10px;font-size:11px;cursor:pointer;width:100%;margin-bottom:4px;'>",
      "<span aria-hidden='true'>&#x1F4CA;</span> View Score Breakdown</button>",
      "<button onclick=\"Shiny.setInputValue('generate_report_btn', '", wsd_source_id,
      "', {priority: 'event'});\" ",
      "style='background:#2a7f7f;color:white;border:none;border-radius:4px;",
      "padding:5px 10px;font-size:11px;cursor:pointer;width:100%;'>",
      "<span aria-hidden='true'>&#x1F4C4;</span> Generate Report</button>")
    
    paste0("<div style='font-family:Arial; font-size:12px; min-width:210px;'>",
           top_section, score_section, flag_section, site_section, "</div>")
  }
}

shinyApp(ui = ui, server = server)