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

############# DATA ############

# Source the vulnerability index calculation function
source("calc_vulnerability_index.R")

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

# Pre-compute filter options
all_regions <- sort(unique(na.omit(combined_data$region)))
all_states  <- sort(unique(na.omit(combined_data$state)))
all_parks   <- sort(unique(na.omit(combined_data$park_unit)))

# ---------------------------------------------------------------------------
# Indicator config: Component -> Factor -> Indicator
# col = normalized column name (for map display)
# raw_col = raw indicator column name (from final_indicators)
# ---------------------------------------------------------------------------
indicator_config <- list(
  
  "Exposure" = list(
    "Runoff" = list(
      "Change in Runoff" = list(
        col = "norm_exp_runoff_change", raw_col = "exp_runoff_change",
        raw_label = "% change mean annual runoff (p10)",
        description = "Projected % change in mean annual runoff (p10)"
      ),
      "Runoff Model Agreement" = list(
        col = "norm_exp_runoff_model_agree", raw_col = "exp_runoff_model_agree",
        raw_label = "% GCMs predicting decrease",
        description = "% of GCMs predicting a decrease in runoff"
      )
    ),
    "Precipitation" = list(
      "Change in Precipitation" = list(
        col = "norm_exp_precip_change", raw_col = "exp_precip_change",
        raw_label = "% change mean annual precip (p10)",
        description = "Projected % change in mean annual precipitation (p10)"
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
      "Drough Model Agreement" = list(
        col = "norm_exp_drought_model_agree", raw_col = "exp_drought_model_agree",
        raw_label = "% models predicting decrease",
        description = "% of climate models predicting a decrease in SPEI"
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
        raw_label = "% change fire probability (p90)",
        description = "Projected % change in probability of wildfire (p90)"
      )
    )
  ),
  
  "Sensitivity" = list(
    "Demand" = list(
      "Historic Visitation Trend" = list(
        col = "norm_sen_visitation_trend", raw_col = "sen_visitation_trend",
        raw_label = "Scaled visitation trend",
        description = "Historic trend in park visitation (scaled)"
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
      "Historic Runoff Trend" = list(
        col = "norm_sen_runoff_trend", raw_col = "sen_runoff_trend",
        raw_label = "Mann-Kendall slope (30-yr runoff)",
        description = "30-year historic trend in runoff (Mann-Kendall slope)"
      )
    ),
    "Precipitation" = list(
      "Historic Precipitation Trend" = list(
        col = "norm_sen_precip_trend", raw_col = "sen_precip_trend",
        raw_label = "Mann-Kendall slope (30-yr precip)",
        description = "30-year historic trend in precipitation (Mann-Kendall slope)"
      )
    )
  )
)

score_views <- list(
  "Vulnerability Score" = "VULNERABILITY",
  "Exposure Score"      = "EXPOSURE",
  "Sensitivity Score"   = "SENSITIVITY"
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
  "factor_exp_slr"          = "SLR\n(Exp)",
  "factor_exp_wildfire"     = "Wildfire\n(Exp)",
  "factor_sen_demand"       = "Demand\n(Sen)",
  #"factor_sen_water_supply" = "Supply\n(Sen)",
  "factor_sen_wildfire"     = "Wildfire\n(Sen)",
  "factor_sen_flood"        = "Flood\n(Sen)",
  "factor_sen_slr"          = "SLR\n(Sen)",
  "factor_sen_runoff"       = "Runoff\n(Sen)",
  "factor_sen_precip"       = "Precip\n(Sen)"
)

###################### UI ###############################

ui <- fluidPage(
  tags$head(
    useShinyjs(),
    tags$style(HTML("
      body { background-color: #EDF4F7; font-family: 'Arial','Helvetica',sans-serif; }

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
        font-weight:700; color:#1D3557; font-size:1.15rem;
      }
      .info-box .badge-demo {
        display:inline-block; color:white; padding:2px 7px;
        border-radius:3px; font-size:0.75rem; margin-right:4px; vertical-align:middle;
      }

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
        letter-spacing: 0.08em; color: #888; margin-bottom: 10px;
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
      .section-panel h4 { color:#1D3557; font-weight:600; margin-top:0; }
      
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
      
    "))
  ),
  
  div(class = "main-header",
      div(class = "main-header-accent"),
      div(class = "main-header-body",
          h1("NATIONAL PARK SERVICE",
             style = "font-size:1rem; font-weight:700; letter-spacing:0.25em;
                    color:#A8DADC; margin-bottom:8px;"),
          h1("Water Supply Vulnerability Assessment Tool",
             style = "font-size:2rem; font-weight:800; letter-spacing:0.01em;
                    line-height:1.2;"),
          tags$div(style = "width:50px; height:3px; background:#2a7f7f;
                          margin:12px auto 10px auto; border-radius:2px;"),
          em(HTML("&#128679; This app is under active development. All results are preliminary and should not be shared out at this time."),
             style = "display:block; text-align:center; font-size:1.5rem; font-weight: 600;
                    color:rgba(255, 100, 0, 1);")
      )
  ),
  
  # ── Controls ──
  div(class = "control-panel",
      
      # Collapsible how-to box
      tags$details(class = "info-box", style = "cursor:pointer;",
                   tags$summary(style = "font-size:1.5rem; font-weight:600; color:#1D3557; list-style:none;",
                                icon("info-circle"), " How to Use This Tool",
                                icon("chevron-down", style = "float:right; font-size:1rem; margin-top:5px;")
                   ),
                   div(style = "margin-top:12px;",
                       p(HTML("This tool visualizes water supply vulnerability across the National Park System
          using a framework modeled after <a href='https://conbio.onlinelibrary.wiley.com/doi/10.1111/con4.70020' target='_blank' style='color:#457B9D;'>Michalak et al. 2026</a>. Each water supply is scored
          on two components &mdash; <strong style='color:#457B9D;'>Exposure</strong> (projected climate threats)
          and <strong style='color:#C05235;'>Sensitivity</strong> (current susceptibility) &mdash;
          which combine into an overall <strong style='color:#386150;'>Vulnerability Score</strong>.
          Larger, darker circles indicate higher vulnerability.")),
                       div(style = "background:white; border-radius:8px; padding:14px 18px; margin:10px 0 6px 0; border:1px solid #dce8ef;",
                           p(style = "font-size:1.2rem; color:#555; margin-bottom:10px;",
                             HTML("Scores are computed in four steps using a Euclidean distance framework:")),
                           div(style = "display:flex; align-items:center; justify-content:center; flex-wrap:wrap; gap:6px; margin-bottom:12px; text-align:center;",
                               div(style = "background:#f5f8fa; border:1px solid #dce8ef; border-radius:6px; padding:8px 12px;",
                                   div(style = "font-size:1.2rem; color:#888; margin-bottom:2px;", "Step 1"),
                                   HTML("<b>Indicators</b> (0&ndash;1)<br><span style='font-size:1.2rem; color:#555;'>normalized raw values</span>")),
                               div(style = "font-size:1.4rem; color:#aaa;", HTML("&rarr;")),
                               div(style = "background:#f5f8fa; border:1px solid #dce8ef; border-radius:6px; padding:8px 12px;",
                                   div(style = "font-size:1.2rem; color:#888; margin-bottom:2px;", "Step 2"),
                                   HTML("<b>Factors</b><br><span style='font-size:1.2rem; color:#555;'>&radic;<span style='text-decoration:overline;'>&thinsp;&Sigma; indicator<sup>2</sup>&thinsp;</span></span><br>
                     <span style='font-size:1.2rem; color:#888; font-style:italic;'>then normalized to 0&ndash;1</span>")),
                               div(style = "font-size:1.4rem; color:#aaa;", HTML("&rarr;")),
                               div(style = "background:#edf3f8; border:1px solid #c1d5e0; border-radius:6px; padding:8px 12px;",
                                   div(style = "font-size:1.2rem; color:#888; margin-bottom:2px;", "Step 3"),
                                   HTML("<b style='color:#457B9D;'>Exposure</b> &amp; <b style='color:#C05235;'>Sensitivity</b><br>
                     <span style='font-size:1.2rem; color:#555;'>&radic;<span style='text-decoration:overline;'>&thinsp;&Sigma; factor<sup>2</sup>&thinsp;</span></span>")),
                               div(style = "font-size:1.4rem; color:#aaa;", HTML("&rarr;")),
                               div(style = "background:#e8f0e9; border:1px solid #b2ccb5; border-radius:6px; padding:8px 12px;",
                                   div(style = "font-size:1.2rem; color:#888; margin-bottom:2px;", "Step 4"),
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
                               p(style = "font-size:1.2rem; color:#5a4a1a; margin:0;",
                                 HTML("<b>&#x26A0;&#xFE0F; Scores are relative, not absolute.</b>
                A score only has meaning within the group it is compared against.
                The same water supply may rank differently depending on whether it is evaluated
                across CONUS, a region, a state, or a subset of parks. Use filters intentionally to ensure comparisons are meaningful.")))
                       ),
                       div(class = "info-section-title", icon("exclamation-triangle"), " Priority & Hazard Flags"),
                       p(HTML("Water supplies in the <strong>top 25% of the current comparison group</strong>
          for overall vulnerability are flagged as
          <span class='badge-demo' style='background:#9B2226;'>HIGH PRIORITY</span>.
          This threshold is recalculated relative to the active filter (CONUS, region, or state subset),
          so priority designations reflect the selected comparison group, not a fixed national threshold.
          Popups also display hazard-specific flags:
          <span class='badge-demo' style='background:#C05235;'>&#x1F525; Fire</span>
          <span class='badge-demo' style='background:#457B9D;'>&#x1F4A7; Flood</span>
          <span class='badge-demo' style='background:#1D3557;'>&#x1F30A; SLR</span>
          <span class='badge-demo' style='background:#8B6914;'>&#x2600;&#xFE0F; Drought</span>"))
                   )
      ),
      
      # ── View mode toggle + options ──
      div(style = "margin-top:6px;",
          
          # Toggle buttons (JS-driven, sets hidden input)
          tags$div(class = "view-toggle-wrap",
                   tags$button(id = "btn_score", class = "view-toggle-btn active",
                               onclick = "setViewMode('score')", "Score View"),
                   tags$button(id = "btn_indicator", class = "view-toggle-btn",
                               onclick = "setViewMode('indicator')", "Specific Indicator")
          ),
          
          # Hidden input read by server
          textInput("view_mode", label = NULL, value = "score"),
          tags$script(HTML("
        function setViewMode(mode) {
          document.getElementById('view_mode').value = mode;
          Shiny.setInputValue('view_mode', mode, {priority: 'event'});
          document.getElementById('btn_score').classList.toggle('active', mode === 'score');
          document.getElementById('btn_indicator').classList.toggle('active', mode === 'indicator');
        }
      ")),
          
          # Score View card
          conditionalPanel(
            condition = "input.view_mode == 'score'",
            div(class = "view-card score-card",
                div(class = "view-card-label", HTML("&#x1F4CA; Map a composite score")),
                fluidRow(
                  column(5,
                         selectInput("score_view",
                                     label = tags$span("Score", style = "color:#386150; font-weight:600;"),
                                     choices = names(score_views),
                                     selected = "Vulnerability Score")
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
            div(class = "view-card ind-card",
                div(class = "view-card-label", HTML("&#x1F50D; Drill into a specific indicator")),
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
  
  # ── Filters ──
  div(class = "filter-panel",
      fluidRow(
        # Geographic filters group
        column(6,
               div(class = "filter-section-label", HTML("&#x1F4CD; Geographic Filter"),
                   tags$span(style = "color:#aaa; font-weight:400; margin-left:6px; font-size:1.2rem;",
                             "(region/state recalculates scores)")),
               fluidRow(
                 column(4,
                        selectInput("filter_region",
                                    label = tags$span("Region", style = "color:#1D3557; font-weight:600; font-size:1.2rem;"),
                                    choices = c("All Regions" = "", all_regions), selected = "")
                 ),
                 column(4,
                        selectInput("filter_state",
                                    label = tags$span("State", style = "color:#1D3557; font-weight:600; font-size:1.2rem;"),
                                    choices = c("All States" = "", all_states), selected = "")
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
        ),
        
        # Divider
        # column(1, div(style = "border-left:1px solid #dce8ef; height:70px; margin:2px auto 0;")),
        
        # Priority toggle + Source Type filter
        column(4,
               fluidRow(
                 column(6,
                        div(class = "filter-section-label", HTML("&#x26A0;&#xFE0F; Priority Filter")),
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
                        div(class = "filter-section-label", HTML("&#x1F4A7; Filter by Source Type")),
                        selectizeInput("filter_source_type",
                                       label = NULL,
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
               div(class = "filter-section-label", HTML("&#x2139;&#xFE0F; Current View")),
               div(style = "margin-top:6px;",
                   uiOutput("filter_info"),
                   actionButton("clear_filters", "Clear All Filters",
                                class = "btn-sm")
               )
        )
      )
  ),
  
  # ── Map ──
  fluidRow(
    column(12,
           div(style = "position:relative;",
               uiOutput("map_title"),
               # conditionalPanel(
               #   condition = "output.parks_loading",
               #   div(class = "loading-overlay",
               #       h4(icon("spinner", class = "fa-spin"), " Loading park boundaries..."))
               # ),
               leafletOutput("map", height = "calc(110vh - 360px)")
           )
    )
  ),
  
  # ── Charts row ──
  # fluidRow(
  #   column(6,
  #          div(class = "section-panel", style = "min-height:440px;",
  #              h4(icon("chart-bar"), " Regional Score Breakdown",
  #                 style = "font-size:1.3rem;"),
  #              p(style = "font-size:1rem; color:#666;",
  #                "Mean factor scores for the currently filtered water supplies."),
  #              plotlyOutput("factor_chart", height = "350px")
  #          )
  #   ),
  # column(6,
  #        div(class = "section-panel", style = "min-height:440px;",
  #            h4(icon("map-marker-alt"), " Individual Water Supply Breakdown",
  #               style = "font-size:1.3rem;"),
  #            conditionalPanel(
  #              condition = "output.site_selected == false",
  #              div(style = "text-align:center; padding:80px 20px; color:#999;",
  #                  icon("mouse-pointer", style = "font-size:2rem; margin-bottom:10px;"),
  #                  p(style = "font-size:1.05rem;",
  #                    "Click on an individual water supply on the map to see its score breakdown."))
  #            ),
  #            conditionalPanel(
  #              condition = "output.site_selected == true",
  #              plotlyOutput("site_chart", height = "350px")
  #            )
  #        )
  # )
  # ),
  
  # ── Data table row ──
  fluidRow(
    column(12,
           div(class = "section-panel",
               h4(icon("table"), " Data Table"),
               p(style = "font-size:1.2rem; color:#666;",
                 "Click a row to highlight it on the map."),
               DTOutput("data_table")
           )
    )
  ),
  
  # ── Footer ──
  div(style = "margin-top:20px; padding:20px; background-color:#1D3557;
               color:white; text-align:center;",
      p("Application developed by the Colorado State University Geospatial Centroid | Data current as of April 2026",
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
  
  # ── Indicator description blurb ─────────────────────────────────────────
  output$indicator_description <- renderUI({
    req(input$view_mode == "indicator", input$component, input$factor, input$indicator)
    cfg <- indicator_config[[input$component]][[input$factor]][[input$indicator]]
    req(!is.null(cfg))
    div(style = "background:#edf3f8; border-left:3px solid #457B9D; border-radius:4px;
                 padding:7px 10px; font-size:1.5rem; color:#2c4a6e; line-height:1.4;",
        icon("info-circle", style = "margin-right:4px; color:#457B9D;"),
        cfg$description
    )
  })
  
  # ── Clear filters ───────────────────────────────────────────────────────
  observeEvent(input$clear_filters, {
    updateSelectInput(session, "filter_region", selected = "")
    updateSelectInput(session, "filter_state",  selected = "")
    updateSelectizeInput(session, "filter_park", selected = character(0))
    updateSelectizeInput(session, "filter_source_type", selected = character(0))
  })
  
  # ── Description pop-out modal ────────────────────────────────────────────
  observeEvent(input$show_description_btn, {
    info <- input$show_description_btn
    showModal(modalDialog(
      title = paste0("System Description — ", info$id),
      p(info$text, style = "font-size:1.5rem; line-height:1.6; color:#333;"),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  # ── State and Park filters cascade from region/state ─────────────────
  observe({
    df <- as.data.frame(combined_data)
    if (input$filter_region != "") {
      df <- df %>% filter(region == input$filter_region)
    }
    
    states_avail <- sort(unique(na.omit(df$state)))
    updateSelectInput(session, "filter_state",
                      choices = c("All States" = "", states_avail),
                      selected = input$filter_state)
  })
  
  observe({
    df <- as.data.frame(combined_data)
    if (input$filter_region != "") {
      df <- df %>% filter(region == input$filter_region)
    }
    if (input$filter_state != "") {
      df <- df %>% filter(state == input$filter_state)
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
  
  # ── Filtered + recalculated data ────────────────────────────────────────
  # When a geographic filter is active, vulnerability scores are recalculated
  # within that subset so rankings are relative to the filtered group.
  filtered_data <- reactive({
    df <- combined_data
    
    if (input$filter_region != "") df <- df %>% filter(region == input$filter_region)
    if (input$filter_state  != "") df <- df %>% filter(state  == input$filter_state)
    if (length(input$filter_park) > 0) df <- df %>% filter(park_unit %in% input$filter_park)
    
    has_geo_filter <- input$filter_region != "" || input$filter_state != ""
    
    # Recalculate scores only for region/state filters, not park-level selections
    if (has_geo_filter) {
      geom <- st_geometry(df)
      df_recalc <- tryCatch(
        calc_vulnerability_index(as.data.frame(df)),
        error = function(e) {
          message("Score recalculation failed (subset too small or low variance): ", e$message)
          as.data.frame(df)
        }
      )
      df <- st_sf(df_recalc, geometry = geom)
    }
    
    # Apply priority filter AFTER recalculation so it reflects updated rankings
    if (isTRUE(input$filter_priority)) df <- df %>% filter(priority_group == TRUE)
    
    df
  })
  
  # ── Populate source type choices from geo/priority-filtered data ────────
  observe({
    df <- as.data.frame(filtered_data())
    types_avail <- sort(unique(na.omit(df$source_type)))
    current_sel <- intersect(input$filter_source_type, types_avail)
    updateSelectizeInput(session, "filter_source_type",
                         choices  = types_avail,
                         selected = current_sel)
  })
  
  # ── Display data: source_type filter applied on top, no recalculation ───
  filtered_data_display <- reactive({
    df <- filtered_data()
    if (length(input$filter_source_type) > 0) {
      df <- df %>% filter(source_type %in% input$filter_source_type)
    }
    df
  })
  
  # ── Zoom to filtered extent ──────────────────────────────────────────────
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
  
  # ── Filter info badge ───────────────────────────────────────────────────
  output$filter_info <- renderUI({
    n   <- nrow(filtered_data())
    tot <- nrow(combined_data)
    has_geo_filter  <- input$filter_region != "" || input$filter_state != ""
    has_park_filter <- length(input$filter_park) > 0
    
    context_label <- if (input$filter_region != "" && input$filter_state != "") {
      paste0(input$filter_state, " (", input$filter_region, ")")
    } else if (input$filter_state != "") {
      input$filter_state
    } else if (input$filter_region != "") {
      input$filter_region
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
  
  # ── Active column ───────────────────────────────────────────────────────
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
  
  # ── Map title ───────────────────────────────────────────────────────────
  output$map_title <- renderUI({
    req(!is.null(input$view_mode) && nchar(input$view_mode) > 0)
    if (input$view_mode == "score") {
      title_text <- input$score_view
      border_col <- if (grepl("Exposure",    title_text)) "#457B9D" else
        if (grepl("Sensitivity", title_text)) "#C05235" else "#386150"
    } else {
      req(input$component, input$factor, input$indicator)
      cfg        <- indicator_config[[input$component]][[input$factor]][[input$indicator]]
      req(!is.null(cfg))
      title_text <- cfg$description
      border_col <- if (input$component == "Exposure") "#457B9D" else "#C05235"
    }
    div(
      style = paste0("position:absolute; top:10px; left:60px;
                      background:white; padding:10px 15px;
                      border-radius:6px; box-shadow:0 2px 8px rgba(0,0,0,0.2);
                      border-left:4px solid ", border_col, "; z-index:1000; max-width:260px;"),
      h5(title_text, style = paste0("margin:0; color:", border_col,
                                    "; font-weight:600; font-size:1.2rem;"))
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
  
  # ── Base map (once) ─────────────────────────────────────────────────────
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

  # ── Map markers ─────────────────────────────────────────────────────────
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
             pmax(2, pmin(12, scales::rescale(plot_vals, to=c(12,2), from=pal_dom))))
    } else {
      ifelse(is.na(plot_vals), 3,
             pmax(2, pmin(12, scales::rescale(plot_vals, to=c(2,12), from=pal_dom))))
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
      addLegend(
        pal = pal,
        values = na.omit(vals),
        title = legend_title,
        na.label = "No Data",
        position = "bottomright",
        opacity = 1,
        layerId = "legend"
      )
  })
  
  # ── Factor score breakdown chart ────────────────────────────────────────
  # output$factor_chart <- renderPlotly({
  #   df      <- as.data.frame(filtered_data())
  #   fac_cols <- names(factor_labels)
  #   fac_cols <- fac_cols[fac_cols %in% names(df)]
  #   
  #   means <- colMeans(df[, fac_cols, drop = FALSE], na.rm = TRUE)
  #   bar_order <- factor_labels[fac_cols]
  #   chart_df <- data.frame(
  #     factor = factor(bar_order, levels = bar_order),
  #     mean   = round(means, 3),
  #     component = ifelse(grepl("Exp", bar_order), "Exposure", "Sensitivity")
  #   )
  #   
  #   colors <- ifelse(chart_df$component == "Exposure", "#457B9D", "#C05235")
  #   
  #   plot_ly(chart_df, x = ~factor, y = ~mean, type = "bar",
  #           marker = list(color = colors),
  #           hovertemplate = "%{x}<br>Mean score: %{y:.3f}<extra></extra>") %>%
  #     layout(
  #       xaxis = list(title = "", tickfont = list(size = 10),
  #                    categoryorder = "array", categoryarray = bar_order),
  #       yaxis = list(title = "Mean Score (0-1)", range = c(0, 1),
  #                    tickfont = list(size = 10)),
  #       margin = list(t = 35, b = 60),
  #       showlegend = FALSE,
  #       plot_bgcolor  = "white",
  #       paper_bgcolor = "white"
  #     )
  # })
  # 
  
  # ── Marker click → modal with site chart ────────────────────────────────
  clicked_site <- reactiveVal(NULL)
  
  
  # ── Marker click — just track the site ──────────────────────────────────
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    req(!is.null(click$id))
    clicked_site(click$id)
  })
  
  # ── Table row click → zoom map to selected site ─────────────────────────
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
  
  # ── Popup button → modal with site chart ────────────────────────────────
  observeEvent(input$show_chart_btn, {
    site_id <- input$show_chart_btn
    df_all  <- as.data.frame(filtered_data())
    req(site_id %in% df_all$wsd_source_id)
    
    site_row  <- df_all[df_all$wsd_source_id == site_id, ]
    fac_cols  <- names(factor_labels)[names(factor_labels) %in% names(df_all)]
    means     <- colMeans(df_all[, fac_cols, drop = FALSE], na.rm = TRUE)
    site_vals <- as.numeric(site_row[1, fac_cols])
    bar_order <- factor_labels[fac_cols]
    
    chart_df <- data.frame(
      factor    = factor(bar_order, levels = bar_order),
      regional  = round(means, 3),
      site      = round(site_vals, 3),
      component = ifelse(grepl("Exp", bar_order), "Exposure", "Sensitivity")
    )
    
    site_title <- paste0(site_row$park_unit[1], " \u2013 ", site_row$wsd_source_id[1])
    
    p <- plot_ly(chart_df, x = ~factor) %>%
      add_bars(y = ~regional, name = "Regional Mean",
               marker = list(color = "rgba(180,180,180,0.5)"),
               hovertemplate = "%{x}<br>Regional mean: %{y:.3f}<extra></extra>") %>%
      add_bars(y = ~site, name = "Selected Site",
               marker = list(color = ifelse(chart_df$component == "Exposure", "#457B9D", "#C05235")),
               hovertemplate = "%{x}<br>Site score: %{y:.3f}<extra></extra>") %>%
      layout(
        barmode = "group",
        title   = list(text = site_title, font = list(size = 13, color = "#1D3557"),
                       x = 0, xanchor = "left"),
        xaxis   = list(title = "", tickfont = list(size = 10),
                       categoryorder = "array", categoryarray = bar_order),
        yaxis   = list(title = "Score (0-1)", range = c(0, 1), tickfont = list(size = 10)),
        margin  = list(t = 35, b = 60),
        legend  = list(orientation = "h", y = -0.25),
        plot_bgcolor  = "white",
        paper_bgcolor = "white"
      )
    
    showModal(modalDialog(
      title = NULL,
      renderPlotly(p),
      tags$p("\u24d8 Regional values representative of the currently filtered selection",
             style = "font-size:11px; color:#888; margin-top:4px; margin-bottom:0;"),
      size      = "l",
      easyClose = TRUE,
      footer    = modalButton("Close")
    ))
  })
  
  #### Generate Report button → build HTML report ####
  observeEvent(input$generate_report_btn, {
    site_id <- input$generate_report_btn
    req(site_id %in% combined_raw$wsd_source_id)
    
    notif_id <- showNotification(
      "Generating report...", duration = NULL, closeButton = FALSE, type = "message"
    )
    
    tryCatch({
      # ── Site metadata ────────────────────────────────────────────────────
      site_meta  <- combined_raw[combined_raw$wsd_source_id == site_id, ]
      site_park  <- site_meta$park_unit[1]
      site_state <- site_meta$state[1]
      site_region <- site_meta$region[1]
      sys_type   <- ifelse(is.na(site_meta$source_type[1]), "N/A", site_meta$source_type[1])
      desc_text  <- ifelse(is.na(site_meta$description[1]) || nchar(trimws(site_meta$description[1])) == 0,
                           "", site_meta$description[1])
      
      # ── Helper: extract ranks from a scored dataframe ────────────────────
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
      
      # ── National: reuse the already-computed combined_data (no recalc!) ──
      national_df <- as.data.frame(combined_data)
      national    <- extract_ranks(national_df, site_id)
      
      # ── Regional, State, Park: recalc only the subsets ───────────────────
      regional <- if (nrow(combined_raw[combined_raw$region == site_region, ]) >= 2) {
        extract_ranks(calc_vulnerability_index(combined_raw[combined_raw$region == site_region, ]), site_id)
      } else { list(vuln_rank = NA, exp_rank = NA, sen_rank = NA, n = 1, priority = NA) }
      
      state_ranks <- if (nrow(combined_raw[combined_raw$state == site_state, ]) >= 2) {
        extract_ranks(calc_vulnerability_index(combined_raw[combined_raw$state == site_state, ]), site_id)
      } else { list(vuln_rank = NA, exp_rank = NA, sen_rank = NA, n = 1, priority = NA) }
      
      park_ranks <- if (nrow(combined_raw[combined_raw$park_unit == site_park, ]) >= 2) {
        extract_ranks(calc_vulnerability_index(combined_raw[combined_raw$park_unit == site_park, ]), site_id)
      } else { list(vuln_rank = NA, exp_rank = NA, sen_rank = NA, n = 1, priority = NA) }
      
      # ── Bar chart data (from national, already computed) ─────────────────
      site_scored <- national_df[national_df$wsd_source_id == site_id, ]
      fac_cols    <- names(factor_labels)[names(factor_labels) %in% names(national_df)]
      nat_means   <- colMeans(national_df[, fac_cols, drop = FALSE], na.rm = TRUE)
      site_vals   <- as.numeric(site_scored[1, fac_cols])
      bar_labels  <- factor_labels[fac_cols]
      components  <- ifelse(grepl("exp", fac_cols), "Exposure", "Sensitivity")
      
      # ── Hazard flags ─────────────────────────────────────────────────────
      flags <- c(
        if (isTRUE(site_scored$flag_fire[1]))    "\U0001F525 Fire"    else NULL,
        if (isTRUE(site_scored$flag_flood[1]))   "\U0001F4A7 Flood"   else NULL,
        if (isTRUE(site_scored$flag_slr[1]))     "\U0001F30A SLR"     else NULL,
        if (isTRUE(site_scored$flag_drought[1])) "\u2600\uFE0F Drought" else NULL
      )
      
      # ── Generate chart as base64 PNG ─────────────────────────────────────
      chart_df <- data.frame(
        factor    = factor(bar_labels, levels = bar_labels),
        score     = site_vals,
        component = components,
        stringsAsFactors = FALSE
      )
      
      site_fill <- ifelse(components == "Exposure", "#457B9D", "#C05235")
      
      p <- ggplot2::ggplot(chart_df, ggplot2::aes(x = factor, y = score, fill = factor)) +
        ggplot2::geom_col(width = 0.65) +
        ggplot2::scale_fill_manual(values = setNames(site_fill, bar_labels), guide = "none") +
        ggplot2::scale_y_continuous(limits = c(0, 1), expand = ggplot2::expansion(mult = c(0, 0.05))) +
        ggplot2::labs(x = NULL, y = "Score (0-1)") +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
          panel.grid.major.x = ggplot2::element_blank(),
          legend.position = "none",
          plot.background = ggplot2::element_rect(fill = "white", color = NA),
          panel.background = ggplot2::element_rect(fill = "white", color = NA)
        )
      
      chart_file <- tempfile(fileext = ".png")
      ggplot2::ggsave(chart_file, p, width = 8, height = 4.5, dpi = 150, bg = "white")
      chart_b64 <- base64enc::base64encode(chart_file)
      
      # ── Build the ranking boxes HTML ─────────────────────────────────────
      make_rank_box <- function(label, ranks, color) {
        rank_val <- if (is.na(ranks$vuln_rank)) "N/A" else paste0(ranks$vuln_rank, "%")
        priority_badge <- if (isTRUE(ranks$priority))
          "<span style='background:#9B2226;color:white;padding:2px 6px;border-radius:3px;font-size:10px;'>TOP PRIORITY</span>" else ""
        paste0(
          "<div style='flex:1;background:white;border-radius:8px;padding:14px 12px;border-top:4px solid ", color, ";",
          "box-shadow:0 2px 6px rgba(0,0,0,0.08);text-align:center;min-width:140px;'>",
          "<div style='font-size:11px;color:#888;text-transform:uppercase;letter-spacing:0.05em;margin-bottom:4px;'>", label, "</div>",
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
      
      # ── Sub-component ranks table ────────────────────────────────────────
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
      
      # ── Hazard flags HTML ────────────────────────────────────────────────
      flag_colors <- c("\U0001F525 Fire" = "#C05235", "\U0001F4A7 Flood" = "#457B9D",
                       "\U0001F30A SLR" = "#1D3557", "\u2600\uFE0F Drought" = "#8B6914")
      flags_html <- if (length(flags) > 0) {
        badges <- sapply(flags, function(f) {
          col <- ifelse(f %in% names(flag_colors), flag_colors[f], "#666")
          paste0("<span style='background:", col, ";color:white;padding:3px 10px;border-radius:4px;",
                 "font-size:12px;margin-right:4px;'>", f, "</span>")
        })
        paste0("<div style='margin:10px 0;'>", paste(badges, collapse = " "), "</div>")
      } else ""
      
      # ── Raw scores ──────────────────────────────────────────────────────
      vuln_score <- round(site_scored$VULNERABILITY[1], 3)
      exp_score  <- round(site_scored$EXPOSURE[1], 3)
      sen_score  <- round(site_scored$SENSITIVITY[1], 3)
      
      # ── Description section ──────────────────────────────────────────────
      desc_html <- if (nchar(desc_text) > 0) {
        paste0("<div style='background:#f7fafc;border-left:3px solid #2a7f7f;padding:10px 14px;",
               "border-radius:4px;margin:10px 0;font-size:13px;color:#333;line-height:1.5;'>",
               "<strong>System Description:</strong> ", htmltools::htmlEscape(desc_text), "</div>")
      } else ""
      
      # ── Assemble full HTML ───────────────────────────────────────────────
      html_content <- paste0(
        "<!DOCTYPE html><html><head><meta charset='utf-8'>",
        "<title>Vulnerability Report - ", htmltools::htmlEscape(site_meta$park_name[1]), "</title>",
        "<style>",
        "body{font-family:Arial,Helvetica,sans-serif;max-width:850px;margin:0 auto;padding:30px 40px;color:#333;line-height:1.5;}",
        "h1{color:#1D3557;font-size:24px;margin:0 0 4px 0;}",
        "h2{color:#1D3557;font-size:17px;border-bottom:2px solid #2a7f7f;padding-bottom:4px;margin-top:28px;}",
        ".subtitle{color:#386150;font-size:16px;font-weight:600;margin:0 0 2px 0;}",
        ".meta{color:#888;font-size:13px;margin:0 0 8px 0;}",
        ".accent-line{height:3px;background:linear-gradient(90deg,#1D3557,#2a7f7f,#A8DADC);border-radius:2px;margin:12px 0 20px 0;}",
        "table{border-collapse:collapse;width:100%;}",
        "thead tr{border-bottom:2px solid #1D3557;}",
        "tbody tr{border-bottom:1px solid #eee;}",
        "tbody tr:hover{background:#f8fbfd;}",
        ".info-table td{padding:5px 10px;font-size:13px;}",
        ".info-table td:first-child{font-weight:600;color:#1D3557;width:140px;}",
        ".score-row{display:flex;gap:16px;margin:12px 0;flex-wrap:wrap;}",
        ".score-box{background:#f0f5f8;border-radius:6px;padding:10px 16px;text-align:center;flex:1;min-width:120px;}",
        ".score-box .label{font-size:11px;color:#888;text-transform:uppercase;letter-spacing:0.05em;}",
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
        "<tr><td>Park Unit</td><td>", htmltools::htmlEscape(site_park), "</td></tr>",
        "<tr><td>Park Name</td><td>", htmltools::htmlEscape(site_meta$park_name[1]), "</td></tr>",
        "<tr><td>State</td><td>", htmltools::htmlEscape(site_state), "</td></tr>",
        "<tr><td>Region</td><td>", htmltools::htmlEscape(site_region), "</td></tr>",
        "<tr><td>Water System</td><td>", htmltools::htmlEscape(site_meta$water_system_name[1]), "</td></tr>",
        "<tr><td>Source Type</td><td>", htmltools::htmlEscape(sys_type), "</td></tr>",
        "</table>",
        desc_html,
        flags_html,
        
        # Raw scores
        "<div class='score-row'>",
        "<div class='score-box'><div class='label'>Vulnerability</div><div class='value' style='color:#386150;'>", vuln_score, "</div></div>",
        "<div class='score-box'><div class='label'>Exposure</div><div class='value' style='color:#457B9D;'>", exp_score, "</div></div>",
        "<div class='score-box'><div class='label'>Sensitivity</div><div class='value' style='color:#C05235;'>", sen_score, "</div></div>",
        "</div>",
        
        # Priority Rankings
        "<h2>Priority Rankings</h2>",
        "<p style='font-size:12px;color:#888;margin-top:-4px;'>Vulnerability percentile rank at four geographic scopes. A rank of 90 means the supply scores higher than 90% of the comparison group.</p>",
        rank_boxes,
        rank_table,
        
        # Chart
        "<h2>Factor Score Breakdown</h2>",
        "<p style='font-size:12px;color:#888;margin-top:-4px;'>Comparison of this site&#39;s factor scores against the national mean. Blue = Exposure, Red = Sensitivity.</p>",
        "<img src='data:image/png;base64,", chart_b64, "' style='width:100%;max-width:750px;display:block;margin:12px auto;' />",
        
        # Footer
        "<div class='footer'>",
        "NPS Water Supply Vulnerability Assessment Tool<br>",
        "Scores calculated using the Michalak et al. (2026) Euclidean distance framework<br>",
        "Colorado State University Geospatial Centroid &nbsp;|&nbsp; ", format(Sys.Date(), "%B %Y"),
        "</div>",
        
        "</body></html>"
      )
      
      # ── Write HTML file ──────────────────────────────────────────────────
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
  
  # ── Pre-compute coordinate lookup from the GLOBAL sf object ────────────
  # This never changes, so no reactive needed — avoids triggering any chain
  coord_lookup <- as.data.frame(st_coordinates(combined_data))
  coord_lookup$wsd_source_id <- combined_data$wsd_source_id
  names(coord_lookup) <- c("lng", "lat", "wsd_source_id")
  
  # Track the wsd_source_id vector in the same row order as the DT receives
  
  dt_ids <- reactiveVal(character(0))
  
  # ── Data table ──────────────────────────────────────────────────────────
  output$data_table <- renderDT(server = TRUE, {
    df <- as.data.frame(filtered_data_display()) %>%
      select(wsd_source_id, park_unit, park_name, state, region,
             water_system_name, source_type,
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
          ifelse(!is.na(flag_fire)    & flag_fire,    "<span style='background:#C05235;color:white;padding:1px 5px;border-radius:3px;font-size:10px;margin-right:2px;'>&#x1F525; Fire</span>", ""),
          ifelse(!is.na(flag_flood)   & flag_flood,   "<span style='background:#457B9D;color:white;padding:1px 5px;border-radius:3px;font-size:10px;margin-right:2px;'>&#x1F4A7; Flood</span>", ""),
          ifelse(!is.na(flag_slr)     & flag_slr,     "<span style='background:#1D3557;color:white;padding:1px 5px;border-radius:3px;font-size:10px;margin-right:2px;'>&#x1F30A; SLR</span>", ""),
          ifelse(!is.na(flag_drought) & flag_drought,  "<span style='background:#8B6914;color:white;padding:1px 5px;border-radius:3px;font-size:10px;margin-right:2px;'>&#x2600;&#xFE0F; Drought</span>", "")
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
  
  # ── Popup builder ───────────────────────────────────────────────────────
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
        paste0("<b>", raw_label, ":</b> <span style='font-size:13px; font-weight:bold;'>",
               round(raw_value, 3), "</span>")
      } else {
        paste0("<b>", raw_label, ":</b> <span style='color:#999;'>No data</span>")
      }
      paste0("<b style='color:#1D3557; font-size:14px;'>", indicator_name, "</b><br>",
             "<span style='font-size:11px; color:#666;'>", component, " \u203a ", factor_name,
             "</span><br>", raw_row)
    }
    
    score_section <- paste0(
      "<hr style='margin:6px 0; border-color:#ddd;'>",
      "<table style='font-size:11px; width:100%;'><tr>",
      "<td><b>Vulnerability</b></td><td><b>Exposure</b></td><td><b>Sensitivity</b></td>",
      "</tr><tr>",
      "<td style='color:#386150; font-weight:bold;'>", round(vuln_rank, 1), "%</td>",
      "<td style='color:#457B9D; font-weight:bold;'>", round(exp_rank, 1), "%</td>",
      "<td style='color:#C05235; font-weight:bold;'>", round(sen_rank, 1), "%</td>",
      "</tr></table>",
      "<span style='font-size:9px; color:#999;'>Percentile rank (higher = more vulnerable)</span>")
    
    flags <- c(
      if (isTRUE(priority))    "<span style='background:#9B2226;color:white;padding:1px 5px;border-radius:3px;font-size:10px;'>HIGH PRIORITY</span>" else NULL,
      if (isTRUE(flag_fire))   "<span style='background:#C05235;color:white;padding:1px 5px;border-radius:3px;font-size:10px;'>&#x1F525; Fire</span>"   else NULL,
      if (isTRUE(flag_flood))  "<span style='background:#457B9D;color:white;padding:1px 5px;border-radius:3px;font-size:10px;'>&#x1F4A7; Flood</span>"  else NULL,
      if (isTRUE(flag_slr))    "<span style='background:#1D3557;color:white;padding:1px 5px;border-radius:3px;font-size:10px;'>&#x1F30A; SLR</span>"    else NULL,
      if (isTRUE(flag_drought)) "<span style='background:#8B6914;color:white;padding:1px 5px;border-radius:3px;font-size:10px;'>&#x2600;&#xFE0F; Drought</span>" else NULL
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
      "<b>Source Type:</b> ",     ifelse(is.na(source_type), "<span style='color:#999;'>N/A</span>", source_type), "<br>",
      "<b>State:</b> ",           state,           "<br>",
      if (!is.na(description) && nchar(trimws(description)) > 0)
        paste0("<a href='#' onclick=\"Shiny.setInputValue('show_description_btn', {id:'",
               wsd_source_id, "', text: '", gsub("'", "\\'", description, fixed = TRUE), "'}, ",
               "{priority:'event'}); return false;\" ",
               "style='font-size:11px; color:#457B9D;'>&#x1F4C4; View Full Description</a><br>")
      else "",
      "<hr style='margin:6px 0; border-color:#ddd;'>",
      "<button onclick=\"Shiny.setInputValue('show_chart_btn', '", wsd_source_id,
      "', {priority: 'event'});\" ",
      "style='background:#1D3557;color:white;border:none;border-radius:4px;",
      "padding:5px 10px;font-size:11px;cursor:pointer;width:100%;margin-bottom:4px;'>",
      "&#x1F4CA; View Score Breakdown</button>",
      "<button onclick=\"Shiny.setInputValue('generate_report_btn', '", wsd_source_id,
      "', {priority: 'event'});\" ",
      "style='background:#2a7f7f;color:white;border:none;border-radius:4px;",
      "padding:5px 10px;font-size:11px;cursor:pointer;width:100%;'>",
      "&#x1F4C4; Generate Report</button>")
    
    paste0("<div style='font-family:Arial; font-size:12px; min-width:210px;'>",
           top_section, score_section, flag_section, site_section, "</div>")
  }
}

shinyApp(ui = ui, server = server)