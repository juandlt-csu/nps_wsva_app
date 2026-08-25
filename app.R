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
# Evaluated at startup rather than hardcoded, so a long-running deployment does
# not keep advertising the month the string was written in. Used by the app
# footer and the map export caption, so both always agree.
#
# CAVEAT: this reports when the app process started, not when the indicators were
# last rebuilt. Those are the same thing on a redeploy that ships new data, and
# diverge otherwise. To state a true data vintage, replace this with a pinned
# string (e.g. "August 2026") updated alongside final_indicators.csv.
DATA_LAST_UPDATED <- format(Sys.Date(), "%B %Y")

## Feedback Configuration ----
# Where user-submitted bug reports / feature requests are routed.
#
# Nothing is stored server-side. Submissions are composed in the browser and
# handed to the user's own mail client; the app never writes them to disk and
# never sends mail itself. That keeps the answer to "where did my feedback go"
# unambiguous, and avoids accumulating reviewers' names and email addresses in a
# CSV on the host.
#
# Interim arrangement: everything goes to the maintainer directly rather than to
# a shared inbox or the upstream issue tracker, pending a proper submission form.
# When that form exists, replace the mailto construction in the feedback submit
# handler with a POST to it; nothing else in the flow needs to change.
FEEDBACK_EMAIL <- "ccmothes@colostate.edu"
APP_VERSION    <- "2026-08 review build"

## Technical Report PDF ----
# The report lives in www/, which Shiny already serves as the web root, so the
# browser reaches it at the bare filename -- no addResourcePath() and no copy
# into tempdir() needed. This is also why www/ is the right home for it rather
# than the app root: serving the root directory would have exposed app.R,
# calc_vulnerability_index.R and everything in app_data/ alongside it.
#
# Presence is checked at startup so that a deployment which drops the file (an
# rsconnect bundle that filters by extension, say) falls back to the "link
# pending" placeholder instead of offering a button that 404s.
TECH_REPORT_FILE      <- "NPS_Water_Vulnerability_Methods_Report_Aug_26.pdf"
TECH_REPORT_SRC       <- file.path("www", TECH_REPORT_FILE)
TECH_REPORT_AVAILABLE <- file.exists(TECH_REPORT_SRC)
TECH_REPORT_URL       <- TECH_REPORT_FILE

TECH_REPORT_SIZE <- NULL
TECH_REPORT_DATE <- NULL
if (TECH_REPORT_AVAILABLE) {
  fi <- file.info(TECH_REPORT_SRC)
  TECH_REPORT_SIZE <- paste0(round(fi$size / 1024^2, 1), " MB")
  TECH_REPORT_DATE <- format(fi$mtime, "%B %d, %Y")
} else {
  message("[WSVA] Technical report PDF not found at ", TECH_REPORT_SRC,
          "; the Technical Documentation tab will show the pending-link placeholder.")
}

## Public App URL ----
# Stamped into the footer of every report and map export so a PDF that has been
# forwarded around still says where it came from and can be regenerated. Update
# here if the deployment moves; the report footer, the map export caption and
# anything added later all read from this one place.
APP_URL         <- "https://apps.gis.colostate.edu/WSVA_tool/"
APP_URL_DISPLAY <- "apps.gis.colostate.edu/WSVA_tool/"




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
         # FMSS asset identifier — the number facilities staff use to look the
         # system up in their own maintenance records, so it belongs in the
         # popup and every report even though it never enters a score.
         fmss_number,
         # State water-rights context. SurfaceWaterLaw / GroundWaterLaw are the
         # STATE legal doctrine, not a determination about this system's rights;
         # state_water_rights_id is the system's own permit/right identifier
         # where the WSD records one (~21% of systems).
         state_water_rights_id, SurfaceWaterLaw, GroundWaterLaw,
         source_longitude, source_latitude) %>%
  mutate(across(c(fmss_number, state_water_rights_id,
                  SurfaceWaterLaw, GroundWaterLaw),
                as.character))

# Raw indicators — vulnerability scores are calculated on the fly
final_indicators <- read_csv("app_data/final_indicators.csv")

# Indicator methodology details (Data Source / Methodology / Vulnerability
# Direction), sourced from the Technical Methods Report. One row per raw_col.
# See app_data/indicator_details.csv — add a row there any time a new
# indicator is added or the methods report is updated; no code change needed.
# Indicators with no row here (e.g. not yet documented) simply show a
# "not yet available" message in the info tooltip.
indicator_details_raw <- read_csv("app_data/indicator_details.csv")
indicator_details <- setNames(purrr::transpose(indicator_details_raw), indicator_details_raw$raw_col)

# Join metadata + raw indicators, calculate national scores, convert to sf
combined_raw <- water_supplies %>%
  left_join(final_indicators, by = "wsd_source_id")

combined_data <- calc_vulnerability_index(combined_raw) %>%
  st_as_sf(coords = c("source_longitude", "source_latitude"), crs = 4326)

## Water Balance Model Explorer Data ----
# One row per water supply x variable x future period, holding the ensemble
# summary of the LOCA2/MWBM projections (Appendix A of the Technical Methods
# Report). Read once at startup and dropped to a plain data frame -- the
# explorer draws its own markers from source_longitude/source_latitude, so the
# gpkg geometry is dead weight in memory.
#
# n_projections == 0 marks sites the MWBM grid does not cover (three offshore
# Dry Tortugas supplies). Their values are already NA; the flag is kept so the
# explorer can say "outside model coverage" rather than "no data".
wbm_data <- sf::st_read("app_data/wbm_data.gpkg", quiet = TRUE) %>%
  sf::st_drop_geometry() %>%
  dplyr::mutate(
    n_projections = suppressWarnings(as.numeric(n_projections)),
    in_coverage   = !is.na(n_projections) & n_projections > 0
  )

# Variable metadata, drawn from Appendix A "Variable Descriptions". `agg` is the
# water-year aggregation the source data already applied, and is surfaced in the
# UI because "annual precipitation" and "annual maximum SWE" are not the same
# kind of number. `pct_ok` is FALSE for temperature: a percent change in degrees
# Celsius is not meaningful, so that metric is hidden for it.
wbm_var_meta <- list(
  precip = list(label = "Precipitation", unit = "mm", agg = "water-year total", pct_ok = TRUE,
                desc = paste("Monthly total precipitation from LOCA2, as liquid-water equivalent.",
                             "Precipitation is a model input and the primary source of water entering the MWBM.",
                             "Depending on temperature and model state it is apportioned among snowpack,",
                             "evapotranspiration, soil-water storage, and immediate or delayed runoff.")),
  temp   = list(label = "Temperature", unit = "\u00b0C", agg = "water-year mean", pct_ok = FALSE,
                desc = paste("Monthly mean near-surface air temperature from LOCA2. Temperature is a model",
                             "input rather than an MWBM output: it determines whether precipitation is treated",
                             "as rain or snow, controls snow accumulation and melt, and is used with",
                             "extraterrestrial radiation to calculate PET. It should NOT be interpreted as the",
                             "temperature of the source water.")),
  runoff = list(label = "Runoff", unit = "mm", agg = "water-year total", pct_ok = TRUE,
                desc = paste("Total monthly runoff generated within the grid cell. A spatially uniform direct",
                             "runoff fraction of 0.05 sends 5% of monthly precipitation straight to runoff;",
                             "the rest passes through the model's snow and soil-water accounting, with water",
                             "above the soil-water-holding capacity contributing to delayed runoff. Runoff is",
                             "generated locally and is NOT routed through a stream network, so it should not be",
                             "read as streamflow or discharge at a channel, gage, or intake.")),
  snow   = list(label = "Snow Water Equivalent (SWE)", unit = "mm", agg = "water-year maximum", pct_ok = TRUE,
                desc = paste("The liquid-water equivalent of water stored in the modeled snowpack, not the",
                             "physical depth of snow. Annual maximum SWE represents the greatest modeled",
                             "snow-water storage reached during a water year. Percent change is unstable where",
                             "the historical baseline is near zero -- read the raw historical value alongside it.")),
  pet    = list(label = "Potential Evapotranspiration (PET)", unit = "mm", agg = "water-year total", pct_ok = TRUE,
                desc = paste("The evapotranspiration that could occur under modeled atmospheric conditions",
                             "before water-availability limits are applied. Calculated from monthly mean air",
                             "temperature and extraterrestrial radiation using the Oudin method. PET represents",
                             "potential atmospheric water demand rather than actual water loss and does not",
                             "account for differences among vegetation or land-cover types.")),
  aet    = list(label = "Actual Evapotranspiration (AET)", unit = "mm", agg = "water-year total", pct_ok = TRUE,
                desc = paste("Modeled water transferred from the land surface to the atmosphere after",
                             "accounting for water availability, calculated as a function of precipitation,",
                             "available soil moisture and PET. AET approaches PET when water is plentiful and",
                             "falls below it when precipitation and soil-water storage cannot meet demand.")),
  stor   = list(label = "Soil-Moisture Storage", unit = "mm", agg = "water-year mean", pct_ok = TRUE,
                desc = paste("Water retained in the model's soil column, limited by a spatially varying",
                             "soil-water-holding capacity and an assumed 1 m rooting depth. Replenished by",
                             "precipitation and snowmelt, depleted by actual evapotranspiration. This is the",
                             "model's conceptual soil-water bucket and should not be read as a measurement of",
                             "volumetric soil moisture, groundwater storage, or basin-wide available water."))
)

# Only offer variables that are actually present in the shipped gpkg.
wbm_vars_present <- intersect(names(wbm_var_meta), unique(wbm_data$variable))
wbm_var_choices  <- setNames(wbm_vars_present,
                             vapply(wbm_vars_present,
                                    function(v) wbm_var_meta[[v]]$label, character(1)))

# Period labels are read from the data rather than hard-coded, so a re-run that
# shifts the projection windows relabels the UI automatically.
wbm_period_label <- function(p) {
  r <- wbm_data[wbm_data$period == p, ][1, ]
  if (nrow(r) == 0 || is.na(r$future_period)) return(p)
  paste0(switch(p, "mid_century" = "Mid-century", "late_century" = "Late-century", p),
         " (", r$future_period, ")")
}
wbm_periods_present <- intersect(c("mid_century", "late_century"), unique(wbm_data$period))
wbm_period_choices  <- setNames(wbm_periods_present,
                                vapply(wbm_periods_present, wbm_period_label, character(1)))
WBM_HIST_PERIOD <- {
  h <- unique(na.omit(wbm_data$historical_period))
  if (length(h) == 0) "historical" else h[1]
}

## Pre-Compute Filter Options ----
all_regions <- sort(unique(na.omit(combined_data$region)))
all_states  <- sort(unique(na.omit(combined_data$state)))
all_parks   <- sort(unique(na.omit(combined_data$park_unit)))

## Indicator Configuration: Component -> Factor -> Indicator ----
# col      = normalized column name (0-1, produced by calc_vulnerability_index)
# raw_col  = raw indicator column name (from final_indicators.csv)
# status   = development status from Table 1 of the Technical Methods Report:
#            "Implemented" or "Provisional". Anything listed as "Planned" in the
#            report (Flood exposure, Water Quality, Water Rights, Water Treatment
#            Type) is intentionally absent here -- it is not calculated and
#            contributes nothing to any score.
# is_weight = TRUE for model-agreement columns. These are confidence weights
#            applied multiplicatively to their parent indicator, not independent
#            axes of the Euclidean distance, so they are excluded from the
#            normalized-distribution comparison chart.
indicator_config <- list(
  
  "Exposure" = list(
    # Section 4.5 of the Technical Methods Report defines ONE Water Supply
    # exposure factor whose underlying variable is chosen per system: runoff for
    # groundwater / spring / surface-water supplies, precipitation for rainwater
    # harvesting, and nothing for ocean sources. The runoff and precipitation
    # indicators are therefore two implementations of the same two indicators
    # (Amount and Timing), not four independent ones -- `source_var` records
    # which systems each applies to so the app can say so on screen.
    "Water Supply" = list(
      "Change in Runoff" = list(
        col = "norm_exp_runoff_change", raw_col = "exp_runoff_change",
        status = "Implemented", source_var = "runoff", supply_role = "amount",
        raw_label = "% change mean annual runoff (10th percentile of all models)",
        description = "Projected % change in mean annual runoff (10th percentile of all models). Runoff is the water supply signal for groundwater, spring and surface-water systems."
      ),
      "Runoff Model Agreement" = list(
        col = NULL, raw_col = "exp_runoff_model_agree",
        status = "Implemented", is_weight = TRUE, source_var = "runoff",
        raw_label = "% models predicting decrease",
        description = "% of climate models predicting a decrease in runoff. Applied as a confidence weight on Change in Runoff."
      ),
      "Change in Runoff Timing" = list(
        col = "norm_exp_runoff_timing_change", raw_col = "exp_runoff_timing_change",
        status = "Provisional", source_var = "runoff", supply_role = "timing",
        raw_label = "Absolute shift in runoff center of timing (days)",
        description = "Projected shift in the circular center of runoff timing, in days. Larger shifts in either direction disrupt established seasonal supply patterns."
      ),
      "Runoff Timing Model Agreement" = list(
        col = NULL, raw_col = "exp_runoff_timing_change_model_agree",
        status = "Provisional", is_weight = TRUE, source_var = "runoff",
        raw_label = "Fraction of models supporting the timing shift direction",
        description = "Fraction of climate models supporting the selected direction of timing change. Applied as a confidence weight on Change in Runoff Timing."
      ),
      "Change in Precipitation" = list(
        col = "norm_exp_precip_change", raw_col = "exp_precip_change",
        status = "Implemented", source_var = "precipitation", supply_role = "amount",
        raw_label = "% change mean annual precip (10th percentile of all models)",
        description = "Projected % change in mean annual precipitation (10th percentile of all models). Precipitation is the water supply signal for rainwater-harvesting systems only."
      ),
      "Precipitation Model Agreement" = list(
        col = NULL, raw_col = "exp_precip_model_agree",
        status = "Implemented", is_weight = TRUE, source_var = "precipitation",
        raw_label = "% models predicting decrease",
        description = "% of climate models predicting a precipitation decrease. Applied as a confidence weight on Change in Precipitation."
      ),
      "Change in Precipitation Timing" = list(
        col = "norm_exp_precip_timing_change", raw_col = "exp_precip_timing_change",
        status = "Provisional", source_var = "precipitation", supply_role = "timing",
        raw_label = "Absolute shift in precip center of timing (days)",
        description = "Projected shift in the circular center of precipitation timing, in days. Larger shifts in either direction disrupt established seasonal supply patterns."
      ),
      "Precipitation Timing Model Agreement" = list(
        col = NULL, raw_col = "exp_precip_timing_change_model_agree",
        status = "Provisional", is_weight = TRUE, source_var = "precipitation",
        raw_label = "Fraction of models supporting the timing shift direction",
        description = "Fraction of climate models supporting the selected direction of timing change. Applied as a confidence weight on Change in Precipitation Timing."
      )
    ),
    "Drought" = list(
      "Change in Drought (SPEI)" = list(
        col = "norm_exp_drought_change", raw_col = "exp_drought_change",
        status = "Implemented",
        raw_label = "Change in drought (SPEI)",
        description = "Projected change in SPEI based on monthly precipitation and PET"
      ),
      "Drought Model Agreement" = list(
        col = NULL, raw_col = "exp_drought_model_agree",
        status = "Implemented", is_weight = TRUE,
        raw_label = "% models predicting decrease",
        description = "% of climate models predicting a decrease in SPEI. Applied as a confidence weight on Change in Drought."
      )
    ),
    "Demand" = list(
      "Change in Competition" = list(
        col = "norm_exp_nearby_use_change", raw_col = "exp_nearby_use_change",
        status = "Implemented",
        raw_label = "% change in nearby water use per km\u00b2 (90th percentile of all models)",
        description = "Projected change in county water use (competition for supply)"
      ),
      "Competition Model Agreement" = list(
        col = NULL, raw_col = "exp_nearby_use_model_agree",
        status = "Implemented", is_weight = TRUE,
        raw_label = "% models predicting increase",
        description = "% of climate models predicting an increase in county water use. Applied as a confidence weight on Change in Competition."
      ),
      "Change in Supply-Demand Mismatch" = list(
        col = "norm_exp_auc_change", raw_col = "exp_auc_change",
        status = "Provisional",
        raw_label = "Change in monthly timing mismatch AUC",
        description = "Projected change in the area under the curve where monthly visitation share exceeds monthly water supply share. A larger increase means demand is shifting further out of alignment with available water."
      )
    ),
    "Sea Level Rise" = list(
      "Inundation from Sea Level Rise" = list(
        col = "norm_exp_inundation_slr", raw_col = "exp_inundation_slr",
        status = "Implemented",
        raw_label = "% point change in inundated area",
        description = "Projected % change in area inundated by sea level rise"
      ),
      "Saltwater Intrusion" = list(
        col = "norm_exp_swi", raw_col = "exp_swi",
        status = "Implemented",
        raw_label = "Fraction of coastal corridor intruded",
        description = "Projected saltwater migration to 2100, expressed as the fraction of the corridor between the supply and the coast that is intruded"
      ),
      "Storm Surge" = list(
        col = "norm_exp_storm_surge", raw_col = "exp_storm_surge",
        status = "Provisional",
        raw_label = "Change in RP100 storm surge (m) per unit distance to coast",
        description = "Projected change in 100-year return period storm surge level, weighted by proximity to the coast"
      )
    ),
    "Wildfire" = list(
      "Change in Fire Probability" = list(
        col = "norm_exp_fire_prob_change", raw_col = "exp_fire_prob_change",
        status = "Implemented",
        raw_label = "% change fire probability (90th percentile of all models)",
        description = "Projected % change in probability of wildfire (90th percentile of all models)"
      ),
      "Fire Model Agreement" = list(
        col = NULL, raw_col = "exp_fire_model_agree",
        status = "Implemented", is_weight = TRUE,
        raw_label = "% models predicting increase",
        description = "% of climate models predicting an increase in fire probability. Applied as a confidence weight on Change in Fire Probability."
      )
    ),
    "Temperature" = list(
      "Change in Mean Annual Temperature" = list(
        col = "norm_exp_temp_change", raw_col = "exp_temp_change",
        status = "Provisional",
        raw_label = "Change in mean annual air temperature (\u00b0C, 90th percentile)",
        description = "Projected increase in mean annual air temperature. A proxy for warming pressure on water quality, not a measurement of source water temperature."
      )
    )
  ),
  
  "Sensitivity" = list(
    "Demand" = list(
      "Historical Visitation Trend" = list(
        col = "norm_sen_visitation_trend", raw_col = "sen_visitation_trend",
        status = "Implemented",
        raw_label = "Scaled visitation trend",
        description = "Historical trend in park visitation (scaled)"
      ),
      "Competition" = list(
        col = "norm_sen_competition", raw_col = "sen_competition",
        status = "Implemented",
        raw_label = "Water use trend slope (per km\u00b2)",
        description = "Trend in nearby county water use (competition for supply)"
      ),
      "Current Supply-Demand Mismatch" = list(
        col = "norm_sen_auc_change", raw_col = "sen_auc_change",
        status = "Provisional",
        raw_label = "Historical monthly timing mismatch AUC",
        description = "Historical area under the curve where monthly visitation share exceeds monthly water supply share. Higher values mean peak visitation already falls in the system's lowest-availability months."
      )
    ),
    "Source Type" = list(
      "Depth to Water Table" = list(
        col = "norm_sen_source_type", raw_col = "sen_source_type",
        status = "Implemented",
        raw_label = "Depth to groundwater (m) at the water source",
        description = "Vulnerability is characterized as depth to the water table (m), with deeper depths representing systems of lower risk. Surface water sources are assigned a depth of 0 m."
      )
    ),
    "Wildfire" = list(
      "Current Wildfire Risk" = list(
        col = "norm_sen_wildfire_hazard", raw_col = "sen_wildfire_hazard",
        status = "Implemented",
        raw_label = "Mean Wildfire Hazard Potential",
        description = "Current Wildfire Hazard Potential index"
      )
    ),
    "Flood" = list(
      "Current Flood Risk" = list(
        col = "norm_sen_flood_risk", raw_col = "sen_flood_risk",
        status = "Implemented",
        raw_label = "% area in FEMA flood zone",
        description = "% of surrounding area in a high-risk FEMA flood zone"
      )
    ),
    "Sea Level Rise" = list(
      "Current Inundation" = list(
        col = "norm_sen_inundation_current", raw_col = "sen_inundation_current",
        status = "Implemented",
        raw_label = "% catchment area inundated (reference)",
        description = "% of catchment area currently inundated (reference condition)"
      )
    ),
    # Section 5.7: one Water Supply Trend sensitivity factor, runoff or
    # precipitation depending on source type. Same structure as the Exposure
    # Water Supply factor above.
    "Water Supply" = list(
      "Historical Runoff Trend" = list(
        col = "norm_sen_runoff_trend", raw_col = "sen_runoff_trend",
        status = "Implemented", source_var = "runoff", supply_role = "trend",
        raw_label = "Sen's slope (30-yr runoff)",
        description = "30-year historical trend in runoff (Sen's slope). Used as the water supply trend for groundwater, spring and surface-water systems."
      ),
      "Historical Precipitation Trend" = list(
        col = "norm_sen_precip_trend", raw_col = "sen_precip_trend",
        status = "Implemented", source_var = "precipitation", supply_role = "trend",
        raw_label = "Sen's slope (30-yr precip)",
        description = "30-year historical trend in precipitation (Sen's slope). Used as the water supply trend for rainwater-harvesting systems only."
      )
    ),
    "Drought" = list(
      "Historical Drought Trend" = list(
        col = "norm_sen_drought_trend", raw_col = "sen_drought_trend",
        status = "Implemented",
        raw_label = "Sen's slope (30-yr SPEI)",
        description = "30-year historical trend in SPEI (water deficit metric for drought)"
      )
    ),
    "Temperature" = list(
      "Historical Temperature Trend" = list(
        col = "norm_sen_temp_trend", raw_col = "sen_temp_trend",
        status = "Provisional",
        raw_label = "Sen's slope (annual mean temperature, \u00b0C/yr)",
        description = "Historical warming trend in annual mean air temperature. Supplies already warming are closer to thresholds for reduced dissolved oxygen and accelerated biological activity."
      )
    )
  )
)

## Drop indicators with no backing column ----
# The indicator dropdown maps straight onto columns of final_indicators.csv. If
# a configured raw_col is not in that file, selecting it fed a NULL into the
# popup builder's Map() call, which returned a zero-length popup vector and left
# the map with no popups at all. exp_fire_model_agree is the live case: the fire
# agreement weight is applied when the indicators are assembled and the column is
# not carried through, so the entry is pruned here rather than being offered and
# then failing. Anything pruned is logged at startup so a missing column shows up
# in the deploy log instead of silently shrinking the menu.
indicator_config <- local({
  cfg_all <- indicator_config
  dropped <- character(0)
  for (cmp in names(cfg_all)) {
    for (fac in names(cfg_all[[cmp]])) {
      keep <- vapply(cfg_all[[cmp]][[fac]], function(cfg) {
        is.null(cfg$raw_col) || cfg$raw_col %in% names(final_indicators)
      }, logical(1))
      if (any(!keep)) {
        dropped <- c(dropped, paste0(cmp, " > ", fac, " > ",
                                     names(cfg_all[[cmp]][[fac]])[!keep]))
        cfg_all[[cmp]][[fac]] <- cfg_all[[cmp]][[fac]][keep]
      }
    }
    # A factor that lost every one of its indicators would leave an empty
    # dropdown, so drop the factor too.
    empt <- vapply(cfg_all[[cmp]], function(f) length(f) == 0, logical(1))
    if (any(empt)) cfg_all[[cmp]] <- cfg_all[[cmp]][!empt]
  }
  if (length(dropped) > 0) {
    message("[WSVA] Indicator(s) hidden -- raw column absent from final_indicators.csv: ",
            paste(dropped, collapse = "; "))
  }
  cfg_all
})

## Source-type conditional indicators ----
# The Water Supply factor is scored on one variable per system, chosen by source
# type, so "Change in Runoff" on the map is an indicator that does not apply to
# every point drawn. That has to be said in the view where the indicator is
# selected, not only in the methods report -- but the sidebar is not the place
# for a paragraph. It is therefore split in two: a two-line chip in the
# description box that answers "does this apply to my system", and the full rule
# in the indicator methodology modal one click away, for the reader who wants to
# know why. SOURCE_VAR_SHORT drives the chip, SOURCE_VAR_APPLIES the long form.
SOURCE_VAR_SHORT <- list(
  runoff        = "groundwater, spring & surface water",
  precipitation = "rainwater harvesting"
)
SOURCE_VAR_APPLIES <- list(
  runoff        = "groundwater, spring, river/stream, lake/reservoir and other non-ocean systems",
  precipitation = "rainwater-harvesting systems only"
)

# How many supplies each branch actually covers, counted from the data rather
# than asserted. "Scored for 1,393 of 1,407 systems" tells a reviewer far more
# in far less space than a sentence describing the rule, and it stays correct if
# the source types in the WSD change. water_supply_var is set by
# calc_vulnerability_index() using the same rule the scores use.
SOURCE_VAR_N <- local({
  v <- combined_data$water_supply_var
  if (is.null(v)) return(NULL)
  list(total         = length(v),
       runoff        = sum(v == "runoff", na.rm = TRUE),
       precipitation = sum(v == "precipitation", na.rm = TRUE),
       none          = sum(v == "none", na.rm = TRUE))
})

#' "1,393 of 1,407 systems", or "" if the counts are unavailable.
source_var_count_text <- function(sv) {
  if (is.null(SOURCE_VAR_N) || is.null(SOURCE_VAR_N[[sv]])) return("")
  paste0(format(SOURCE_VAR_N[[sv]], big.mark = ","), " of ",
         format(SOURCE_VAR_N$total, big.mark = ","), " systems")
}

#' Is this a source-type conditional (Water Supply branch) indicator?
is_source_var <- function(cfg) {
  sv <- cfg$source_var
  !is.null(sv) && !is.na(sv) && sv %in% names(SOURCE_VAR_SHORT)
}

#' Compact two-line marker for the indicator description box: which variable
#' this is, which systems get it, and what the others get. Anything longer than
#' this belongs in the modal, not the sidebar.
source_var_chip <- function(cfg) {
  if (!is_source_var(cfg)) return("")
  sv    <- cfg$source_var
  other <- setdiff(names(SOURCE_VAR_SHORT), sv)
  cnt   <- source_var_count_text(sv)
  paste0(
    "<span class='srcvar-chip'>", toupper(sv), "</span>",
    "<span class='srcvar-rule'>Scored for <b>", SOURCE_VAR_SHORT[[sv]], "</b>",
    if (nzchar(cnt)) paste0(" &mdash; ", cnt) else "", ". ",
    "Others use ", other, "; ocean scores 0.</span>"
  )
}

#' The facts the methods report's own source_note leaves out: how many supplies
#' this branch covers in the loaded data, and that each system contributes one
#' Water Supply value rather than both. Appended to the CSV note in the modal.
source_var_supplement <- function(cfg) {
  if (!is_source_var(cfg)) return("")
  cnt <- source_var_count_text(cfg$source_var)
  paste0(
    if (nzchar(cnt)) paste0("That is ", cnt, " in the current dataset. ") else "",
    "Every system contributes exactly one Water Supply value to its score, never ",
    "both, so a system scored on the other branch is not missing data \u2014 the map ",
    "for this indicator shows the systems it applies to rather than the whole portfolio."
  )
}

#' Full rule, for the indicator methodology modal and report prose. Plain text:
#' the modal escapes it into a table cell.
source_var_detail <- function(cfg) {
  if (!is_source_var(cfg)) return("")
  sv    <- cfg$source_var
  other <- setdiff(names(SOURCE_VAR_SHORT), sv)
  cnt   <- source_var_count_text(sv)
  paste0(
    "The Water Supply factor uses one variable per system, chosen by source type. ",
    "This is the ", sv, " version, and it is the one scored for ",
    SOURCE_VAR_APPLIES[[sv]],
    if (nzchar(cnt)) paste0(" \u2014 ", cnt, " in the current dataset") else "",
    ". Systems that use ", other, " are scored on the ", other,
    " version of the same indicator instead, and ocean sources are assigned 0 because ",
    "their supply is not treated as quantity- or seasonally limited. Every system ",
    "therefore contributes exactly one Water Supply value to its score, never both, so ",
    "the map for this indicator shows the systems it applies to rather than the whole ",
    "portfolio, and a system scored on the other branch is not missing data."
  )
}

## Indicator Status Helpers ----
# Factor-level status mirrors the Technical Methods Report convention: a factor
# built entirely from Implemented indicators is Implemented; one built entirely
# from Provisional indicators is Provisional; a mix is Semi-Provisional.
factor_status <- function(component, factor_name) {
  inds <- indicator_config[[component]][[factor_name]]
  if (is.null(inds)) return(NA_character_)
  st <- vapply(inds, function(x) {
    s <- x$status
    if (is.null(s)) "Implemented" else s
  }, character(1))
  if (all(st == "Implemented")) "Implemented"
  else if (all(st == "Provisional")) "Provisional"
  else "Semi-Provisional"
}

status_badge <- function(status, size = "10px") {
  if (is.null(status) || is.na(status)) return("")
  col <- switch(status,
                "Implemented"      = "#386150",
                "Provisional"      = "#B26B00",
                "Semi-Provisional" = "#8B6914",
                "#696969")
  paste0("<span style='background:", col, ";color:white;padding:1px 7px;",
         "border-radius:10px;font-size:", size, ";font-weight:700;",
         "letter-spacing:0.04em;text-transform:uppercase;white-space:nowrap;'>",
         status, "</span>")
}

# Flat lookup of every scored (non-weight) indicator, used by the normalized
# distribution chart and the report builders.
scored_indicator_index <- local({
  out <- list()
  for (cmp in names(indicator_config)) {
    for (fac in names(indicator_config[[cmp]])) {
      for (ind in names(indicator_config[[cmp]][[fac]])) {
        cfg <- indicator_config[[cmp]][[fac]][[ind]]
        if (isTRUE(cfg$is_weight)) next
        if (is.null(cfg$col)) next
        out[[length(out) + 1]] <- list(
          component = cmp, factor = fac, indicator = ind,
          col = cfg$col, raw_col = cfg$raw_col,
          # NULL for every indicator that applies to all source types; "runoff"
          # or "precipitation" for the two Water Supply branches, so per-site
          # views can drop the branch that does not apply to that system.
          source_var = if (is.null(cfg$source_var)) NA_character_ else cfg$source_var,
          status = if (is.null(cfg$status)) "Implemented" else cfg$status
        )
      }
    }
  }
  out
})

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
  "factor_exp_water_supply" = "Water Supply\n(Exp)",
  "factor_exp_drought"      = "Drought\n(Exp)",
  "factor_exp_slr"          = "Sea Level Rise\n(Exp)",
  "factor_exp_wildfire"     = "Wildfire\n(Exp)",
  "factor_exp_demand"       = "Demand\n(Exp)",
  "factor_exp_temp"         = "Temperature\n(Exp)",
  "factor_sen_demand"       = "Demand\n(Sen)",
  "factor_sen_infrastructure" = "Supply\n(Sen)",
  "factor_sen_wildfire"     = "Wildfire\n(Sen)",
  "factor_sen_flood"        = "Flood\n(Sen)",
  "factor_sen_slr"          = "Sea Level Rise\n(Sen)",
  "factor_sen_water_supply" = "Water Supply\n(Sen)",
  "factor_sen_drought"      = "Drought\n(Sen)",
  "factor_sen_temp"         = "Temperature\n(Sen)"
)

# Report Builders ----
# Shared machinery for every report the app produces: the single-site report
# reached from a map popup, the whole-park comparison report, the multi-park
# comparison report, and the batch report. All four write a self-contained HTML
# document (images inlined as base64) into REPORT_DIR, which is served back to
# the browser for the in-app preview before the user commits to downloading.

REPORT_DIR <- file.path(tempdir(), "wsva_reports")
dir.create(REPORT_DIR, showWarnings = FALSE, recursive = TRUE)
shiny::addResourcePath("wsva_reports", REPORT_DIR)

RPT_EXP_COLOR  <- "#5B8C6E"
RPT_SEN_COLOR  <- "#7C6FAD"
RPT_VULN_COLOR <- "#C05235"
RPT_IND_COLOR  <- "#B7B7B7"
RPT_NA_COLOR   <- "#c9c9c9"

## Shared factor definitions ----
# One definition list, used by the interactive score-breakdown modal AND every
# report, so the two can no longer drift apart (they previously held two
# near-identical copies that had already diverged in factor ordering).
report_factor_defs <- list(
  # One Water Supply factor (Report S4.5), but its two indicators are drawn from
  # a different variable depending on source type. `indicators_by_var` holds both
  # sets and resolve_factor_indicators() picks the applicable one per site;
  # `indicators` is the default (runoff) so any code path that has not been
  # taught about the split still gets the right answer for ~99% of systems.
  list(id = "factor_exp_water_supply", comp = "Exposure", label = "Water Supply",
       type = "euclidean", source_conditional = TRUE,
       indicators = list(
         list(norm_col = "norm_exp_runoff_change",        label = "Amount"),
         list(norm_col = "norm_exp_runoff_timing_change", label = "Timing")
       ),
       indicators_by_var = list(
         runoff = list(
           list(norm_col = "norm_exp_runoff_change",        label = "Amount (runoff)"),
           list(norm_col = "norm_exp_runoff_timing_change", label = "Timing (runoff)")
         ),
         precipitation = list(
           list(norm_col = "norm_exp_precip_change",        label = "Amount (precip)"),
           list(norm_col = "norm_exp_precip_timing_change", label = "Timing (precip)")
         )
       )),
  list(id = "factor_exp_drought", comp = "Exposure", label = "Drought", type = "single"),
  list(id = "factor_exp_demand", comp = "Exposure", label = "Demand", type = "euclidean",
       indicators = list(
         list(norm_col = "norm_exp_nearby_use_change", label = "Nearby Water Use"),
         list(norm_col = "norm_exp_auc_change",        label = "Supply-Demand Mismatch")
       )),
  list(id = "factor_exp_slr", comp = "Exposure", label = "Sea Level Rise", type = "euclidean",
       indicators = list(
         list(norm_col = "norm_exp_inundation_slr", label = "Inundation"),
         list(norm_col = "norm_exp_swi",            label = "Saltwater Intrusion"),
         list(norm_col = "norm_exp_storm_surge",    label = "Storm Surge")
       )),
  list(id = "factor_exp_wildfire", comp = "Exposure", label = "Wildfire", type = "single"),
  list(id = "factor_exp_temp", comp = "Exposure", label = "Temperature", type = "single"),
  list(id = "factor_sen_demand", comp = "Sensitivity", label = "Demand", type = "euclidean",
       indicators = list(
         list(norm_col = "norm_sen_visitation_trend", label = "Visitation Trend"),
         list(norm_col = "norm_sen_competition",      label = "Competition"),
         list(norm_col = "norm_sen_auc_change",       label = "Supply-Demand Mismatch")
       )),
  list(id = "factor_sen_infrastructure", comp = "Sensitivity", label = "Source Type", type = "single"),
  list(id = "factor_sen_wildfire", comp = "Sensitivity", label = "Wildfire", type = "single"),
  list(id = "factor_sen_flood", comp = "Sensitivity", label = "Flood", type = "single"),
  list(id = "factor_sen_slr", comp = "Sensitivity", label = "Sea Level Rise", type = "single"),
  # Report S5.7. Single-indicator factor, but which indicator depends on source
  # type, so it carries the same indicators_by_var map as its Exposure twin --
  # used for labelling rather than for drilling down, since a single-indicator
  # factor has no internal composition to show.
  list(id = "factor_sen_water_supply", comp = "Sensitivity", label = "Water Supply",
       type = "single", source_conditional = TRUE,
       indicators_by_var = list(
         runoff = list(
           list(norm_col = "norm_sen_runoff_trend", label = "Trend (runoff)")
         ),
         precipitation = list(
           list(norm_col = "norm_sen_precip_trend", label = "Trend (precip)")
         )
       )),
  list(id = "factor_sen_drought", comp = "Sensitivity", label = "Drought", type = "single"),
  list(id = "factor_sen_temp", comp = "Sensitivity", label = "Temperature", type = "single")
)

#' Is this factor structurally not applicable to the given source type?
#' Ocean supplies zero out runoff, precip AND demand in calc_vulnerability_index;
#' the demand check was previously missing here, so ocean systems reported a 0%
#' Demand contribution as if it were a real score rather than an N/A.
na_flag_for_source <- function(id, src_type) {
  is_ocn <- !is.na(src_type) && src_type == "ocean"
  # Only ocean systems have a structurally inapplicable factor now. Runoff and
  # precipitation used to be two factors, exactly one of which was N/A for every
  # system; they are one Water Supply factor that resolves to the applicable
  # variable, so the only remaining N/A case is an ocean source, for which both
  # Water Supply and Demand are zeroed in calc_vulnerability_index().
  is_ocn && grepl("water_supply|demand", id)
}

#' Which variable drives the Water Supply factor for a given source type.
#' Mirrors the rule in calc_vulnerability_index(); kept as a function rather
#' than read from the water_supply_var column so report builders that work from
#' combined_raw (unscored) can call it too.
water_supply_var_for <- function(src_type) {
  if (!is.na(src_type) && src_type == "ocean")     return("none")
  if (!is.na(src_type) && src_type == "rainwater") return("precipitation")
  "runoff"
}

#' Resolve a factor definition's indicator list for one source type.
#'
#' Source-conditional factors carry both branches in `indicators_by_var`; every
#' other factor just returns its static `indicators`. Returning an empty list for
#' ocean sources is deliberate -- there is no Water Supply indicator to drill
#' into, and callers already skip factors that na_flag_for_source() marks N/A.
resolve_factor_indicators <- function(d, src_type) {
  if (!isTRUE(d$source_conditional)) return(d$indicators)
  v <- water_supply_var_for(src_type)
  if (v == "none") return(list())
  d$indicators_by_var[[v]]
}

#' Display label for a factor, naming the variable actually used where that is
#' source-type dependent. "Water Supply (runoff)" is a more useful axis label
#' than "Water Supply" when the reader is looking at one specific site.
factor_display_label <- function(d, src_type) {
  if (!isTRUE(d$source_conditional)) return(d$label)
  v <- water_supply_var_for(src_type)
  if (v == "none") return(d$label)
  paste0(d$label, " (", if (v == "precipitation") "precip" else v, ")")
}

## Water rights context ----
# State water law doctrine attached to each supply. This is CONTEXT, not a
# determination: SurfaceWaterLaw / GroundWaterLaw describe the legal regime of
# the state the supply sits in, and state_water_rights_id is the system's own
# permit or right identifier where the WSD records one (it is blank for roughly
# four in five systems). Water Rights is a "Planned" indicator in Table 1 of the
# Technical Methods Report -- none of this enters any score, and the app must not
# imply otherwise.
#
# Which doctrine is the operative one depends on the source: groundwater and
# springs fall under the groundwater regime, everything else under surface water.
# Springs are legally ambiguous in several states, so they are labelled as
# groundwater with the ambiguity stated rather than asserted away.
wr_primary_regime <- function(source_type) {
  if (is.na(source_type)) return(NA_character_)
  st <- tolower(source_type)
  if (grepl("groundwater|well|spring", st)) "groundwater"
  else if (grepl("ocean|reclaimed", st))    NA_character_
  else                                       "surface"
}

WR_DISCLAIMER <- paste(
  "State legal framework for context only. This describes the water law doctrine of the",
  "state the supply is located in; it is not a determination of this system's water right,",
  "priority date, or allocation. Water Rights is a Planned indicator and contributes nothing",
  "to any score. Confirm rights status with the park's water rights records and the state",
  "administering agency."
)

#' Fetch a column as character, returning an all-NA vector of the right length
#' when the column is absent. Map() over a NULL argument yields a zero-length
#' result, which is how a single missing metadata column can wipe out every popup
#' on the map, so metadata reads go through this rather than `df[[col]]`.
col_missing <- function(df, col) {
  if (!col %in% names(df)) return(rep(NA_character_, nrow(df)))
  as.character(df[[col]])
}

#' Division that returns 0 rather than NaN/Inf when the denominator is 0.
#' Deliberately NOT written with ifelse(): ifelse() truncates its result to the
#' length of the test argument, which silently reduced vector numerators to a
#' single element.
rpt_safe_div <- function(num, den) { out <- num / den; out[den == 0] <- 0; out }

## Factor contribution table for one scored site row ----
build_factor_contrib_df <- function(site_row, src_type) {
  one <- function(comp_name) {
    defs     <- Filter(function(d) d$comp == comp_name, report_factor_defs)
    fac_cols <- vapply(defs, function(d) d$id, character(1))
    vals     <- as.numeric(site_row[1, fac_cols])
    contrib  <- 100 * rpt_safe_div(vals^2, sum(vals^2, na.rm = TRUE))
    suffix   <- if (comp_name == "Exposure") " (E)" else " (S)"
    rows <- lapply(seq_along(defs), function(i) {
      d <- defs[[i]]
      status <- if (na_flag_for_source(d$id, src_type)) "n/a"
      else if (is.na(vals[i])) "missing" else "scored"
      data.frame(
        factor_id     = d$id,
        display_label = paste0(factor_display_label(d, src_type), suffix),
        status        = status,
        contrib       = if (status == "scored") contrib[i] else NA_real_,
        component     = comp_name,
        stringsAsFactors = FALSE
      )
    })
    d_out <- do.call(rbind, rows)
    status_rank <- match(d_out$status, c("scored", "n/a", "missing"))
    d_out[order(status_rank, -ifelse(is.na(d_out$contrib), -Inf, d_out$contrib)), ]
  }
  rbind(one("Exposure"), one("Sensitivity"))
}

## Static component-contribution bar chart -> base64 PNG ----
build_contrib_chart_b64 <- function(site_row, src_type) {
  contrib_df <- build_factor_contrib_df(site_row, src_type)
  contrib_df$bar_x <- ifelse(contrib_df$status == "scored", contrib_df$contrib, 0)
  contrib_df$fill_color <- ifelse(
    contrib_df$status != "scored", RPT_NA_COLOR,
    ifelse(contrib_df$component == "Exposure", RPT_EXP_COLOR, RPT_SEN_COLOR))
  contrib_df$bar_label <- ifelse(
    contrib_df$status == "scored", sprintf("%.0f%%", contrib_df$contrib),
    ifelse(contrib_df$status == "n/a", "N/A", "No data"))
  contrib_df$order_key <- with(contrib_df,
                               ifelse(component == "Exposure", 1, 2) * 1000 - ifelse(is.na(bar_x), 0, bar_x))
  contrib_df <- contrib_df[order(contrib_df$order_key), ]
  contrib_df$display_label <- factor(contrib_df$display_label,
                                     levels = rev(contrib_df$display_label))
  
  ymax <- max(contrib_df$bar_x, na.rm = TRUE)
  if (!is.finite(ymax) || ymax <= 0) ymax <- 1
  
  p <- ggplot2::ggplot(contrib_df,
                       ggplot2::aes(x = display_label, y = bar_x, fill = fill_color)) +
    ggplot2::geom_col(width = 0.65) +
    ggplot2::geom_text(ggplot2::aes(label = bar_label), hjust = -0.1,
                       size = 3.2, color = "#333333") +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_y_continuous(limits = c(0, ymax * 1.25), expand = c(0, 0)) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = "% Contribution to Component Score") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      axis.text.y        = ggplot2::element_text(size = 9),
      plot.background    = ggplot2::element_rect(fill = "white", color = NA),
      panel.background   = ggplot2::element_rect(fill = "white", color = NA)
    )
  
  f <- tempfile(fileext = ".png")
  ggplot2::ggsave(f, p, width = 7.5, height = 5.2, dpi = 150, bg = "white")
  base64enc::base64encode(f)
}

## Static all-levels icicle chart + companion table ----
build_icicle_assets <- function(site_row, src_type) {
  ids <- character(0); labels <- character(0); parents <- character(0)
  values <- numeric(0); colors <- character(0); levels_ <- integer(0)
  ic_add <- function(id, label, parent, value, color, level) {
    ids     <<- c(ids, id);        labels  <<- c(labels, label)
    parents <<- c(parents, parent); values <<- c(values, value)
    colors  <<- c(colors, color);   levels_ <<- c(levels_, level)
  }
  
  exp_defs <- Filter(function(d) d$comp == "Exposure",    report_factor_defs)
  sen_defs <- Filter(function(d) d$comp == "Sensitivity", report_factor_defs)
  exp_vals <- as.numeric(site_row[1, sapply(exp_defs, function(d) d$id)])
  sen_vals <- as.numeric(site_row[1, sapply(sen_defs, function(d) d$id)])
  exp_denom <- sum(exp_vals^2, na.rm = TRUE)
  sen_denom <- sum(sen_vals^2, na.rm = TRUE)
  vuln_denom <- site_row$EXPOSURE[1]^2 + site_row$SENSITIVITY[1]^2
  exp_share <- 100 * rpt_safe_div(site_row$EXPOSURE[1]^2, vuln_denom)
  sen_share <- 100 * rpt_safe_div(site_row$SENSITIVITY[1]^2, vuln_denom)
  
  ic_add("Vulnerability", "Vulnerability", "", 100, RPT_VULN_COLOR, 0)
  ic_add("Exposure", "Exposure", "Vulnerability", exp_share, RPT_EXP_COLOR, 1)
  ic_add("Sensitivity", "Sensitivity", "Vulnerability", sen_share, RPT_SEN_COLOR, 1)
  
  place <- function(defs, vals, denom, comp_id, comp_share, color) {
    scored <- !sapply(defs, function(d) na_flag_for_source(d$id, src_type)) & !is.na(vals)
    for (i in seq_along(defs)) {
      if (!scored[i]) next
      d <- defs[[i]]
      suffix <- if (comp_id == "Exposure") " (E)" else " (S)"
      lbl <- paste0(factor_display_label(d, src_type), suffix)
      fid <- paste0(comp_id, "/", lbl)
      fv <- rpt_safe_div(vals[i]^2, denom) * comp_share
      ic_add(fid, lbl, comp_id, fv, color, 2)
      if (d$type == "euclidean") {
        inds <- resolve_factor_indicators(d, src_type)
        nv <- vapply(inds, function(ind) {
          v <- site_row[[ind$norm_col]]
          if (is.null(v)) NA_real_ else as.numeric(v[1])
        }, numeric(1))
        denom_nv <- sum(nv^2, na.rm = TRUE)
        sh <- rpt_safe_div(nv^2, denom_nv)
        for (j in seq_along(inds)) {
          if (is.na(nv[j])) next
          ind <- inds[[j]]
          ic_add(paste0(fid, "/", ind$label), ind$label, fid, sh[j] * fv, RPT_IND_COLOR, 3)
        }
      }
    }
  }
  place(exp_defs, exp_vals, exp_denom, "Exposure",    exp_share, RPT_EXP_COLOR)
  place(sen_defs, sen_vals, sen_denom, "Sensitivity", sen_share, RPT_SEN_COLOR)
  
  tree <- data.frame(id = ids, label = labels, parent = parents, value = values,
                     color = colors, level = levels_, stringsAsFactors = FALSE)
  tree$y0 <- NA_real_; tree$y1 <- NA_real_
  root_idx <- which(tree$parent == "")
  tree$y0[root_idx] <- 0; tree$y1[root_idx] <- 100
  
  recurse <- function(tr, parent_id) {
    kids <- which(tr$parent == parent_id)
    if (length(kids) == 0) return(tr)
    p_y0 <- tr$y0[tr$id == parent_id]; p_y1 <- tr$y1[tr$id == parent_id]
    total <- sum(tr$value[kids]); cursor <- p_y0
    kids <- rev(kids)
    for (k in kids) {
      h <- if (total == 0) 0 else (tr$value[k] / total) * (p_y1 - p_y0)
      tr$y0[k] <- cursor; tr$y1[k] <- cursor + h
      cursor <- cursor + h
      tr <- recurse(tr, tr$id[k])
    }
    tr
  }
  tree <- recurse(tree, tree$id[root_idx])
  tree$y0 <- pmin(pmax(tree$y0, 0), 100)
  tree$y1 <- pmin(pmax(tree$y1, 0), 100)
  
  col_bounds <- list("0" = c(0.78, 1.0), "1" = c(0.52, 0.76),
                     "2" = c(0.26, 0.50), "3" = c(0.0, 0.24))
  bounds <- do.call(rbind, col_bounds[as.character(tree$level)])
  tree$xmin <- bounds[, 1]; tree$xmax <- bounds[, 2]
  tree$box_height <- tree$y1 - tree$y0
  tree$show_label <- tree$box_height >= 3.5
  tree$label_only <- ifelse(
    tree$box_height >= 8, sprintf("%s\n%.1f%%", tree$label, tree$value),
    ifelse(tree$show_label, sprintf("%s %.0f%%", tree$label, tree$value), ""))
  
  p <- ggplot2::ggplot(tree) +
    ggplot2::geom_rect(ggplot2::aes(xmin = xmin, xmax = xmax, ymin = y0, ymax = y1,
                                    fill = color), color = "white", linewidth = 0.6) +
    ggplot2::geom_text(data = subset(tree, show_label),
                       ggplot2::aes(x = (xmin + xmax) / 2, y = (y0 + y1) / 2,
                                    label = label_only),
                       size = 2.8, color = "white", lineheight = 0.85, fontface = "bold") +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0),
                                breaks = c(0.12, 0.38, 0.64, 0.89),
                                labels = c("Indicators", "Factors", "Components", "Vulnerability")) +
    ggplot2::scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      axis.text.x      = ggplot2::element_text(size = 9, face = "bold", color = "#1D3557"),
      axis.text.y      = ggplot2::element_blank(),
      axis.ticks       = ggplot2::element_blank(),
      panel.grid       = ggplot2::element_blank(),
      plot.background  = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA)
    )
  
  f <- tempfile(fileext = ".png")
  ggplot2::ggsave(f, p, width = 8, height = 4.6, dpi = 150, bg = "white")
  
  tbl_rows <- tree[tree$level %in% c(2, 3), ]
  tbl_rows <- tbl_rows[order(match(tbl_rows$parent, c("Exposure", "Sensitivity")),
                             -tbl_rows$value), ]
  tbl_html <- paste0(
    "<table style='width:100%;max-width:460px;border-collapse:collapse;font-size:12px;margin:10px 0;'>",
    "<thead><tr style='border-bottom:2px solid #1D3557;'>",
    "<th style='padding:4px 8px;text-align:left;'>Factor / Indicator</th>",
    "<th style='padding:4px 8px;text-align:right;'>% of Vulnerability</th></tr></thead><tbody>",
    paste0("<tr><td style='padding:4px 8px;",
           ifelse(tbl_rows$level == 3, "padding-left:24px;color:#666;", "font-weight:600;"),
           "'>", htmltools::htmlEscape(tbl_rows$label), "</td>",
           "<td style='padding:4px 8px;text-align:right;'>",
           sprintf("%.2f%%", tbl_rows$value), "</td></tr>", collapse = ""),
    "</tbody></table>")
  
  list(b64 = base64enc::base64encode(f), table_html = tbl_html)
}

## Report maps ----
# Point coordinates for the report maps. combined_data is an sf object, so the
# geometry is pulled once here rather than round-tripping through the data
# frames the report builders already work with.
report_site_coords <- function(ids) {
  idx <- match(as.character(ids), as.character(combined_data$wsd_source_id))
  xy  <- sf::st_coordinates(combined_data)
  data.frame(wsd_source_id = as.character(ids),
             lon = xy[idx, 1], lat = xy[idx, 2],
             stringsAsFactors = FALSE)
}

# State outlines, fetched once per session. maps:: is already a soft dependency
# of the map-export feature and is wrapped the same way, so a missing package
# degrades to a map without a basemap rather than a failed report.
report_states_sf <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    cached <<- tryCatch({
      sf::st_as_sf(maps::map("state", fill = TRUE, plot = FALSE)) %>% sf::st_set_crs(4326)
    }, error = function(e) NA)
    if (identical(cached, NA)) NULL else cached
  }
})

# Park polygon lookup. park_boundaries carries UNIT_NAME (the long park name),
# so matching is on park_name rather than the 4-letter unit code.
report_park_boundary <- function(park_name) {
  if (is.null(park_name) || is.na(park_name)) return(NULL)
  hit <- tryCatch(park_boundaries[park_boundaries$UNIT_NAME == park_name, ],
                  error = function(e) NULL)
  if (is.null(hit) || nrow(hit) == 0) NULL else hit
}

#' North arrow and scale bar layers for a geographic ggplot.
#'
#' Hand-built rather than pulled from ggspatial: the app already ships a long
#' dependency list to shinyapps.io, and adding a package for two decorations is
#' a deploy risk out of proportion to the payoff. These are plain annotate()
#' layers, so they work anywhere ggplot2 does.
#'
#' Both shapes are sized in DATA units (degrees), which matters because coord_sf
#' applies a latitude-dependent aspect ratio to geographic coordinates. A shape
#' defined as a square in degrees renders as a tall rectangle. Every horizontal
#' dimension is therefore divided by cos(latitude) so the arrow reads as the
#' intended shape at any latitude in CONUS, from south Florida to the Canadian
#' border.
#'
#' @param xlim,ylim  the same limits handed to coord_sf
#' @param deco_scale text and shape multiplier; drop below 1 for small panels
map_decorations <- function(xlim, ylim, deco_scale = 1,
                            arrow = TRUE, scalebar = TRUE) {
  if (!all(is.finite(c(xlim, ylim)))) return(list())
  dx <- diff(xlim); dy <- diff(ylim)
  if (!is.finite(dx) || !is.finite(dy) || dx <= 0 || dy <= 0) return(list())
  
  lat_mid <- mean(ylim)
  # Guard against a degenerate cosine near the poles; irrelevant for CONUS but
  # cheap, and it keeps the helper safe if the tool is ever extended to Alaska.
  cosl <- max(cos(lat_mid * pi / 180), 0.15)
  
  ink   <- "#1D3557"
  layers <- list()
  
  ## Scale bar (bottom left) ----
  if (isTRUE(scalebar)) {
    span_km <- dx * 111.320 * cosl
    target  <- span_km * 0.25
    # Round DOWN to a conventional bar length so the label is always a clean
    # number; a bar labelled "37 km" looks like a bug even when it is accurate.
    nice <- c(0.1, 0.25, 0.5, 1, 2, 5, 10, 25, 50, 100, 200, 250, 500, 1000, 2000)
    ok   <- nice[nice <= target]
    bar_km  <- if (length(ok)) max(ok) else min(nice)
    bar_deg <- bar_km / (111.320 * cosl)
    # Never let the bar run past the panel if the extent is tiny.
    bar_deg <- min(bar_deg, dx * 0.42)
    
    x0 <- xlim[1] + dx * 0.05
    y0 <- ylim[1] + dy * 0.06
    h  <- dy * 0.014
    xm <- x0 + bar_deg / 2
    x1 <- x0 + bar_deg
    lbl <- if (bar_km < 1) paste0(bar_km, " km") else paste0(round(bar_km), " km")
    
    # No backdrop panel. The opaque white boxes read as UI chrome pasted onto
    # the map rather than cartography, so legibility over dark markers is bought
    # with white outlines on the marks themselves instead.
    layers <- c(layers, list(
      ggplot2::annotate("rect", xmin = x0, xmax = xm, ymin = y0, ymax = y0 + h,
                        fill = ink, color = "white", linewidth = 0.55),
      ggplot2::annotate("rect", xmin = xm, xmax = x1, ymin = y0, ymax = y0 + h,
                        fill = "white", color = ink, linewidth = 0.4),
      ggplot2::annotate("text", x = x0, y = y0 + h + dy * 0.028, label = "0",
                        size = 2.2 * deco_scale, color = ink, hjust = 0.5),
      ggplot2::annotate("text", x = x1, y = y0 + h + dy * 0.028, label = lbl,
                        size = 2.2 * deco_scale, color = ink, hjust = 0.5)
    ))
  }
  
  ## North arrow (bottom right) ----
  if (isTRUE(arrow)) {
    ah <- dy * 0.08                 # visual height, in latitude degrees
    aw <- (ah * 0.52) / cosl        # width corrected for the coord_sf aspect
    cx <- xlim[2] - dx * 0.07
    cy <- ylim[1] + dy * 0.06
    
    layers <- c(layers, list(
      # Kite arrowhead: apex, left foot, centre notch, right foot. The white
      # stroke is what separates it from a dark basemap or an overlapping
      # marker, replacing the backdrop panel.
      ggplot2::annotate("polygon",
                        x = c(cx, cx - aw / 2, cx, cx + aw / 2),
                        y = c(cy + ah, cy, cy + ah * 0.3, cy),
                        fill = ink, color = "white", linewidth = 0.55),
      ggplot2::annotate("text", x = cx, y = cy + ah + dy * 0.036, label = "N",
                        size = 2.7 * deco_scale, fontface = "bold", color = ink)
    ))
  }
  
  layers
}

#' Vulnerability map for one set of water supplies, zoomed to their extent.
#'
#' @param pts_df   scored data frame containing wsd_source_id and rank_col
#' @param rank_col which percentile rank drives colour and size
#' @param legend_name label for the colour bar
build_vulnerability_map_b64 <- function(pts_df, rank_col = "VULNERABILITY_rank",
                                        title = NULL, subtitle = NULL,
                                        park_name = NULL, legend_name = "Vulnerability\npercentile",
                                        width = 7.5, height = 5.5,
                                        show_labels = TRUE, show_legend = TRUE,
                                        deco_scale = 1) {
  d <- as.data.frame(pts_df)
  co <- report_site_coords(d$wsd_source_id)
  d  <- merge(d, co, by = "wsd_source_id", all.x = TRUE)
  d$rank_val <- suppressWarnings(as.numeric(d[[rank_col]]))
  d <- d[is.finite(d$lon) & is.finite(d$lat), ]
  if (nrow(d) == 0) return(NULL)
  
  bnd <- report_park_boundary(park_name)
  
  # Extent: points plus any park polygon, padded. The minimum span keeps a park
  # with a single supply (or several nearly co-located ones) from zooming to a
  # degenerate window where the basemap renders as flat colour.
  xr <- range(d$lon); yr <- range(d$lat)
  if (!is.null(bnd)) {
    bb <- sf::st_bbox(bnd)
    xr <- range(c(xr, bb[["xmin"]], bb[["xmax"]]))
    yr <- range(c(yr, bb[["ymin"]], bb[["ymax"]]))
  }
  pad_x <- max(diff(xr) * 0.18, 0.05)
  pad_y <- max(diff(yr) * 0.18, 0.05)
  xlim  <- c(xr[1] - pad_x, xr[2] + pad_x)
  ylim  <- c(yr[1] - pad_y, yr[2] + pad_y)
  
  p <- ggplot2::ggplot()
  st <- report_states_sf()
  if (!is.null(st)) {
    p <- p + ggplot2::geom_sf(data = st, fill = "#eef3f7", color = "#c4cfd7",
                              linewidth = 0.3, inherit.aes = FALSE)
  }
  if (!is.null(bnd)) {
    p <- p + ggplot2::geom_sf(data = bnd, fill = "#A8DADC", alpha = 0.28,
                              color = "#1D3557", linewidth = 0.45, inherit.aes = FALSE)
  }
  
  p <- p +
    ggplot2::geom_point(
      data = d[is.na(d$rank_val), , drop = FALSE],
      ggplot2::aes(x = lon, y = lat),
      shape = 21, fill = "#d9d9d9", color = "#666666", size = 2.4, stroke = 0.4) +
    ggplot2::geom_point(
      data = d[!is.na(d$rank_val), , drop = FALSE],
      ggplot2::aes(x = lon, y = lat, fill = rank_val, size = rank_val),
      shape = 21, color = "black", stroke = 0.45, alpha = 0.92) +
    ggplot2::scale_fill_gradientn(
      colors = c("#FFF3D6", "#F0C75E", "#DD8844", "#C05235", "#9B2226"),
      limits = c(0, 100), na.value = "#d9d9d9", name = legend_name,
      guide = if (show_legend)
        ggplot2::guide_colorbar(barwidth = 0.7, barheight = 7, title.position = "top")
      else "none") +
    ggplot2::scale_size_continuous(range = c(2.2, 6.5), limits = c(0, 100), guide = "none")
  
  # Labels only when they will be legible; beyond ~20 supplies they collapse
  # into a smear and the accompanying table is the better reference anyway.
  if (show_labels && nrow(d) <= 20) {
    p <- p + ggplot2::geom_text(
      data = d, ggplot2::aes(x = lon, y = lat, label = wsd_source_id),
      size = 2.1, vjust = -1.1, color = "#1D3557", check_overlap = TRUE)
  }
  
  # Added after the data layers so the decorations sit on top of the markers,
  # and before coord_sf, which then clips them to the same extent.
  p <- p + map_decorations(xlim, ylim, deco_scale = deco_scale)
  
  p <- p +
    ggplot2::coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    ggplot2::labs(x = NULL, y = NULL, title = title, subtitle = subtitle) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(size = 11, face = "bold", color = "#1D3557"),
      plot.subtitle    = ggplot2::element_text(size = 8.5, color = "#696969"),
      axis.text        = ggplot2::element_text(size = 7, color = "#999"),
      panel.grid.major = ggplot2::element_line(color = "#e8e8e8", linewidth = 0.25),
      panel.border     = ggplot2::element_rect(color = "#bbb", fill = NA, linewidth = 0.4),
      legend.title     = ggplot2::element_text(size = 8, face = "bold"),
      legend.text      = ggplot2::element_text(size = 7.5),
      plot.background  = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "#f7fbfd", color = NA)
    )
  
  f <- tempfile(fileext = ".png")
  ggplot2::ggsave(f, p, width = width, height = height, dpi = 150,
                  bg = "white", limitsize = FALSE)
  base64enc::base64encode(f)
}

#' Small-multiple grid of per-park maps.
#'
#' ggplot2's facet_wrap cannot give each panel its own geographic extent when
#' coord_sf is in play (free scales and a fixed CRS aspect ratio fight each
#' other), so each park is rendered as its own plot and the "facetting" is done
#' with a CSS grid in the report. Every panel shares one colour scale fixed to
#' 0-100 national percentile, so panels stay comparable to each other.
build_park_facet_maps_html <- function(scored_sub, park_codes, max_panels = 12) {
  park_codes <- park_codes[park_codes %in% unique(scored_sub$park_unit)]
  truncated  <- length(park_codes) > max_panels
  shown      <- utils::head(park_codes, max_panels)
  
  panels <- vapply(shown, function(pu) {
    d <- scored_sub[!is.na(scored_sub$park_unit) & scored_sub$park_unit == pu, ]
    if (nrow(d) == 0) return("")
    b64 <- tryCatch(
      build_vulnerability_map_b64(
        d, rank_col = "VULNERABILITY_rank",
        title = paste0(pu, "  (n = ", nrow(d), ")"),
        park_name = d$park_name[1],
        width = 4.6, height = 3.6,
        show_labels = FALSE, show_legend = FALSE, deco_scale = 0.8),
      error = function(e) NULL)
    if (is.null(b64)) {
      return(paste0("<div style='background:#f7fafc;border:1px dashed #c1d5e0;border-radius:6px;",
                    "padding:20px;text-align:center;font-size:11px;color:#999;'>",
                    htmltools::htmlEscape(pu), ": map unavailable</div>"))
    }
    paste0("<div style='background:white;border:1px solid #dce8ef;border-radius:6px;padding:6px;'>",
           "<img src='data:image/png;base64,", b64, "' style='width:100%;display:block;' /></div>")
  }, character(1))
  
  # One shared legend strip, since the panels themselves suppress theirs.
  legend_strip <- paste0(
    "<div style='display:flex;align-items:center;gap:8px;margin:10px 0 4px 0;font-size:11px;color:#555;'>",
    "<span>Less vulnerable</span>",
    "<span style='flex:0 0 180px;height:12px;border:1px solid #bbb;border-radius:3px;",
    "background:linear-gradient(90deg,#FFF3D6,#F0C75E,#DD8844,#C05235,#9B2226);'></span>",
    "<span>More vulnerable</span>",
    "<span style='margin-left:10px;'>National vulnerability percentile (0-100), shared across all panels</span>",
    "</div>")
  
  paste0(
    legend_strip,
    "<div style='display:grid;grid-template-columns:repeat(2,1fr);gap:12px;margin:10px 0;'>",
    paste(panels, collapse = ""), "</div>",
    if (truncated) paste0(
      "<p style='font-size:11px;color:#8B6914;'>Showing the first ", max_panels,
      " of ", length(park_codes), " selected park units to keep the report readable. ",
      "The tables above cover every selected park.</p>") else "")
}

## Shared report stylesheet ----
report_css <- function() paste0(
  "<style>",
  "body{font-family:Arial,Helvetica,sans-serif;max-width:900px;margin:0 auto;padding:30px 40px;color:#333;line-height:1.5;}",
  "h1{color:#1D3557;font-size:24px;margin:0 0 4px 0;}",
  "h2{color:#1D3557;font-size:17px;border-bottom:2px solid #2a7f7f;padding-bottom:4px;margin-top:28px;}",
  "h3{color:#1D3557;font-size:15px;margin-top:22px;}",
  ".subtitle{color:#386150;font-size:16px;font-weight:600;margin:0 0 2px 0;}",
  ".meta{color:#696969;font-size:13px;margin:0 0 8px 0;}",
  ".accent-line{height:3px;background:linear-gradient(90deg,#1D3557,#2a7f7f,#A8DADC);border-radius:2px;margin:12px 0 20px 0;}",
  "table{border-collapse:collapse;width:100%;}",
  "thead tr{border-bottom:2px solid #1D3557;}",
  "tbody tr{border-bottom:1px solid #eee;}",
  ".info-table td{padding:5px 10px;font-size:13px;}",
  ".info-table td:first-child{font-weight:600;color:#1D3557;width:150px;}",
  ".cmp-table th{padding:7px 8px;font-size:12px;color:#1D3557;text-align:center;background:#f0f5f8;}",
  ".cmp-table td{padding:6px 8px;font-size:12px;text-align:center;}",
  ".cmp-table td.l{text-align:left;}",
  ".note{background:#fff8e6;border-left:3px solid #8B6914;border-radius:4px;padding:10px 14px;font-size:12px;color:#5a4a1a;margin:14px 0;}",
  ".footer{margin-top:30px;padding-top:12px;border-top:1px solid #ddd;text-align:center;font-size:11px;color:#999;}",
  ".footer a{color:#457B9D;text-decoration:none;}",
  ".footer a:hover{text-decoration:underline;}",
  ".page-break{page-break-before:always;}",
  "@media print{body{padding:20px;}}",
  "</style>")

report_footer <- function() paste0(
  "<div class='footer'>",
  "NPS Water Supply Vulnerability Assessment Tool<br>",
  "Generated from <a href='", APP_URL, "'>", APP_URL_DISPLAY, "</a> on ",
  format(Sys.Date(), "%B %d, %Y"), "<br>",
  "Colorado State University Geospatial Centroid &nbsp;|&nbsp; ", format(Sys.Date(), "%B %Y"),
  "</div>")

report_provisional_note <- function() paste0(
  "<div class='note'><b>Development status.</b> The scores in this report include ",
  "indicators the Technical Methods Report classifies as <b>Provisional</b> ",
  "(supply and demand mismatch, water supply timing, storm surge, and air ",
  "temperature). Those indicators are calculated for all applicable supplies and ",
  "are included in every score shown here, but their methodology is still being ",
  "revised and their values are expected to change. Indicators classified as ",
  "<b>Planned</b> are not calculated and contribute nothing. Scores are relative ",
  "to the comparison group stated above, not absolute measures of risk.</div>")

#' Format an optional metadata value for a report table. Blank, NA and
#' whitespace-only entries all render as an explicit "Not recorded" rather than
#' an empty cell, so a reader can tell a missing record from a rendering bug.
rpt_val <- function(x, fallback = "<span style='color:#999;'>Not recorded</span>") {
  if (length(x) == 0 || is.na(x) || !nzchar(trimws(as.character(x)))) return(fallback)
  htmltools::htmlEscape(trimws(as.character(x)))
}

## Rank extraction against an arbitrary scored subset ----
rpt_extract_ranks <- function(scored_df, sid) {
  row <- scored_df[scored_df$wsd_source_id == sid, ]
  if (nrow(row) == 0) {
    return(list(vuln_rank = NA, exp_rank = NA, sen_rank = NA,
                n = nrow(scored_df), priority = NA))
  }
  list(
    vuln_rank = round(row$VULNERABILITY_rank[1], 1),
    exp_rank  = round(row$EXPOSURE_rank[1], 1),
    sen_rank  = round(row$SENSITIVITY_rank[1], 1),
    n         = nrow(scored_df),
    priority  = isTRUE(row$priority_group[1])
  )
}

rpt_flag_badges <- function(row) {
  flags <- c(
    if (isTRUE(row$flag_fire[1]))    "\U0001F525 Fire"           else NULL,
    if (isTRUE(row$flag_flood[1]))   "\U0001F4A7 Flood"          else NULL,
    if (isTRUE(row$flag_slr[1]))     "\U0001F30A Sea Level Rise" else NULL,
    if (isTRUE(row$flag_drought[1])) "\u2600\uFE0F Drought"      else NULL
  )
  cols <- c("\U0001F525 Fire" = "#C05235", "\U0001F4A7 Flood" = "#457B9D",
            "\U0001F30A Sea Level Rise" = "#1D3557", "\u2600\uFE0F Drought" = "#8B6914")
  if (length(flags) == 0) return("")
  badges <- sapply(flags, function(f) {
    col <- if (f %in% names(cols)) cols[[f]] else "#666"
    paste0("<span style='background:", col, ";color:white;padding:3px 10px;",
           "border-radius:4px;font-size:12px;margin-right:4px;'>", f, "</span>")
  })
  paste0("<div style='margin:10px 0;'>", paste(badges, collapse = " "), "</div>")
}

rpt_flag_text <- function(row) {
  f <- c(if (isTRUE(row$flag_fire[1]))    "Fire"    else NULL,
         if (isTRUE(row$flag_flood[1]))   "Flood"   else NULL,
         if (isTRUE(row$flag_slr[1]))     "SLR"     else NULL,
         if (isTRUE(row$flag_drought[1])) "Drought" else NULL)
  if (length(f) == 0) "\u2014" else paste(f, collapse = ", ")
}

## Single-site report body (no <html> wrapper, so it can be concatenated) ----
build_site_report_body <- function(site_id, include_header = TRUE) {
  site_meta   <- combined_raw[combined_raw$wsd_source_id == site_id, ]
  if (nrow(site_meta) == 0) return(paste0("<p>Site ", site_id, " not found.</p>"))
  site_park   <- site_meta$park_unit[1]
  site_state  <- site_meta$state[1]
  site_region <- site_meta$region[1]
  src_type    <- site_meta$source_type[1]
  sys_type    <- if (is.na(src_type)) "N/A" else src_type
  desc_text   <- if (is.na(site_meta$description[1]) ||
                     nchar(trimws(site_meta$description[1])) == 0) "" else site_meta$description[1]
  
  national_df <- as.data.frame(combined_data)
  national    <- rpt_extract_ranks(national_df, site_id)
  site_scored <- national_df[national_df$wsd_source_id == site_id, ]
  
  scope_subset <- function(mask) {
    sub <- combined_raw[mask, ]
    if (nrow(sub) >= 2) rpt_extract_ranks(calc_vulnerability_index(sub), site_id)
    else list(vuln_rank = NA, exp_rank = NA, sen_rank = NA, n = nrow(sub), priority = NA)
  }
  regional    <- scope_subset(combined_raw$region    == site_region)
  state_ranks <- scope_subset(combined_raw$state     == site_state)
  park_ranks  <- scope_subset(combined_raw$park_unit == site_park)
  
  contrib_b64 <- build_contrib_chart_b64(site_scored, src_type)
  icicle      <- build_icicle_assets(site_scored, src_type)
  
  make_rank_box <- function(label, ranks, color) {
    rank_val <- if (is.na(ranks$vuln_rank)) "N/A" else paste0(ranks$vuln_rank, "%")
    badge <- if (isTRUE(ranks$priority))
      "<span style='background:#9B2226;color:white;padding:2px 6px;border-radius:3px;font-size:10px;'>TOP PRIORITY</span>" else ""
    paste0("<div style='flex:1;background:white;border-radius:8px;padding:14px 12px;border-top:4px solid ",
           color, ";box-shadow:0 2px 6px rgba(0,0,0,0.08);text-align:center;min-width:140px;'>",
           "<div style='font-size:11px;color:#696969;text-transform:uppercase;letter-spacing:0.05em;margin-bottom:4px;'>",
           htmltools::htmlEscape(label), "</div>",
           "<div style='font-size:28px;font-weight:800;color:", color, ";'>", rank_val, "</div>",
           "<div style='font-size:11px;color:#999;margin-top:2px;'>n = ", ranks$n, " sites</div>",
           badge, "</div>")
  }
  rank_boxes <- paste0(
    "<div style='display:flex;gap:12px;flex-wrap:wrap;margin:16px 0;'>",
    make_rank_box(paste0("Within Park (", site_park, ")"), park_ranks, "#2a7f7f"),
    make_rank_box(paste0("Within State (", site_state, ")"), state_ranks, "#457B9D"),
    make_rank_box(paste0("Within Region (", site_region, ")"), regional, "#1D3557"),
    make_rank_box("Nationally", national, "#386150"), "</div>")
  
  make_rank_row <- function(label, ranks) {
    fmt <- function(v) if (is.na(v)) "--" else paste0(v, "%")
    paste0("<tr><td class='l' style='padding:6px 10px;font-weight:600;'>",
           htmltools::htmlEscape(label), "</td>",
           "<td style='padding:6px 10px;text-align:center;'>", fmt(ranks$vuln_rank), "</td>",
           "<td style='padding:6px 10px;text-align:center;color:#457B9D;'>", fmt(ranks$exp_rank), "</td>",
           "<td style='padding:6px 10px;text-align:center;color:#C05235;'>", fmt(ranks$sen_rank), "</td>",
           "<td style='padding:6px 10px;text-align:center;'>", ranks$n, "</td></tr>")
  }
  rank_table <- paste0(
    "<table class='cmp-table' style='width:100%;border-collapse:collapse;font-size:13px;margin:12px 0;'>",
    "<thead><tr style='background:#f0f5f8;border-bottom:2px solid #1D3557;'>",
    "<th style='text-align:left;'>Scope</th><th>Vulnerability</th><th>Exposure</th>",
    "<th>Sensitivity</th><th>N Sites</th></tr></thead><tbody>",
    make_rank_row(paste0("Within Park (", site_park, ")"), park_ranks),
    make_rank_row(paste0("Within State (", site_state, ")"), state_ranks),
    make_rank_row(paste0("Within Region (", site_region, ")"), regional),
    make_rank_row("Nationally", national),
    "</tbody></table>")
  
  desc_html <- if (nchar(desc_text) > 0) {
    paste0("<div style='background:#f7fafc;border-left:3px solid #2a7f7f;padding:10px 14px;",
           "border-radius:4px;margin:10px 0;font-size:13px;color:#333;line-height:1.5;'>",
           "<strong>System Description:</strong> ", htmltools::htmlEscape(desc_text), "</div>")
  } else ""
  
  # Water rights context. Reported alongside the site metadata rather than in the
  # scoring sections, because none of it enters a score -- Water Rights is a
  # Planned indicator. The doctrine governing this source type is marked so the
  # reader is not left guessing which of the two rows is the operative one.
  regime    <- wr_primary_regime(src_type)
  rpt_mark  <- function(which_one) {
    if (!is.na(regime) && identical(regime, which_one))
      " <span style='background:#2a7f7f;color:white;padding:1px 5px;border-radius:3px;font-size:10px;'>APPLIES TO THIS SOURCE</span>"
    else ""
  }
  rights_html <- paste0(
    "<h2>Water Rights Context</h2>",
    "<table class='info-table'>",
    "<tr><td>Surface Water Law</td><td>", rpt_val(site_meta$SurfaceWaterLaw[1]), rpt_mark("surface"), "</td></tr>",
    "<tr><td>Groundwater Law</td><td>", rpt_val(site_meta$GroundWaterLaw[1]), rpt_mark("groundwater"), "</td></tr>",
    "<tr><td>State Water Right ID</td><td>", rpt_val(site_meta$state_water_rights_id[1]), "</td></tr>",
    "</table>",
    "<div class='note' style='background:#eef4fa;border-left-color:#457B9D;color:#24425f;'>",
    htmltools::htmlEscape(WR_DISCLAIMER), "</div>")
  
  header <- if (include_header) paste0(
    "<h1>Water Supply Vulnerability Report</h1>",
    "<p class='subtitle'>", htmltools::htmlEscape(site_meta$park_name[1]), "</p>",
    "<p class='meta'>", htmltools::htmlEscape(site_meta$water_system_name[1]),
    " &nbsp;|&nbsp; ", site_id, " &nbsp;|&nbsp; Generated ",
    format(Sys.Date(), "%B %d, %Y"), "</p>",
    "<div class='accent-line'></div>") else paste0(
      "<h2 style='border-bottom:3px solid #1D3557;'>", htmltools::htmlEscape(site_meta$park_name[1]),
      " &mdash; ", site_id, "</h2>")
  
  paste0(
    header,
    "<h2>Site Information</h2>",
    "<table class='info-table'>",
    "<tr><td>Water System</td><td>", htmltools::htmlEscape(site_meta$water_system_name[1]), "</td></tr>",
    "<tr><td>Park Unit</td><td>", htmltools::htmlEscape(site_park), "</td></tr>",
    "<tr><td>Park Name</td><td>", htmltools::htmlEscape(site_meta$park_name[1]), "</td></tr>",
    "<tr><td>State</td><td>", htmltools::htmlEscape(site_state), "</td></tr>",
    "<tr><td>Region</td><td>", htmltools::htmlEscape(site_region), "</td></tr>",
    "<tr><td>Source Type</td><td>", htmltools::htmlEscape(sys_type), "</td></tr>",
    "<tr><td>FMSS Number</td><td>", rpt_val(site_meta$fmss_number[1]), "</td></tr>",
    "</table>", desc_html, rights_html, rpt_flag_badges(site_scored),
    "<h2>Priority Rankings <em>(higher percentile = higher relative risk)</em></h2>",
    "<p style='font-size:12px;color:#696969;margin-top:-4px;'>Vulnerability percentile rank at four geographic scopes. A rank of 90 means the supply scores higher than 90% of the comparison group.</p>",
    rank_boxes, rank_table,
    "<h2>Component Contribution</h2>",
    "<p style='font-size:12px;color:#696969;margin-top:-4px;'>Each factor's share of this site's Exposure or Sensitivity score, calculated nationally. Green = Exposure, Purple = Sensitivity.</p>",
    "<img src='data:image/png;base64,", contrib_b64,
    "' style='width:100%;max-width:750px;display:block;margin:12px auto;' />",
    "<h2>All Levels Contribution</h2>",
    "<p style='font-size:12px;color:#696969;margin-top:-4px;'>Nested view from raw indicators through factors and components to the overall Vulnerability score.</p>",
    "<img src='data:image/png;base64,", icicle$b64,
    "' style='width:100%;max-width:750px;display:block;margin:12px auto;' />",
    icicle$table_html)
}

wrap_report_html <- function(title, body) paste0(
  "<!DOCTYPE html><html lang='en'><head><meta charset='utf-8'>",
  "<title>", htmltools::htmlEscape(title), "</title>", report_css(),
  "</head><body>", body, report_provisional_note(), report_footer(), "</body></html>")

## Single-site report ----
build_site_report_html <- function(site_id) {
  site_meta <- combined_raw[combined_raw$wsd_source_id == site_id, ]
  ttl <- paste0("Vulnerability Report - ", site_meta$park_name[1])
  wrap_report_html(ttl, build_site_report_body(site_id, include_header = TRUE))
}

## Comparison chart across a set of scored supplies ----
build_supply_comparison_chart_b64 <- function(scored_df, label_col = "wsd_source_id",
                                              title = NULL, height = NULL) {
  d <- data.frame(
    label = as.character(scored_df[[label_col]]),
    vuln  = as.numeric(scored_df$VULNERABILITY_rank),
    exp   = as.numeric(scored_df$EXPOSURE_rank),
    sen   = as.numeric(scored_df$SENSITIVITY_rank),
    stringsAsFactors = FALSE
  )
  d <- d[order(d$vuln, decreasing = FALSE), ]
  d$label <- factor(d$label, levels = d$label)
  long <- rbind(
    data.frame(label = d$label, metric = "Vulnerability", value = d$vuln),
    data.frame(label = d$label, metric = "Exposure",      value = d$exp),
    data.frame(label = d$label, metric = "Sensitivity",   value = d$sen)
  )
  long$metric <- factor(long$metric, levels = c("Vulnerability", "Exposure", "Sensitivity"))
  
  p <- ggplot2::ggplot(long, ggplot2::aes(x = label, y = value, fill = metric)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.75) +
    # Vulnerability uses RPT_VULN_COLOR so this chart matches the All Levels
    # Contribution chart and the popup, where Vulnerability is already the
    # red/orange root box. It was previously the same green family as Exposure,
    # which read as "Exposure, twice".
    ggplot2::scale_fill_manual(values = c("Vulnerability" = RPT_VULN_COLOR,
                                          "Exposure" = RPT_EXP_COLOR,
                                          "Sensitivity" = RPT_SEN_COLOR), name = NULL) +
    ggplot2::scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = "National percentile rank", title = title) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position  = "top",
      axis.text.y      = ggplot2::element_text(size = 8),
      panel.grid.major.y = ggplot2::element_blank(),
      plot.title       = ggplot2::element_text(size = 12, face = "bold", color = "#1D3557"),
      plot.background  = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA)
    )
  h <- if (is.null(height)) max(3, min(14, 0.28 * nrow(d) + 1.5)) else height
  f <- tempfile(fileext = ".png")
  ggplot2::ggsave(f, p, width = 8, height = h, dpi = 150, bg = "white", limitsize = FALSE)
  base64enc::base64encode(f)
}

## Factor heatmap across a set of scored supplies ----
build_factor_heatmap_b64 <- function(scored_df, label_col = "wsd_source_id") {
  fac_ids <- vapply(report_factor_defs, function(d) d$id, character(1))
  fac_ids <- fac_ids[fac_ids %in% names(scored_df)]
  if (length(fac_ids) == 0) return(NULL)
  lbls <- vapply(report_factor_defs, function(d) {
    paste0(d$label, if (d$comp == "Exposure") " (E)" else " (S)")
  }, character(1))
  names(lbls) <- vapply(report_factor_defs, function(d) d$id, character(1))
  
  m   <- as.data.frame(scored_df)[, fac_ids, drop = FALSE]
  src <- as.character(as.data.frame(scored_df)$source_type)
  
  long <- do.call(rbind, lapply(fac_ids, function(fid) {
    v <- suppressWarnings(as.numeric(m[[fid]]))
    # calc_vulnerability_index ZEROES factors that do not apply to a source type
    # (precip for anything that is not rainwater, runoff for rainwater, and
    # runoff/precip/demand for ocean). A zero is a real value to the colour
    # scale, so those cells rendered as pale yellow -- indistinguishable from a
    # genuinely low score, and wrong: at MORA every Precip cell looked like a
    # scored 0 rather than "not applicable". Re-apply the same N/A test the
    # contribution charts use so those cells drop to the grey na.value instead.
    na_mask <- vapply(src, function(st) na_flag_for_source(fid, st), logical(1))
    v[na_mask] <- NA_real_
    data.frame(site = as.character(scored_df[[label_col]]),
               factor_lbl = lbls[[fid]],
               value = v,
               stringsAsFactors = FALSE)
  }))
  long$factor_lbl <- factor(long$factor_lbl, levels = rev(unname(lbls[fac_ids])))
  
  p <- ggplot2::ggplot(long, ggplot2::aes(x = site, y = factor_lbl, fill = value)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.4) +
    ggplot2::scale_fill_gradientn(
      colors = c("#FFF3D6", "#F0C75E", "#DD8844", "#C05235", "#9B2226"),
      limits = c(0, 1), na.value = "#e8e8e8", name = "Factor\nscore") +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      axis.text.x      = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7),
      axis.text.y      = ggplot2::element_text(size = 8),
      panel.grid       = ggplot2::element_blank(),
      plot.background  = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA)
    )
  w <- max(7, min(18, 0.22 * length(unique(long$site)) + 3))
  f <- tempfile(fileext = ".png")
  ggplot2::ggsave(f, p, width = w, height = 5.5, dpi = 150, bg = "white", limitsize = FALSE)
  base64enc::base64encode(f)
}

## Supply comparison table ----
build_supply_table_html <- function(scored_df, show_park = FALSE) {
  d <- as.data.frame(scored_df)
  d <- d[order(-d$VULNERABILITY_rank), ]
  fmt <- function(v) if (is.na(v)) "--" else sprintf("%.1f", v)
  rows <- vapply(seq_len(nrow(d)), function(i) {
    r <- d[i, , drop = FALSE]
    prio <- if (isTRUE(r$priority_group[1]))
      "<span style='background:#9B2226;color:white;padding:1px 6px;border-radius:3px;font-size:10px;'>HIGH</span>"
    else "\u2014"
    # Water rights is summarised to the single operative doctrine here rather
    # than showing both columns: a comparison table across dozens of supplies has
    # no room for two law columns, and the doctrine that does not govern the
    # source type is noise in that context. The full pair is in the single-site
    # report.
    reg  <- wr_primary_regime(r$source_type[1])
    lawv <- if (is.na(reg)) NA_character_
    else if (identical(reg, "groundwater")) as.character(r$GroundWaterLaw[1])
    else as.character(r$SurfaceWaterLaw[1])
    paste0("<tr>",
           "<td class='l'>", htmltools::htmlEscape(as.character(r$wsd_source_id[1])), "</td>",
           if (show_park) paste0("<td class='l'>", htmltools::htmlEscape(as.character(r$park_unit[1])), "</td>") else "",
           "<td class='l'>", htmltools::htmlEscape(as.character(r$water_system_name[1])), "</td>",
           "<td class='l'>", rpt_val(r$fmss_number[1], "\u2014"), "</td>",
           "<td class='l'>", htmltools::htmlEscape(ifelse(is.na(r$source_type[1]), "N/A", as.character(r$source_type[1]))), "</td>",
           "<td class='l' style='font-size:11px;'>", rpt_val(lawv, "\u2014"), "</td>",
           "<td><b>", fmt(r$VULNERABILITY_rank[1]), "</b></td>",
           "<td style='color:#457B9D;'>", fmt(r$EXPOSURE_rank[1]), "</td>",
           "<td style='color:#C05235;'>", fmt(r$SENSITIVITY_rank[1]), "</td>",
           "<td>", prio, "</td>",
           "<td>", rpt_flag_text(r), "</td></tr>")
  }, character(1))
  paste0("<table class='cmp-table'><thead><tr>",
         "<th style='text-align:left;'>Supply ID</th>",
         if (show_park) "<th style='text-align:left;'>Park</th>" else "",
         "<th style='text-align:left;'>Water System</th>",
         "<th style='text-align:left;'>FMSS</th>",
         "<th style='text-align:left;'>Source</th>",
         "<th style='text-align:left;'>Water Law</th>",
         "<th>Vuln.</th><th>Exp.</th><th>Sen.</th><th>Priority</th><th>Hazard Flags</th>",
         "</tr></thead><tbody>", paste(rows, collapse = ""), "</tbody></table>",
         "<p style='font-size:11px;color:#888;margin:4px 0 0 0;'>Water Law shows the state ",
         "doctrine governing this source type (groundwater doctrine for groundwater and spring ",
         "sources, surface water doctrine otherwise). Context only; contributes nothing to any score.</p>")
}

## Whole-park report: every supply in one park, compared ----
build_park_report_html <- function(park_unit_code) {
  nat <- as.data.frame(combined_data)
  sub <- nat[!is.na(nat$park_unit) & nat$park_unit == park_unit_code, ]
  if (nrow(sub) == 0) return(wrap_report_html("Park Report", "<p>No water supplies found for this park unit.</p>"))
  
  park_name <- sub$park_name[1]
  region    <- sub$region[1]
  states    <- paste(sort(unique(na.omit(sub$state))), collapse = ", ")
  
  # Within-park scores: recalculated so ranks are relative to this park alone,
  # shown alongside the national ranks rather than replacing them.
  raw_sub <- combined_raw[!is.na(combined_raw$park_unit) & combined_raw$park_unit == park_unit_code, ]
  within  <- if (nrow(raw_sub) >= 2) {
    tryCatch(calc_vulnerability_index(raw_sub), error = function(e) NULL)
  } else NULL
  
  n_priority <- sum(isTRUE(TRUE) & !is.na(sub$priority_group) & sub$priority_group)
  flag_counts <- c(
    Fire    = sum(!is.na(sub$flag_fire)    & sub$flag_fire),
    Flood   = sum(!is.na(sub$flag_flood)   & sub$flag_flood),
    SLR     = sum(!is.na(sub$flag_slr)     & sub$flag_slr),
    Drought = sum(!is.na(sub$flag_drought) & sub$flag_drought)
  )
  
  summary_cards <- paste0(
    "<div style='display:flex;gap:12px;flex-wrap:wrap;margin:16px 0;'>",
    paste0("<div style='flex:1;min-width:130px;background:white;border-radius:8px;padding:14px 12px;",
           "border-top:4px solid #1D3557;box-shadow:0 2px 6px rgba(0,0,0,0.08);text-align:center;'>",
           "<div style='font-size:11px;color:#696969;text-transform:uppercase;'>Water Supplies</div>",
           "<div style='font-size:28px;font-weight:800;color:#1D3557;'>", nrow(sub), "</div></div>"),
    paste0("<div style='flex:1;min-width:130px;background:white;border-radius:8px;padding:14px 12px;",
           "border-top:4px solid #9B2226;box-shadow:0 2px 6px rgba(0,0,0,0.08);text-align:center;'>",
           "<div style='font-size:11px;color:#696969;text-transform:uppercase;'>High Priority (National)</div>",
           "<div style='font-size:28px;font-weight:800;color:#9B2226;'>", n_priority, "</div></div>"),
    paste0("<div style='flex:1;min-width:130px;background:white;border-radius:8px;padding:14px 12px;",
           "border-top:4px solid #386150;box-shadow:0 2px 6px rgba(0,0,0,0.08);text-align:center;'>",
           "<div style='font-size:11px;color:#696969;text-transform:uppercase;'>Median Vuln. Rank</div>",
           "<div style='font-size:28px;font-weight:800;color:#386150;'>",
           sprintf("%.1f", median(sub$VULNERABILITY_rank, na.rm = TRUE)), "</div></div>"),
    paste0("<div style='flex:1;min-width:130px;background:white;border-radius:8px;padding:14px 12px;",
           "border-top:4px solid #C05235;box-shadow:0 2px 6px rgba(0,0,0,0.08);text-align:center;'>",
           "<div style='font-size:11px;color:#696969;text-transform:uppercase;'>Max Vuln. Rank</div>",
           "<div style='font-size:28px;font-weight:800;color:#C05235;'>",
           sprintf("%.1f", max(sub$VULNERABILITY_rank, na.rm = TRUE)), "</div></div>"),
    "</div>")
  
  flags_summary <- paste0(
    "<table class='cmp-table' style='max-width:480px;'><thead><tr>",
    "<th style='text-align:left;'>Hazard Flag</th><th>Supplies Flagged</th><th>% of Park</th>",
    "</tr></thead><tbody>",
    paste0(vapply(names(flag_counts), function(k) paste0(
      "<tr><td class='l'>", k, "</td><td>", flag_counts[[k]], "</td><td>",
      sprintf("%.0f%%", 100 * flag_counts[[k]] / nrow(sub)), "</td></tr>"), character(1)),
      collapse = ""),
    "</tbody></table>")
  
  cmp_b64 <- build_supply_comparison_chart_b64(sub, "wsd_source_id",
                                               title = paste0("National percentile ranks, ", park_unit_code, " water supplies"))
  heat_b64 <- build_factor_heatmap_b64(sub, "wsd_source_id")
  
  # Within-park map. Ranks come from the recalculated within-park scores where
  # the park has enough supplies to rank, so the map answers the same question
  # as the Within-Park Ranking table: which of MY systems is worst. Falls back to
  # national ranks for single-supply parks, and says which it is drawing.
  map_src <- if (!is.null(within)) {
    m <- as.data.frame(within)
    m$park_name <- sub$park_name[1]
    m
  } else sub
  map_rank_basis <- if (!is.null(within)) "within-park" else "national"
  park_map_b64 <- tryCatch(
    build_vulnerability_map_b64(
      map_src, rank_col = "VULNERABILITY_rank",
      title = paste0(park_unit_code, " water supplies"),
      subtitle = paste0("Coloured and sized by ", map_rank_basis,
                        " vulnerability percentile rank"),
      park_name = sub$park_name[1],
      legend_name = paste0(if (map_rank_basis == "within-park") "Within-park" else "National",
                           "\nvulnerability\npercentile"),
      width = 7.5, height = 5.5),
    error = function(e) NULL)
  
  within_html <- if (!is.null(within)) {
    w <- as.data.frame(within)
    w <- w[order(-w$VULNERABILITY_rank), c("wsd_source_id", "VULNERABILITY_rank",
                                           "EXPOSURE_rank", "SENSITIVITY_rank")]
    paste0(
      "<h2>Within-Park Ranking <em>(recalculated within this park)</em></h2>",
      "<p style='font-size:12px;color:#696969;margin-top:-4px;'>Scores recalculated using only this park's supplies as the comparison group. ",
      "These ranks answer \"which of my systems is most vulnerable relative to my other systems\" and are ",
      "not comparable to the national ranks above.</p>",
      "<table class='cmp-table'><thead><tr><th style='text-align:left;'>Supply ID</th>",
      "<th>Vuln.</th><th>Exp.</th><th>Sen.</th></tr></thead><tbody>",
      paste0(vapply(seq_len(nrow(w)), function(i) paste0(
        "<tr><td class='l'>", htmltools::htmlEscape(as.character(w$wsd_source_id[i])), "</td><td><b>",
        sprintf("%.1f", w$VULNERABILITY_rank[i]), "</b></td><td>",
        sprintf("%.1f", w$EXPOSURE_rank[i]), "</td><td>",
        sprintf("%.1f", w$SENSITIVITY_rank[i]), "</td></tr>"), character(1)), collapse = ""),
      "</tbody></table>")
  } else {
    "<h2>Within-Park Ranking</h2><p style='font-size:12px;color:#696969;'>This park has a single water supply, so within-park ranking is not meaningful. Every score in this report is therefore national.</p>"
  }
  
  body <- paste0(
    "<h1>Park Water Supply Vulnerability Report</h1>",
    "<p class='subtitle'>", htmltools::htmlEscape(park_name), " (", htmltools::htmlEscape(park_unit_code), ")</p>",
    "<p class='meta'>", htmltools::htmlEscape(region), " &nbsp;|&nbsp; ", htmltools::htmlEscape(states),
    " &nbsp;|&nbsp; Generated ", format(Sys.Date(), "%B %d, %Y"), "</p>",
    "<div class='accent-line'></div>",
    # This report deliberately mixes two comparison groups: national scoring for
    # everything that should be comparable outside the park, and recalculated
    # within-park scoring for the two sections that answer "which of my systems
    # is worst". Readers hit both within a few pages, so the basis for each
    # section is stated up front rather than left to the individual captions.
    "<h2>How to Read This Report</h2>",
    "<p style='font-size:12px;color:#696969;margin-top:-4px;'>Scores in this assessment are <b>relative</b>, so every number ",
    "depends on the group it was compared against. This report uses two different groups, and each section below says which ",
    "one it is using.</p>",
    "<table class='cmp-table' style='max-width:640px;'><thead><tr>",
    "<th style='text-align:left;'>Section</th><th style='text-align:left;'>Comparison group</th>",
    "<th style='text-align:left;'>Answers</th></tr></thead><tbody>",
    "<tr><td class='l'>Park Summary</td><td class='l'>National</td>",
    "<td class='l'>How does this park sit nationally?</td></tr>",
    "<tr><td class='l'>Hazard Flag Summary</td><td class='l'>National</td>",
    "<td class='l'>Which hazards trigger here, on national thresholds?</td></tr>",
    "<tr><td class='l'>All Water Supplies</td><td class='l'>National</td>",
    "<td class='l'>How do these supplies rank against all CONUS NPS supplies?</td></tr>",
    "<tr><td class='l'>Factor Score Comparison</td><td class='l'>National</td>",
    "<td class='l'>Which factors are high here, on a national 0-1 scale?</td></tr>",
    "<tr style='background:#f7fafc;'><td class='l'>Water Supply Map</td><td class='l'><b>",
    if (!is.null(within)) "Within-park (recalculated)" else "National",
    "</b></td><td class='l'>",
    if (!is.null(within)) "Which of my systems is most vulnerable relative to my others?"
    else "Single-supply park, so national ranks are shown.", "</td></tr>",
    "<tr style='background:#f7fafc;'><td class='l'>Within-Park Ranking</td><td class='l'><b>Within-park (recalculated)</b></td>",
    "<td class='l'>Which of my systems should I look at first?</td></tr>",
    "</tbody></table>",
    "<p style='font-size:12px;color:#696969;'>The two shaded rows are the only sections rescored using this park alone. ",
    "A supply can therefore be near the top within the park while sitting mid-range nationally, or the reverse. Neither ",
    "number is wrong; they answer different questions, so quote the one that matches the decision you are making.</p>",
    "<h2>Park Summary <em>(national scoring)</em></h2>", summary_cards,
    "<h2>Hazard Flag Summary <em>(national scoring)</em></h2>",
    "<p style='font-size:12px;color:#696969;margin-top:-4px;'>Counts of supplies triggering each hazard flag (factor score in the top 25% nationally), evaluated nationally.</p>",
    flags_summary,
    if (!is.null(park_map_b64)) paste0(
      "<h2>Water Supply Map <em>(", map_rank_basis, " scoring)</em></h2>",
      "<p style='font-size:12px;color:#696969;margin-top:-4px;'>Each water supply in the park, coloured and sized by its <b>",
      map_rank_basis, "</b> vulnerability percentile rank. Larger, darker circles are more vulnerable. ",
      "Grey circles have no score. The shaded polygon is the park boundary where one could be matched.</p>",
      "<img src='data:image/png;base64,", park_map_b64,
      "' style='width:100%;max-width:780px;display:block;margin:12px auto;' />") else "",
    "<h2>All Water Supplies (National Ranks)</h2>",
    "<p style='font-size:12px;color:#696969;margin-top:-4px;'>Percentile ranks against all CONUS NPS water supplies. Higher = more vulnerable.</p>",
    build_supply_table_html(sub, show_park = FALSE),
    "<img src='data:image/png;base64,", cmp_b64,
    "' style='width:100%;max-width:800px;display:block;margin:16px auto;' />",
    if (!is.null(heat_b64)) paste0(
      "<h2>Factor Score Comparison <em>(national scoring)</em></h2>",
      "<p style='font-size:12px;color:#696969;margin-top:-4px;'>Normalized factor scores (0-1) for every supply in the park. ",
      "<b>These are national scores.</b> Each factor was min-max normalized across all CONUS NPS water supplies, so a value of ",
      "1.0 means that supply is at the national maximum for that factor, and a park whose cells are uniformly pale is genuinely ",
      "low-risk on that factor rather than merely lowest among its neighbours. They are <i>not</i> renormalized within the park: ",
      "doing so would force one supply to 0.0 and another to 1.0 on every single factor, which for a park with a handful of ",
      "supplies manufactures contrast that is not in the data. Grey cells are not applicable to the supply's source type ",
      "(for example precipitation factors on a groundwater system) or have no data.</p>",
      "<img src='data:image/png;base64,", heat_b64,
      "' style='width:100%;max-width:850px;display:block;margin:12px auto;' />") else "",
    within_html)
  
  wrap_report_html(paste0("Park Report - ", park_name), body)
}

## Multi-park comparison report ----
build_multipark_report_html <- function(park_unit_codes) {
  nat <- as.data.frame(combined_data)
  sub <- nat[!is.na(nat$park_unit) & nat$park_unit %in% park_unit_codes, ]
  if (nrow(sub) == 0) return(wrap_report_html("Multi-Park Report", "<p>No water supplies found for the selected parks.</p>"))
  
  park_rows <- lapply(split(sub, sub$park_unit), function(d) {
    data.frame(
      park_unit = d$park_unit[1],
      park_name = d$park_name[1],
      region    = d$region[1],
      n         = nrow(d),
      med_vuln  = median(d$VULNERABILITY_rank, na.rm = TRUE),
      max_vuln  = max(d$VULNERABILITY_rank, na.rm = TRUE),
      med_exp   = median(d$EXPOSURE_rank, na.rm = TRUE),
      med_sen   = median(d$SENSITIVITY_rank, na.rm = TRUE),
      n_prio    = sum(!is.na(d$priority_group) & d$priority_group),
      n_flag    = sum((!is.na(d$flag_fire) & d$flag_fire) |
                        (!is.na(d$flag_flood) & d$flag_flood) |
                        (!is.na(d$flag_slr) & d$flag_slr) |
                        (!is.na(d$flag_drought) & d$flag_drought)),
      stringsAsFactors = FALSE)
  })
  park_df <- do.call(rbind, park_rows)
  park_df <- park_df[order(-park_df$med_vuln), ]
  
  park_table <- paste0(
    "<table class='cmp-table'><thead><tr>",
    "<th style='text-align:left;'>Park Unit</th><th style='text-align:left;'>Park Name</th>",
    "<th style='text-align:left;'>Region</th><th>Supplies</th><th>Median Vuln.</th>",
    "<th>Max Vuln.</th><th>Median Exp.</th><th>Median Sen.</th>",
    "<th>High Priority</th><th>Any Hazard Flag</th></tr></thead><tbody>",
    paste0(vapply(seq_len(nrow(park_df)), function(i) {
      r <- park_df[i, ]
      paste0("<tr><td class='l'><b>", htmltools::htmlEscape(r$park_unit), "</b></td>",
             "<td class='l'>", htmltools::htmlEscape(r$park_name), "</td>",
             "<td class='l'>", htmltools::htmlEscape(r$region), "</td>",
             "<td>", r$n, "</td>",
             "<td><b>", sprintf("%.1f", r$med_vuln), "</b></td>",
             "<td>", sprintf("%.1f", r$max_vuln), "</td>",
             "<td style='color:#457B9D;'>", sprintf("%.1f", r$med_exp), "</td>",
             "<td style='color:#C05235;'>", sprintf("%.1f", r$med_sen), "</td>",
             "<td>", r$n_prio, "</td><td>", r$n_flag, "</td></tr>")
    }, character(1)), collapse = ""),
    "</tbody></table>")
  
  # Distribution of supply-level vulnerability rank within each selected park
  bx <- data.frame(park = factor(sub$park_unit, levels = rev(park_df$park_unit)),
                   value = as.numeric(sub$VULNERABILITY_rank))
  p_box <- ggplot2::ggplot(bx, ggplot2::aes(x = park, y = value)) +
    ggplot2::geom_boxplot(fill = "#A8DADC", color = "#1D3557", outlier.shape = NA, width = 0.6) +
    ggplot2::geom_jitter(width = 0.12, height = 0, size = 1.4, alpha = 0.7, color = "#C05235") +
    ggplot2::geom_hline(yintercept = 75, linetype = "dashed", color = "#9B2226", linewidth = 0.4) +
    ggplot2::scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = "National vulnerability percentile rank",
                  title = "Distribution of supply vulnerability by park",
                  subtitle = "Dashed line = 75th percentile national high-priority threshold") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(size = 12, face = "bold", color = "#1D3557"),
      plot.subtitle    = ggplot2::element_text(size = 9, color = "#696969"),
      panel.grid.major.y = ggplot2::element_blank(),
      plot.background  = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA)
    )
  f <- tempfile(fileext = ".png")
  ggplot2::ggsave(f, p_box, width = 8, height = max(3.5, min(14, 0.55 * nrow(park_df) + 2)),
                  dpi = 150, bg = "white", limitsize = FALSE)
  box_b64 <- base64enc::base64encode(f)
  
  facet_maps_html <- tryCatch(
    build_park_facet_maps_html(sub, park_df$park_unit),
    error = function(e) NULL)
  
  top_n <- min(25, nrow(sub))
  top_sub <- sub[order(-sub$VULNERABILITY_rank), ][seq_len(top_n), ]
  
  body <- paste0(
    "<h1>Multi-Park Water Supply Comparison</h1>",
    "<p class='subtitle'>", length(unique(sub$park_unit)), " park units &nbsp;|&nbsp; ",
    nrow(sub), " water supplies</p>",
    "<p class='meta'>", htmltools::htmlEscape(paste(sort(unique(sub$park_unit)), collapse = ", ")),
    " &nbsp;|&nbsp; Generated ", format(Sys.Date(), "%B %d, %Y"), "</p>",
    "<div class='accent-line'></div>",
    "<h2>How to Read This Report</h2>",
    "<p style='font-size:12px;color:#696969;margin-top:-4px;'><b>Every score in this report is national.</b> Factor scores, ",
    "component scores and percentile ranks were all normalized across all CONUS NPS water supplies, which is what makes the ",
    "parks below directly comparable to one another. Nothing here is renormalized within a park or within the selection. If ",
    "you need to know which supply is worst <i>inside</i> one park, build a whole-park report for that park instead: it adds ",
    "a recalculated within-park ranking alongside the national one.</p>",
    "<h2>Park-Level Summary <em>(national scoring)</em></h2>",
    "<p style='font-size:12px;color:#696969;margin-top:-4px;'>All ranks are national percentiles across all CONUS NPS water supplies, so parks are directly comparable to one another. Sorted by median vulnerability.</p>",
    park_table,
    "<img src='data:image/png;base64,", box_b64,
    "' style='width:100%;max-width:800px;display:block;margin:16px auto;' />",
    if (!is.null(facet_maps_html)) paste0(
      "<h2>Water Supply Maps by Park <em>(national scoring)</em></h2>",
      "<p style='font-size:12px;color:#696969;margin-top:-4px;'>One panel per park unit, each zoomed to that park's own extent. ",
      "All panels share a single colour scale fixed to the <b>national</b> vulnerability percentile (0-100), so a dark circle ",
      "means the same thing in every panel and the panels can be read against one another. Note that the panels are not ",
      "at a common map scale &mdash; each is zoomed to fit its park, so circle spacing is not comparable between panels.</p>",
      facet_maps_html) else "",
    "<h2>Most Vulnerable Supplies Across Selection <em>(national scoring)</em></h2>",
    "<p style='font-size:12px;color:#696969;margin-top:-4px;'>Top ", top_n,
    " individual water supplies by national vulnerability rank.</p>",
    build_supply_table_html(top_sub, show_park = TRUE))
  
  wrap_report_html("Multi-Park Comparison Report", body)
}

## Batch report: one full single-site section per supply, page-broken ----
build_batch_report_html <- function(site_ids, scope_label = "") {
  n <- length(site_ids)
  sections <- vapply(seq_along(site_ids), function(i) {
    paste0(if (i > 1) "<div class='page-break'></div>" else "",
           build_site_report_body(site_ids[i], include_header = FALSE))
  }, character(1))
  
  toc <- paste0(
    "<h2>Contents</h2><ol style='font-size:13px;color:#333;'>",
    paste0(vapply(site_ids, function(sid) {
      m <- combined_raw[combined_raw$wsd_source_id == sid, ]
      paste0("<li>", htmltools::htmlEscape(as.character(m$park_name[1])), " &mdash; ",
             htmltools::htmlEscape(as.character(sid)), "</li>")
    }, character(1)), collapse = ""),
    "</ol>")
  
  body <- paste0(
    "<h1>Batch Water Supply Vulnerability Report</h1>",
    "<p class='subtitle'>", n, " water supplies</p>",
    "<p class='meta'>", htmltools::htmlEscape(scope_label),
    " &nbsp;|&nbsp; Generated ", format(Sys.Date(), "%B %d, %Y"), "</p>",
    "<div class='accent-line'></div>", toc,
    paste(sections, collapse = ""))
  
  wrap_report_html("Batch Vulnerability Report", body)
}

## Write a report to the served directory and return its metadata ----
stage_report <- function(html, filename_stem) {
  token <- paste0(filename_stem, "_",
                  format(Sys.time(), "%Y%m%d%H%M%S"), "_",
                  paste0(sample(c(letters, 0:9), 6, replace = TRUE), collapse = ""))
  fname <- paste0(token, ".html")
  path  <- file.path(REPORT_DIR, fname)
  writeLines(html, path, useBytes = TRUE)
  list(path = path, url = paste0("wsva_reports/", fname),
       download_name = paste0(filename_stem, ".html"))
}


# UI ----

ui <- navbarPage(
  # The navbar brand is an icon rather than text, which means `title` is an HTML
  # tag object rather than a character string. Shiny derives the browser <title>
  # from `title` when windowTitle is not given, and a tag object stringifies to
  # its own markup -- which is why the published tab read `<i class="fa fa...`.
  # windowTitle supplies the plain-text name explicitly and is what the tab, the
  # bookmark label and the browser history entry all use.
  title = HTML("<i class=\"fa fa-tint\" style=\"color:#A8DADC; font-size:1.3rem;\" aria-hidden=\"true\"></i><span class=\"sr-only\">NPS Water Supply Vulnerability Assessment Tool</span>"),
  windowTitle = "NPS Water Supply Vulnerability Assessment Tool",
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
      /* actionButton() always emits 'btn btn-default action-button' and then
         APPENDS whatever class you pass, so a btn-primary button carries both
         classes. .btn-default's color:#1D3557 is declared later and has equal
         specificity, so it won every time -- navy text on a navy fill, i.e.
         invisible until :hover repainted it. Raising the primary rules to
         .btn.btn-primary (specificity 0,2,0) settles it without needing the
         call sites to stop passing btn-primary. */
      .btn.btn-primary,
      .btn.btn-primary:focus,
      .btn.btn-primary:active {
        background-color:#1D3557; border-color:#1D3557; color:#ffffff;
      }
      .btn.btn-primary:hover {
        background-color:#122440; border-color:#122440; color:#ffffff;
      }
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
      /* The badge sits in a narrow 2-column panel, so its text WILL wrap.
         As a plain inline span the background broke into two ragged runs with
         rounded corners only at the very start and end -- the overflow artifact.
         inline-block makes it one block box that wraps internally, so the pill
         stays a single rounded rectangle at any width. box-decoration-break is
         belt-and-braces for anywhere it still renders inline. */
      .filter-badge {
        display:inline-block; max-width:100%; box-sizing:border-box;
        background:#1D3557; color:white; border-radius:10px;
        padding:4px 10px; font-size:1.1rem; line-height:1.35;
        margin:6px 0 0 0; white-space:normal; overflow-wrap:break-word;
        -webkit-box-decoration-break:clone; box-decoration-break:clone;
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
      
      /* ---- Modal sizing ----
         Everything passed to modalDialog() before `footer` lands in .modal-body,
         and Bootstrap gives that div no height limit. The Indicator
         Distributions chart is sized to fit ~23 indicator rows (roughly 850px),
         so the body simply grew past the viewport and pushed the footer -- and
         its Close button -- off the bottom of the screen. Capping the body and
         scrolling it keeps the footer pinned and visible at any content
         height. */
      .modal-body {
        max-height: 72vh;
        overflow-y: auto;
        overflow-x: hidden;
      }
      .modal-lg { width: 92%; max-width: 1100px; }

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
      /* ---- Raw indicator value-range filter (top-right map overlay) ---- */
      .map-value-filter-overlay {
        position: absolute; top: 10px; right: 10px; z-index: 1000;
        background: white; padding: 10px 14px; border-radius: 6px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.2); width: 260px;
      }
      .map-value-filter-overlay .irs--shiny .irs-bar,
      .map-value-filter-overlay .irs--shiny .irs-single,
      .map-value-filter-overlay .irs--shiny .irs-from,
      .map-value-filter-overlay .irs--shiny .irs-to {
        background: #457B9D !important;
      }
      /* ---- Indicator description box + info button ---- */
      .indicator-desc-box { position: relative; }
      .indicator-info-btn {
        position: absolute; top: 6px; right: 6px; z-index: 5;
        background: white; border: 1.5px solid #457B9D; color: #457B9D;
        border-radius: 50%; width: 22px; height: 22px; padding: 0;
        font-size: 0.85rem; line-height: 1; cursor: pointer;
        display: flex; align-items: center; justify-content: center;
      }
      .indicator-info-btn:hover { background: #457B9D; color: white; }
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
      /* ---- Feedback button ---- */
      #feedback_btn {
        width: 100%; background-color: #8B4A2B; color: white;
        border: none; border-radius: 6px; font-weight: 600;
        font-size: 1.1rem; padding: 7px 10px; margin-top: 8px;
      }
      #feedback_btn:hover { background-color: #6d3921; color: white; }
      .navbar-feedback-link {
        color: #A8DADC !important; font-weight: 600; cursor: pointer;
      }
      /* ---- Advanced report panel ---- */
      .report-card {
        background: #f7fafc; border: 1px solid #dce8ef;
        border-left: 3px solid #2a7f7f; border-radius: 8px;
        padding: 14px 16px; height: 100%;
      }
      .report-preview-frame {
        width: 100%; height: 62vh; border: 1px solid #c1d5e0;
        border-radius: 6px; background: white;
      }
      .status-pill-row { margin: 4px 0 2px 0; }
      /* Compact source-type marker in the indicator description box. Two lines
         at sidebar width: an uppercase variable chip, then the rule beside it.
         The full explanation is in the indicator methodology modal. */
      .srcvar-box {
        background: #eef4fa; border-left: 2px solid #457B9D; border-radius: 3px;
        padding: 5px 8px; margin-top: 5px; line-height: 1.4;
      }
      .srcvar-chip {
        display: inline-block; background: #457B9D; color: white;
        padding: 1px 7px; border-radius: 9px; font-size: 0.85rem;
        font-weight: 700; letter-spacing: 0.04em; margin-right: 6px;
        vertical-align: 1px; white-space: nowrap;
      }
      .srcvar-rule { font-size: 0.98rem; color: #24425f; }
      .techdoc-pdf-btn {
        display: inline-block; background: #C05235; color: white;
        padding: 12px 28px; border-radius: 6px; font-size: 1.1rem;
        font-weight: 700; text-decoration: none; margin: 12px 0;
        border: none; cursor: not-allowed; opacity: 0.65;
      }
      /* The disabled placeholder above is the base style; this turns it into a
         live control once the PDF is actually present on the server. */
      .techdoc-pdf-btn--ready {
        cursor: pointer; opacity: 1; background: #2a7f7f;
      }
      .techdoc-pdf-btn--ready:hover,
      .techdoc-pdf-btn--ready:focus {
        background: #1D3557; color: white; text-decoration: none;
      }
      .techdoc-pdf-btn--ready:focus-visible {
        outline: 3px solid #F0C75E; outline-offset: 2px;
      }
      .techdoc-pdf-link {
        font-size: 1.05rem; color: #457B9D; text-decoration: none; font-weight: 600;
      }
      .techdoc-pdf-link:hover, .techdoc-pdf-link:focus { text-decoration: underline; }
      "))
    )
  ),
  
  footer = div(
    style = "margin-top:20px; padding:20px; background-color:#1D3557;
             color:white; text-align:center;",
    p(paste0("Application developed by the Colorado State University Geospatial Centroid",
             " | Data current as of ", DATA_LAST_UPDATED),
      style = "margin:0; opacity:0.9;"),
    # Duplicated here so the feedback form is reachable from the WBM Explorer and
    # Technical Documentation tabs, not just the map tab.
    actionLink("feedback_btn_footer",
               label = tagList(icon("bug"), " Report a bug or request an update"),
               style = "color:#A8DADC; font-size:1.05rem; margin-top:8px; display:inline-block;")
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
            indicate where a water supply falls relative to others in its <b>comparison group</b> &mdash;
            a rank of <b>90</b> means the supply scores higher than 90% of that group.
            <b>Only the Region and State filters change the comparison group.</b> Applying either one
            renormalizes and reranks every supply in the selection, so a supply flagged as High Priority
            within a state may not hold that designation nationally.")),
                                    p(style = "font-size:1.2rem; color:#555; margin-bottom:6px;",
                                      HTML("The <b>Park Unit</b>, <b>Source Type</b>, <b>Hazard Flag</b> and
            <b>Top Priority</b> filters work differently: they subset what is drawn on the map and listed
            in the table, but they do <b>not</b> trigger a recalculation. Filtering to a single park shows you
            that park&rsquo;s supplies carrying their <i>national</i> (or Region/State) ranks &mdash; not
            ranks against the other supplies in that park. The <b>Scores:</b> badge in the Current View
            panel always names the group the numbers on screen were actually calculated against.
            If you want true within-park rankings, build a <b>whole-park report</b> from the Advanced
            Reports panel below the map; it reports both, side by side and labelled.")),
                                    div(style = "background:#fff8e6; border-left:3px solid #8B6914; border-radius:4px; padding:8px 12px; margin-top:6px;",
                                        p(style = "font-size:1.4rem; color:#5a4a1a; margin:0;",
                                          HTML("<b><span aria-hidden='true'>&#x26A0;&#xFE0F;</span> Scores are relative, not absolute.</b>
                A score only has meaning within the group it is compared against.
                The same water supply may rank differently depending on whether it is evaluated
                across CONUS, a region, or a state. Set Region and State intentionally to ensure
                comparisons are meaningful, and read the <b>Scores:</b> badge before quoting a rank.")))
                                ),
                                h3(class = "info-section-title", icon("exclamation-triangle"), " Priority & Hazard Flags"),
                                p(HTML("Water supplies in the <strong>top 25% vulnerability score of the current comparison group</strong>
          are flagged as
          <span class='badge-demo' style='background:#9B2226;'>HIGH PRIORITY</span>.
          This threshold is recalculated relative to the Region/State comparison group
          (CONUS, region, or state subset), so priority designations reflect that group rather than a
          fixed national threshold. Filtering by park, source type or hazard flag does not move the threshold.
          Popups also display hazard-specific flags, which use the <strong>same top-25% rule applied to the
          individual factor score</strong> rather than to the overall vulnerability score:
          <span class='badge-demo' style='background:#C05235;'><span aria-hidden='true'>&#x1F525;</span> Fire</span>
          <span class='badge-demo' style='background:#457B9D;'><span aria-hidden='true'>&#x1F4A7;</span> Flood</span>
          <span class='badge-demo' style='background:#1D3557;'><span aria-hidden='true'>&#x1F30A;</span> Sea Level Rise</span>
          <span class='badge-demo' style='background:#8B6914;'><span aria-hidden='true'>&#x2600;&#xFE0F;</span> Drought</span>
          A supply is flagged for a hazard when its Exposure <em>or</em> Sensitivity factor for that hazard is in the
          top quartile. Factors with limited spatial coverage (flood, sea level rise) are ranked only against the
          supplies that have a value, so a flag means &ldquo;high relative to the supplies that were assessed for
          this hazard&rdquo;, not high relative to all supplies nationally."))
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
                           column(6,
                                  selectInput("score_view",
                                              label = tags$span("Score", style = "color:#386150; font-weight:600;"),
                                              choices = names(score_views),
                                              selected = "Relative Vulnerability Score")
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
                                          "(Region and State recalculate scores; Park Unit does not)")),
                            fluidRow(
                              column(4,
                                     selectizeInput("filter_region",
                                                    label = tags$span("Region",
                                                                      tags$span(style = "color:#386150; font-weight:400; font-size:1rem;", " \u21bb rescores"),
                                                                      style = "color:#1D3557; font-weight:600; font-size:1.2rem;"),
                                                    choices = c("All Regions" = "", all_regions),
                                                    selected = NULL, multiple = TRUE,
                                                    options = list(placeholder = "Search regions...",
                                                                   plugins = list("remove_button")))
                              ),
                              column(4,
                                     selectizeInput("filter_state",
                                                    label = tags$span("State",
                                                                      tags$span(style = "color:#386150; font-weight:400; font-size:1rem;", " \u21bb rescores"),
                                                                      style = "color:#1D3557; font-weight:600; font-size:1.2rem;"),
                                                    choices = c("All States" = "", all_states),
                                                    selected = NULL, multiple = TRUE,
                                                    options = list(placeholder = "Search states...",
                                                                   plugins = list("remove_button")))
                              ),
                              column(4,
                                     selectizeInput("filter_park",
                                                    label = tags$span("Park Unit",
                                                                      tags$span(style = "color:#888; font-weight:400; font-size:1rem;", " view only"),
                                                                      style = "color:#1D3557; font-weight:600; font-size:1.2rem;"),
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
                 
                 # Priority toggle + Source Type filter + Hazard flag filter
                 column(4,
                        fluidRow(
                          column(4,
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
                          column(4,
                                 div(class = "filter-section-label", HTML("<span aria-hidden='true'>&#x1F4A7;</span> Filter by Source Type")),
                                 selectizeInput("filter_source_type",
                                                label = tags$span("Source Type", style = "color:#1D3557; font-weight:600; font-size:1.2rem;"),
                                                choices = NULL,
                                                selected = NULL,
                                                multiple = TRUE,
                                                options = list(placeholder = "All source types...",
                                                               plugins = list("remove_button")))
                          ),
                          ### Hazard Flag Filter ----
                          # Filters on the hazard flags produced by
                          # calc_vulnerability_index (factor score in the top 25% of the
                          # comparison group). Like the priority toggle, flags are
                          # recalculated whenever a region or state filter is active, so
                          # this filters on flags as computed for the CURRENT comparison
                          # group, not fixed national flags.
                          column(4,
                                 div(class = "filter-section-label", HTML("<span aria-hidden='true'>&#x1F6A9;</span> Filter by Hazard Flag")),
                                 selectizeInput("filter_flags",
                                                label = tags$span("Hazard Flags", style = "color:#1D3557; font-weight:600; font-size:1.2rem;"),
                                                choices = c("Fire" = "flag_fire",
                                                            "Flood" = "flag_flood",
                                                            "Sea Level Rise" = "flag_slr",
                                                            "Drought" = "flag_drought"),
                                                selected = NULL,
                                                multiple = TRUE,
                                                options = list(placeholder = "Any hazard flag...",
                                                               plugins = list("remove_button"))),
                                 conditionalPanel(
                                   condition = "input.filter_flags && input.filter_flags.length > 1",
                                   radioButtons("filter_flags_mode", label = NULL,
                                                choices = c("Match any" = "any", "Match all" = "all"),
                                                selected = "any", inline = TRUE)
                                 )
                          )
                        )
                 ),
                 
                 # Status badge + clear
                 column(2,
                        div(class = "filter-section-label", HTML("<span aria-hidden='true'>&#x2139;&#xFE0F;</span> Current View")),
                        div(style = "margin-top:6px;",
                            uiOutput("filter_info"),
                            actionButton("clear_filters", "Clear All Filters",
                                         class = "btn-sm"),
                            actionButton("feedback_btn",
                                         label = tagList(icon("bug"), " Report an Issue"),
                                         class = "btn-sm btn-feedback")
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
                          div(class = "map-value-filter-overlay",
                              uiOutput("indicator_range_slider_ui"))
                        ),
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
           ## Advanced Reports ----
           fluidRow(
             column(12,
                    div(class = "section-panel",
                        h2(icon("file-lines"), " Advanced Reports"),
                        p(style = "font-size:1.2rem; color:#666;",
                          HTML("Build a report across more than one water supply. Every report opens in a
                          preview window first &mdash; nothing downloads until you choose to download it.
                          Reports are always scored <b>nationally</b> so that they are reproducible and
                          comparable, regardless of the filters currently applied to the map.")),
                        fluidRow(
                          column(4,
                                 div(class = "report-card",
                                     div(class = "view-card-label", "1. Choose a report type"),
                                     radioButtons(
                                       "report_mode", label = NULL,
                                       choices = c(
                                         "Whole park \u2014 compare every supply in one park" = "park",
                                         "Multi-park \u2014 compare parks against each other" = "multipark",
                                         "Batch \u2014 one full report per supply, combined" = "batch"
                                       ),
                                       selected = "park"
                                     )
                                 )
                          ),
                          column(5,
                                 div(class = "report-card",
                                     div(class = "view-card-label", "2. Choose the parks"),
                                     conditionalPanel(
                                       condition = "input.report_mode == 'park'",
                                       selectizeInput("report_park", label = "Park Unit",
                                                      choices = NULL, multiple = FALSE,
                                                      options = list(placeholder = "Search parks..."))
                                     ),
                                     conditionalPanel(
                                       condition = "input.report_mode != 'park'",
                                       selectizeInput("report_parks_multi", label = "Park Units",
                                                      choices = NULL, multiple = TRUE,
                                                      options = list(placeholder = "Search parks...",
                                                                     plugins = list("remove_button"))),
                                       actionLink("report_use_filtered",
                                                  "Use the parks currently shown on the map",
                                                  style = "font-size:1.05rem; color:#457B9D;")
                                     ),
                                     uiOutput("report_scope_note")
                                 )
                          ),
                          column(3,
                                 div(class = "report-card",
                                     div(class = "view-card-label", "3. Preview"),
                                     actionButton("build_advanced_report",
                                                  label = tagList(icon("eye"), " Build & Preview Report"),
                                                  class = "btn btn-primary",
                                                  style = "width:100%; font-weight:600;"),
                                     p(style = "font-size:1.05rem; color:#888; margin-top:8px;",
                                       "Batch reports over many supplies can take a minute or two to build,
                                       because each supply gets its own recalculated park, state and region ranks.")
                                 )
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
                  "Water Balance Model Explorer"),
               p(HTML(paste0(
                 "Historical and projected LOCA2 / Monthly Water Balance Model conditions at each NPS water ",
                 "supply. These are the underlying hydroclimate variables the Water Supply, Drought and ",
                 "Temperature indicators are built from; nothing on this tab is a vulnerability score. ",
                 "See <strong>Appendix A</strong> of the Technical Methods Report for the full description.")),
                 style = "margin:0; font-size:1.1rem; opacity:0.9;")
           ),
           
           fluidRow(
             column(3,
                    div(class = "control-panel",
                        div(class = "view-card-label",
                            HTML("<span aria-hidden='true'>&#x1F4C6;</span> Model Controls")),
                        br(),
                        selectInput("wbm_variable", "Variable",
                                    choices  = wbm_var_choices,
                                    selected = if ("runoff" %in% wbm_vars_present) "runoff" else wbm_vars_present[1]),
                        selectInput("wbm_period", "Future period",
                                    choices  = wbm_period_choices,
                                    selected = if ("mid_century" %in% wbm_periods_present) "mid_century" else wbm_periods_present[1]),
                        radioButtons("wbm_metric", "Metric",
                                     choices = c("Change (%)"            = "pct",
                                                 "Change (absolute)"     = "abs",
                                                 "Historical value"      = "hist",
                                                 "Future value"          = "future"),
                                     selected = "pct"),
                        conditionalPanel(
                          condition = "input.wbm_metric == 'pct' || input.wbm_metric == 'abs'",
                          radioButtons("wbm_stat", "Ensemble statistic",
                                       choices = c("10th percentile (dry / low end)" = "change_p10",
                                                   "Median"                          = "change_p50",
                                                   "90th percentile (wet / high end)" = "change_p90",
                                                   "Minimum"                          = "change_min",
                                                   "Maximum"                          = "change_max"),
                                       selected = "change_p50")
                        ),
                        selectizeInput("wbm_region", "NPS Region (optional)",
                                       choices = all_regions, multiple = TRUE,
                                       options = list(placeholder = "All regions")),
                        selectizeInput("wbm_park", "Park unit (optional)",
                                       choices = NULL, multiple = TRUE,
                                       options = list(placeholder = "All parks")),
                        checkboxInput("wbm_clip", "Clip colour scale to 2nd-98th percentile", value = TRUE),
                        div(style = "font-size:0.95rem; color:#666; margin-top:-6px;",
                            "Keeps a handful of extreme values from flattening the colour ramp. ",
                            "Marker colours are clipped; the values in the popups are not."),
                        tags$hr(style = "border-color:#dce8ef;"),
                        uiOutput("wbm_var_desc")
                    )
             ),
             column(9,
                    uiOutput("wbm_summary_cards"),
                    div(style = "position:relative;",
                        leafletOutput("wbm_map", height = "calc(72vh - 200px)")),
                    div(style = "margin-top:10px;",
                        plotlyOutput("wbm_hist", height = "220px")),
                    uiOutput("wbm_caption")
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
           methodology and data sources behind the vulnerability index scores displayed in this tool.
           The report follows the <a href=\'https://conbio.onlinelibrary.wiley.com/doi/10.1111/con4.70020\'
           target=\'_blank\' style=\'color:#457B9D;\'>Michalak et al. (2026)</a> hierarchical Euclidean
           distance framework."),
                 style = "font-size:1.2rem; line-height:1.7; color:#333;"),
               
               div(style = "margin:20px 0;",
                   h3(style = "color:#1D3557; font-size:1.2rem; margin-bottom:6px;",
                      icon("list-ul", style = "margin-right:6px;"), "Report Contents"),
                   tags$ul(style = "font-size:1.15rem; color:#555; line-height:1.8;",
                           tags$li("Conceptual framework and indicator hierarchy"),
                           tags$li("Raw data sources and processing steps for every implemented and provisional indicator"),
                           tags$li("Indicator development status (Implemented / Provisional / Planned) and what it means for the scores"),
                           tags$li("Euclidean distance normalization and aggregation methodology"),
                           tags$li("Source-type conditional zeroing (rainwater, ocean sources)"),
                           tags$li("Priority classification and hazard flag thresholds"),
                           tags$li("Limitations and recommended uses of the vulnerability scores")
                   )
               ),
               
               div(style = "background:#f0f5f8; border-radius:8px; padding:20px 24px; margin-top:20px;",
                   h3(style = "color:#1D3557; margin:0 0 8px 0; font-size:1.2rem;",
                      icon("download", style = "margin-right:6px; color:#2a7f7f;"),
                      "Download Technical Report"),
                   # Two routes to the same file in www/. The link opens it in the
                   # browser's own PDF viewer so a reader can check one section
                   # without saving anything; the download button forces a save
                   # with the canonical filename.
                   if (TECH_REPORT_AVAILABLE) tagList(
                     p(HTML(paste0(
                       "The full technical documentation is available below (PDF, ",
                       TECH_REPORT_SIZE, ", last updated ", TECH_REPORT_DATE, ").")),
                       style = "font-size:1.15rem; color:#555; margin:0 0 12px 0;"),
                     div(style = "display:flex; gap:12px; flex-wrap:wrap; align-items:center;",
                         downloadLink(
                           "download_tech_report",
                           class = "techdoc-pdf-btn techdoc-pdf-btn--ready",
                           HTML("<span aria-hidden='true'>&#x1F4C4;</span> Download Technical Report (PDF)")
                         ),
                         tags$a(
                           href = TECH_REPORT_URL, target = "_blank", rel = "noopener",
                           class = "techdoc-pdf-link",
                           icon("up-right-from-square"), " Open in new tab"
                         )
                     )
                   ) else tagList(
                     p(HTML("<strong>PDF report link coming soon.</strong> The full technical documentation is currently in final review.
               It will be linked here upon completion."),
                       style = "font-size:1.15rem; color:#555; margin:0 0 12px 0;"),
                     tags$button(
                       class = "techdoc-pdf-btn",
                       disabled = NA,
                       HTML("<span aria-hidden=\'true\'>&#x1F4C4;</span> Technical Report PDF (Link Pending)")
                     )
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
      class = "indicator-desc-box",
      style = paste0("background:#edf3f8; border-left:3px solid ", border_col, ";
                      border-radius:4px; padding:8px 30px 8px 10px; font-size:1.1rem;
                      color:#2c4a6e; line-height:1.5;"),
      actionButton("indicator_info_btn", label = NULL, icon = icon("info-circle"),
                   class = "btn action-button indicator-info-btn",
                   title = "View full indicator methodology"),
      tags$strong(
        style = paste0("color:", border_col, "; font-size:1.05rem;"),
        paste0(input$component, " Indicator Description:")
      ),
      # Development status straight from Table 1 of the Technical Methods Report.
      # Provisional indicators ARE included in every score the app shows; the
      # badge exists so reviewers know which numbers are still expected to move.
      div(class = "status-pill-row",
          HTML(status_badge(if (is.null(cfg$status)) "Implemented" else cfg$status)),
          tags$span(style = "font-size:0.95rem; color:#666; margin-left:6px;",
                    paste0("Factor: ", factor_status(input$component, input$factor)))),
      tags$span(cfg$description, style = "color:#333; font-size:1.1rem;"),
      # Source-type marker, deliberately two lines. The full rule lives in the
      # methodology modal behind the (i) button at the top of this box -- see
      # source_var_detail(). Keeping the sidebar short matters because this sits
      # directly above the map controls the user is actually here to change.
      if (nzchar(source_var_chip(cfg)))
        div(class = "srcvar-box", HTML(source_var_chip(cfg))),
      if (identical(cfg$status, "Provisional"))
        div(style = "background:#fff5e6; border-left:2px solid #B26B00; border-radius:3px;
                     padding:3px 8px; margin-top:5px; font-size:1rem; color:#7a4a00;",
            "Provisional: this indicator is calculated and included in the scores,
             but its methodology is still being revised and its values are expected to change."),
      tags$hr(style = "margin:6px 0; border-color:#c1d5e0;"),
      div(
        style = "background:#e8f4f0; border-left:2px solid #2a7f7f; border-radius:3px;
                 padding:3px 8px; font-size:1rem; color:#1a5c4a;",
        icon("calendar-alt", style = "margin-right:4px; font-size:0.95rem;"),
        paste0("Data Last Updated: ", DATA_LAST_UPDATED)
      )
    )
  })
  
  ## Indicator Methodology Modal (info tooltip) ----
  observeEvent(input$indicator_info_btn, {
    req(input$component, input$factor, input$indicator)
    cfg <- indicator_config[[input$component]][[input$factor]][[input$indicator]]
    req(!is.null(cfg))
    
    border_col <- if (input$component == "Exposure") "#5B8C6E" else "#7C6FAD"
    details     <- indicator_details[[cfg$raw_col]]
    
    ind_status <- if (!is.null(details) && !is.null(details$status) && !is.na(details$status)) {
      details$status
    } else if (!is.null(cfg$status)) cfg$status else "Implemented"
    
    # The Applies To row is where the full source-type rule lives. The sidebar
    # only carries a two-line chip, so this row has to stand on its own for a
    # reader who clicked (i) precisely because the chip was not enough. Prefer
    # the source_note column of indicator_details.csv (the methods report's own
    # wording) and fall back to the rule encoded in the app config, so the two
    # cannot silently disagree.
    applies_txt <- {
      from_csv <- if (!is.null(details) && !is.null(details$source_note) &&
                      !is.na(details$source_note) && nzchar(trimws(details$source_note))) {
        trimws(details$source_note)
      } else ""
      # The CSV note carries the methods report's own wording of the rule; the
      # supplement adds only what the report leaves out -- the live system count
      # and the point that each system contributes one Water Supply value rather
      # than both. Falling back to the full app-side rule when the CSV has no
      # note keeps the row informative for an indicator added ahead of its
      # documentation.
      if (nzchar(from_csv)) paste0(from_csv, " ", source_var_supplement(cfg))
      else source_var_detail(cfg)
    }
    
    body_html <- if (!is.null(details)) {
      paste0(
        "<table style='width:100%; border-collapse:collapse; font-size:13px;'>",
        "<tr><td style='padding:6px 8px; font-weight:600; color:#1D3557; width:150px; vertical-align:top;'>Development Status</td>",
        "<td style='padding:6px 8px;'>", status_badge(ind_status, "11px"),
        if (identical(ind_status, "Provisional"))
          "<div style='margin-top:5px;color:#7a4a00;'>Calculated and included in all scores shown in the tool, but the methodology is still under revision and values are expected to change.</div>"
        else if (identical(ind_status, "Implemented"))
          "<div style='margin-top:5px;color:#555;'>Methodology is stable; results can be interpreted as described below.</div>"
        else "",
        "</td></tr>",
        # Applies-to row, shown only for the source-type conditional indicators.
        # Drawn from the source_note column of indicator_details.csv when the
        # methods report supplies one, and falling back to the rule encoded in
        # the app config otherwise, so the two cannot silently disagree.
        if (nzchar(applies_txt))
          paste0("<tr><td style='padding:6px 8px; font-weight:600; color:#1D3557; vertical-align:top;'>Applies To</td>",
                 "<td style='padding:6px 8px; background:#eef4fa;'>",
                 htmltools::htmlEscape(applies_txt), "</td></tr>")
        else "",
        "<tr><td style='padding:6px 8px; font-weight:600; color:#1D3557; width:150px; vertical-align:top;'>Data Source</td>",
        "<td style='padding:6px 8px;'>", htmltools::htmlEscape(details$data_source), "</td></tr>",
        "<tr style='background:#f7fafc;'><td style='padding:6px 8px; font-weight:600; color:#1D3557; vertical-align:top;'>Methodology</td>",
        "<td style='padding:6px 8px;'>", htmltools::htmlEscape(details$methodology), "</td></tr>",
        "<tr><td style='padding:6px 8px; font-weight:600; color:#1D3557; vertical-align:top;'>Vulnerability Direction</td>",
        "<td style='padding:6px 8px;'>", htmltools::htmlEscape(details$vulnerability_direction), "</td></tr>",
        if (!is.null(details$report_section) && !is.na(details$report_section))
          paste0("<tr style='background:#f7fafc;'><td style='padding:6px 8px; font-weight:600; color:#1D3557; vertical-align:top;'>Reference</td>",
                 "<td style='padding:6px 8px;'>Technical Methods Report, Section ",
                 htmltools::htmlEscape(as.character(details$report_section)),
                 ". See the <b>Technical Documentation</b> tab for the full report.</td></tr>")
        else "",
        "</table>"
      )
    } else {
      paste0(
        "<p style='color:#999; font-style:italic; font-size:13px;'>",
        "Detailed data source and methodology information for this indicator is not yet ",
        "available in the Technical Methods Report reference file.</p>"
      )
    }
    
    showModal(modalDialog(
      title = tagList(
        icon("info-circle", style = paste0("color:", border_col, "; margin-right:6px;")),
        input$indicator
      ),
      div(
        style = paste0("border-left:4px solid ", border_col, "; padding-left:10px; margin-bottom:12px;"),
        tags$span(style = paste0("color:", border_col, "; font-weight:600; font-size:11px; text-transform:uppercase; letter-spacing:0.05em;"),
                  paste0(input$component, " \u203a ", input$factor)),
        tags$p(style = "color:#333; margin:4px 0 0 0;", cfg$description)
      ),
      HTML(body_html),
      size      = "m",
      easyClose = TRUE,
      footer    = modalButton("Close")
    ))
  })
  
  
  ## Clear Filters ----
  observeEvent(input$clear_filters, {
    updateSelectizeInput(session, "filter_region", selected = character(0))
    updateSelectizeInput(session, "filter_state",  selected = character(0))
    updateSelectizeInput(session, "filter_park", selected = character(0))
    updateSelectizeInput(session, "filter_source_type", selected = character(0))
    updateSelectizeInput(session, "filter_flags", selected = character(0))
    updateRadioButtons(session, "filter_flags_mode", selected = "any")
    # The priority switch is a filter like any other and has to reset too --
    # leaving it on made "Clear All Filters" silently keep the view filtered.
    updateMaterialSwitch(session, "filter_priority", value = FALSE)
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
    
    ## Hazard flag filter ----
    # Flag columns are logical with possible NAs; NA is treated as "not flagged"
    # so a site with a missing factor score is never silently promoted into a
    # hazard-filtered view.
    sel_flags <- input$filter_flags
    if (length(sel_flags) > 0) {
      sel_flags <- sel_flags[sel_flags %in% names(df)]
      if (length(sel_flags) > 0) {
        flag_mat <- as.data.frame(df)[, sel_flags, drop = FALSE]
        flag_mat[] <- lapply(flag_mat, function(x) !is.na(x) & x)
        mode_sel <- if (is.null(input$filter_flags_mode)) "any" else input$filter_flags_mode
        keep <- if (identical(mode_sel, "all")) {
          rowSums(as.matrix(flag_mat)) == length(sel_flags)
        } else {
          rowSums(as.matrix(flag_mat)) > 0
        }
        df <- df[keep, ]
      }
    }
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
  
  ## Raw Indicator Value-Range Slider ----
  # Bounds are rebuilt from the current filtered_data_display() distribution every
  # time the active indicator or an upstream filter changes, so the slider always
  # starts covering the full range of what's "currently shown on the map."
  output$indicator_range_slider_ui <- renderUI({
    req(input$view_mode == "indicator", input$component, input$factor, input$indicator)
    cfg <- indicator_config[[input$component]][[input$factor]][[input$indicator]]
    req(!is.null(cfg))
    
    vals <- suppressWarnings(as.numeric(as.data.frame(filtered_data_display())[[cfg$raw_col]]))
    vals <- vals[is.finite(vals)]
    
    if (length(vals) == 0) {
      return(div(style = "font-size:1rem; color:#999; font-style:italic;",
                 "No numeric values available to filter for this indicator."))
    }
    
    rng <- range(vals)
    if (rng[1] == rng[2]) rng <- rng + c(-0.5, 0.5)
    # Round OUTWARD (min down, max up), not to-nearest. signif() rounds to
    # nearest and can clip the true min/max inward by a hair, which silently
    # excludes the extreme site(s) from the default full-range slider and
    # makes the chart falsely look "filtered" before the user touches anything.
    round_out <- function(x, direction) {
      if (x == 0) return(0)
      scale <- 10 ^ (floor(log10(abs(x))) - 2)
      if (direction == "down") floor(x / scale) * scale else ceiling(x / scale) * scale
    }
    rng  <- c(round_out(rng[1], "down"), round_out(rng[2], "up"))
    step <- signif(diff(rng) / 100, 2)
    if (!is.finite(step) || step <= 0) step <- 0.01
    
    tagList(
      div(style = "font-size:1.05rem; font-weight:600; color:#1D3557; margin-bottom:4px;",
          "Filter by Value Range"),
      sliderInput("indicator_value_range", label = NULL,
                  min = rng[1], max = rng[2], value = c(rng[1], rng[2]),
                  step = step, width = "100%"),
      div(style = "font-size:0.95rem; color:#666;",
          paste0(length(vals), " sites with data \u2022 no-data sites always shown as grey"))
    )
  })
  
  ## Map/Table Data After Value-Range Filter ----
  # In indicator view, narrows filtered_data_display() to the slider's selected
  # range. Sites with NA for the active indicator are always kept (shown grey) --
  # the slider filters on value, not on data availability.
  map_indicator_filtered_data <- reactive({
    df <- filtered_data_display()
    if (isTRUE(input$view_mode == "indicator") && !is.null(input$indicator_value_range)) {
      req(input$component, input$factor, input$indicator)
      cfg <- indicator_config[[input$component]][[input$factor]][[input$indicator]]
      if (!is.null(cfg) && cfg$raw_col %in% names(df)) {
        vals  <- suppressWarnings(as.numeric(as.data.frame(df)[[cfg$raw_col]]))
        rng   <- input$indicator_value_range
        keep  <- is.na(vals) | (vals >= rng[1] & vals <= rng[2])
        df    <- df[keep, ]
      }
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
  # Two independent facts, previously conflated into one branch:
  #   (a) HOW MANY supplies are on the map right now, and
  #   (b) WHICH group the scores on those supplies were calculated against.
  # The old version keyed the count off filtered_data() and only showed a count
  # at all when a Region/State/Park filter was set. That meant the Source Type,
  # Hazard Flag, Top Priority and indicator value-range filters could all cut the
  # map down to a handful of points while this panel still read "Showing all
  # 1409 water supplies". The count now comes from map_indicator_filtered_data(),
  # which is exactly what addCircleMarkers() is handed, so it cannot drift out of
  # step with the map again no matter which filter is added later.
  output$filter_info <- renderUI({
    shown <- nrow(map_indicator_filtered_data())
    tot   <- nrow(combined_data)
    
    has_geo_filter <- length(input$filter_region) > 0 || length(input$filter_state) > 0
    
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
    
    # Named so the tooltip can tell the user WHY the count dropped, which the
    # bare number never did.
    active_filters <- c(
      if (length(input$filter_region) > 0)      "Region"      else NULL,
      if (length(input$filter_state) > 0)       "State"       else NULL,
      if (length(input$filter_park) > 0)        "Park Unit"   else NULL,
      if (length(input$filter_source_type) > 0) "Source Type" else NULL,
      if (length(input$filter_flags) > 0)       "Hazard Flag" else NULL,
      if (isTRUE(input$filter_priority))        "Top Priority" else NULL
    )
    # The value-range slider lives on the map rather than the filter panel, so it
    # is detected by its effect rather than by reading an input.
    range_active <- isTRUE(input$view_mode == "indicator") &&
      shown < nrow(as.data.frame(filtered_data_display()))
    if (range_active) active_filters <- c(active_filters, "Indicator Value Range")
    
    count_line <- if (shown == tot) {
      p(style = "color:#666; font-size:1.25rem; margin-top:8px; margin-bottom:2px;",
        paste0("Showing all ", tot, " water supplies"))
    } else {
      p(style = "color:#1D3557; font-weight:600; font-size:1.25rem; margin-top:8px; margin-bottom:2px;",
        title = if (length(active_filters) > 0)
          paste0("Active filters: ", paste(active_filters, collapse = ", ")) else NULL,
        paste0("Showing ", shown, " of ", tot, " water supplies"))
    }
    
    filter_line <- if (length(active_filters) > 0) {
      p(style = "color:#888; font-size:1rem; line-height:1.3; margin:0 0 2px 0;",
        paste0("Filtered by: ", paste(active_filters, collapse = ", ")))
    } else NULL
    
    # Scores badge tracks the NORMALIZATION group, which only Region and State
    # change -- deliberately independent of the count above.
    score_badge <- if (has_geo_filter) {
      tags$span(class = "filter-badge", style = "background:#386150;",
                paste0("\U0001f4ca Scores: ", context_label))
    } else if (length(active_filters) > 0) {
      tags$span(class = "filter-badge", style = "background:#457B9D;",
                title = paste0("The filters applied (", paste(active_filters, collapse = ", "),
                               ") narrow the view but do not rescore. ",
                               "Only Region and State change the comparison group."),
                "\U0001f4ca Scores: National \u00b7 view filtered")
    } else {
      tags$span(class = "filter-badge", style = "background:#386150;",
                "\U0001f30e Scores: National")
    }
    
    tagList(count_line, filter_line, score_badge)
  })
  
  ## Active Column ----
  active_column <- reactive({
    req(!is.null(input$view_mode) && nchar(input$view_mode) > 0)
    if (input$view_mode == "score") {
      base_col <- score_views[[input$score_view]]
      score_rank_cols[[base_col]]
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
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Terrain") %>% 
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
          "<b>Region:</b> ", region, "<br/>",
          "<b>State:</b> ", state, "<br/>",
          "<b>Source Type:</b> ", source_type
        ),
        options     = pathOptions(pane = "m_h")
      ) %>% 
      addLayersControl(baseGroups = c("OpenStreetMap", "Satellite", "Terrain"),
                       overlayGroups = "Municipal/Hauled Supplies",
                       options = layersControlOptions(position = "topleft")) %>% 
      hideGroup("Municipal/Hauled Supplies")
  })
  
  ## Map Markers ----
  observe({
    req(active_column())
    col       <- active_column()
    plot_data <- map_indicator_filtered_data()
    req(col %in% names(plot_data))
    
    vals    <- as.numeric(as.data.frame(plot_data)[[col]])
    vals    <- vals[is.finite(vals)]
    
    # Empty result: clear the map rather than stopping the observer.
    #
    # This used to be req(length(vals) > 0), which halts the observer silently.
    # Because clearGroup() and the marker redraw both live at the BOTTOM of this
    # block, halting here meant the previous selection's markers were never
    # removed -- so a filter combination matching nothing (Match all on several
    # hazard flags, most often) reported "0 of N supplies" in the summary while
    # the map still showed the points from the last selection that did match.
    # Stale points that contradict the count are worse than an empty map, so the
    # empty case is now handled explicitly: wipe the markers, drop the legend,
    # and say what happened.
    if (nrow(plot_data) == 0 || length(vals) == 0) {
      empty_msg <- paste0(
        "<div style='background:rgba(255,255,255,0.95); border-left:3px solid #C05235; ",
        "border-radius:4px; padding:10px 14px; font-family:Arial; font-size:12px; ",
        "color:#1D3557; max-width:260px; box-shadow:0 2px 8px rgba(0,0,0,0.15);'>",
        "<b>No water supplies match the current filters.</b><br>",
        "<span style='color:#555;'>",
        if (length(input$filter_flags) > 1 && identical(input$filter_flags_mode, "all"))
          paste0("Matching <b>all</b> of ", length(input$filter_flags),
                 " hazard flags is restrictive; few supplies are flagged on more than one. ",
                 "Try &ldquo;Match any&rdquo;, or remove a flag.")
        else
          "Try removing a filter or widening the value range.",
        "</span></div>")
      leafletProxy("map") %>%
        removeControl("legend") %>%
        removeControl("empty_state") %>%
        clearGroup(c("data_points", "highlight")) %>%
        addControl(html = empty_msg, position = "bottomright", layerId = "empty_state")
      return(invisible(NULL))
    }
    
    val_rng <- range(vals, na.rm = TRUE)
    # Prevent zero-width domain which breaks colorNumeric
    if (val_rng[1] == val_rng[2]) val_rng <- c(val_rng[1] - 0.001, val_rng[2] + 0.001)
    
    # Determine palette domain
    is_rank <- (input$view_mode == "score")
    pal_dom <- if (is_rank) c(0, 100) else val_rng
    
    # Columns where lower raw values = higher vulnerability (invert color scale)
    invert_raw_cols <- c("exp_runoff_change", "exp_precip_change", "exp_drought_change",
                         "sen_runoff_trend", "sen_precip_trend", "sen_drought_trend", "sen_source_type")
    is_inverted <- (input$view_mode == "indicator") && col %in% invert_raw_cols
    
    pal_colors <- if (is_inverted) {
      rev(c("#FFF3D6","#F0C75E","#DD8844","#C05235","#9B2226"))
    } else {
      c("#FFF3D6","#F0C75E","#DD8844","#C05235","#9B2226")
    }
    
    
    pal <- colorNumeric(pal_colors, domain = pal_dom, na.color = "lightgrey")
    
    legend_title <- if (input$view_mode == "score") {
      paste0(str_wrap(input$score_view, 20), "\n(Percentile Rank)")
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
             pmax(3, pmin(12, scales::rescale(plot_vals, to=c(12,3), from=pal_dom))))
    } else {
      ifelse(is.na(plot_vals), 3,
             pmax(3, pmin(12, scales::rescale(plot_vals, to=c(3,12), from=pal_dom))))
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
      # These four are metadata carried through from water_supplies.csv and
      # never scored. col_missing() keeps the Map() arity intact if an older
      # water_supplies.csv without the water-rights columns is deployed --
      # a NULL here would silently collapse the whole popup vector to length 0
      # and leave the map with no popups at all.
      fmss_number           = col_missing(df, "fmss_number"),
      state_water_rights_id = col_missing(df, "state_water_rights_id"),
      surface_water_law     = col_missing(df, "SurfaceWaterLaw"),
      ground_water_law      = col_missing(df, "GroundWaterLaw"),
      MoreArgs = list(view_mode = input$view_mode,
                      score_label = popup_score, component = popup_comp,
                      factor_name = popup_fac, indicator_name = popup_ind,
                      raw_label = popup_rawlbl)
    ))
    
    # force legend to show legend points sized and colored by var
    breaks <- pretty(pal_dom, n = 5)
    breaks <- breaks[breaks >= pal_dom[1] & breaks <= pal_dom[2]]
    
    legend_radius <- if (is_inverted) {
      pmax(4, pmin(16, scales::rescale(breaks, to = c(16, 4), from = pal_dom)))
    } else {
      pmax(4, pmin(16, scales::rescale(breaks, to = c(4, 16), from = pal_dom)))
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
    
    legend_component_label <- if (is_ind) {
      comp_color <- if (input$component == "Exposure") "#5B8C6E" else "#7C6FAD"
      paste0(
        "<div style='font-weight:700;font-size:11px;text-transform:uppercase;",
        "letter-spacing:0.05em;color:", comp_color, ";margin-bottom:3px;'>",
        input$component, "</div>"
      )
    } else ""
    
    legend_html <- paste0(
      "<div style='background:white;padding:8px 10px;border-radius:6px;",
      "box-shadow:0 2px 8px rgba(0,0,0,0.2);font-size:12px;max-width:180px;'>",
      legend_component_label,
      "<div style='font-weight:600;margin-bottom:6px;'>", gsub("\n", "<br/>", legend_title), "</div>",
      legend_rows,
      "</div>"
    )
    
    # ADD DATA POINTS ------------
    leafletProxy("map") %>%
      removeControl("legend") %>% 
      # Clear the empty-state notice too, otherwise it stays pinned to the map
      # after the user relaxes the filter and points come back.
      removeControl("empty_state") %>%
      clearGroup(c("data_points", "highlight")) %>%
      addCircleMarkers(
        data = plot_data,
        group = "data_points",
        radius = radius_vec,
        color = "#000000",
        fillColor = fill_vec,
        fillOpacity = 0.8,
        stroke = TRUE,
        weight = 1.5,
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
    
    # Bins are computed over the TRUE data range; the 4% padding is applied only
    # to the axis view. These used to be the same vector, which meant the cosmetic
    # padding became real bin boundaries: wildfire hazard runs 0 to 99.56, so the
    # first edge landed at -3.98 and the hover read "-3.98 - 1.39" on an indicator
    # that is a percentage of buffer area and cannot be negative. Keeping them
    # separate leaves the bars their breathing room at the plot edges while every
    # bin label stays inside the range the data actually occupies. Counts are
    # unchanged; only the edge positions and hover text move.
    data_range <- range(nat_vals)
    pad <- diff(data_range) * 0.04
    if (pad == 0) pad <- 0.5
    x_range <- c(data_range[1] - pad, data_range[2] + pad)
    
    n_bins    <- 20
    # A zero-width data range (every supply identical) would collapse seq() to a
    # single repeated value and break cut(), so fall back to the padded view.
    bin_edges <- if (diff(data_range) > 0) {
      seq(data_range[1], data_range[2], length.out = n_bins + 1)
    } else {
      seq(x_range[1], x_range[2], length.out = n_bins + 1)
    }
    bin_size  <- diff(bin_edges)[1]
    
    filt_df     <- as.data.frame(map_indicator_filtered_data())
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
    
    filt_df   <- as.data.frame(map_indicator_filtered_data())
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
    
    # Chart headers name the water system, not the park unit: a park can hold
    # dozens of supplies, so "MORA - 12345" told the reader nothing they could
    # not already see, whereas the system name is what appears on the facility
    # paperwork. Falls back to the park unit when the WSD has no system name on
    # record, so the header never degrades to a bare "NA - 12345".
    sys_name <- site_row$water_system_name[1]
    site_title <- paste0(
      if (is.na(sys_name) || !nzchar(trimws(sys_name))) site_row$park_unit[1] else trimws(sys_name),
      " \u2013 ", site_row$wsd_source_id[1]
    )
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
    # Factor definitions, the source-type N/A test and the zero-safe division
    # all come from the shared report helpers so the interactive breakdown and
    # the generated reports can no longer drift apart.
    factor_defs <- report_factor_defs
    na_flag_for <- function(id) na_flag_for_source(id, src_type)
    safe_div    <- rpt_safe_div
    
    # Site's normalized value for an indicator, NA-safe.
    site_norm <- function(norm_col) {
      v <- site_row[[norm_col]]
      if (is.null(v)) NA_real_ else as.numeric(v[1])
    }
    
    
    # ===================================================================
    # TAB 1: "Component Contribution" -- factor-level bar chart
    # ===================================================================
    build_hover <- function(d, header) {
      # Source-conditional factors get a one-line reminder of which variable is
      # in play, so the hover explains why a rainwater system shows "Amount
      # (precip)" where every neighbouring system shows "Amount (runoff)".
      cond_line <- if (isTRUE(d$source_conditional)) {
        v <- water_supply_var_for(src_type)
        if (v == "none") "Not applicable to ocean sources"
        else sprintf("<i>Scored on %s for this source type</i>", v)
      } else NULL
      if (d$type == "euclidean") {
        inds      <- resolve_factor_indicators(d, src_type)
        norm_vals <- vapply(inds, function(ind) site_norm(ind$norm_col), numeric(1))
        denom     <- sum(norm_vals^2, na.rm = TRUE)
        shares    <- 100 * safe_div(norm_vals^2, denom)
        lines <- vapply(seq_along(inds), function(j) {
          if (is.na(norm_vals[j])) {
            sprintf("%s: no data", inds[[j]]$label)
          } else {
            sprintf("%s: %.0f%%", inds[[j]]$label, shares[j])
          }
        }, character(1))
        paste(c(header, cond_line, lines), collapse = "<br>")
      } else {
        paste(c(header, cond_line, "(Single Indicator)"), collapse = "<br>")
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
        lbl     <- paste0(factor_display_label(d, src_type), suffix)
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
        lbl <- paste0(factor_display_label(d, src_type), suffix)
        fid <- paste0(comp_id, "/", lbl)
        factor_share <- safe_div(vals[i]^2, denom)
        factor_vuln  <- factor_share * comp_share_vuln
        ic_add(fid, lbl, comp_id, factor_vuln, color)
        if (d$type == "euclidean") {
          inds      <- resolve_factor_indicators(d, src_type)
          norm_vals <- vapply(inds, function(ind) site_norm(ind$norm_col), numeric(1))
          ind_share <- safe_div(norm_vals^2, sum(norm_vals^2, na.rm = TRUE))
          for (j in seq_along(inds)) {
            # Skip indicators with no data: an NA value would propagate an NA
            # box size into the icicle and blank out its whole parent branch.
            if (is.na(norm_vals[j])) next
            ind <- inds[[j]]
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
    # TAB 0: "Indicator Distributions" -- where this site sits within the
    # comparison group on EVERY normalized indicator at once. The contribution
    # tabs answer "what makes up this site's score"; this one answers the
    # different question "is this site unusual, and on what". Normalized (0-1)
    # values are plotted rather than raw values so that indicators measured in
    # days, metres, percent and degrees C share one axis.
    # ===================================================================
    # The comparison group here is geo_recalculated_data(), NOT filtered_data().
    # That is the set min-max normalization was actually run over: scores are
    # only recalculated on Region/State changes, so the Park, Priority, Source
    # Type and Hazard Flag filters narrow what is DISPLAYED without changing a
    # single normalized value. Drawing the boxes from filtered_data() therefore
    # showed a distribution the numbers were never normalized against, and
    # reported an "n =" that shrank with those filters while still labelling the
    # scope "National" -- e.g. n = 100 under a priority filter, when all 1,410
    # supplies defined the 0-1 range every diamond is plotted on.
    norm_group_df  <- as.data.frame(geo_recalculated_data())
    n_norm_group   <- nrow(norm_group_df)
    n_display_view <- nrow(as.data.frame(filtered_data()))
    
    dist_parts <- list(); site_parts <- list(); missing_labels <- character(0)
    has_provisional <- FALSE
    # Which Water Supply branch this system is actually scored on. The other
    # branch has a normalized value in the data -- every site gets both runoff
    # and precipitation columns -- but it contributes nothing to this site's
    # score, so plotting it invites the reader to interpret a number that is not
    # part of the answer. Dropped here and named in the caption instead.
    site_supply_var <- water_supply_var_for(src_type)
    skipped_branch  <- character(0)
    for (e in scored_indicator_index) {
      if (!e$col %in% names(norm_group_df)) next
      if (!is.na(e$source_var) && !identical(e$source_var, site_supply_var)) {
        skipped_branch <- c(skipped_branch, e$indicator)
        next
      }
      v <- suppressWarnings(as.numeric(norm_group_df[[e$col]]))
      v <- v[is.finite(v)]
      if (length(v) == 0) next
      is_prov <- identical(e$status, "Provisional")
      if (is_prov) has_provisional <- TRUE
      lbl <- paste0(e$indicator, if (is_prov) " *" else "")
      dist_parts[[length(dist_parts) + 1]] <- data.frame(
        label = lbl, component = e$component, value = v, stringsAsFactors = FALSE)
      sv <- site_norm(e$col)
      if (is.finite(sv)) {
        site_parts[[length(site_parts) + 1]] <- data.frame(
          label = lbl, component = e$component, value = sv,
          pctl = round(100 * mean(v <= sv), 0), stringsAsFactors = FALSE)
      } else {
        missing_labels <- c(missing_labels, e$indicator)
      }
    }
    
    p_dist <- NULL
    dist_height <- 520
    if (length(dist_parts) > 0) {
      dist_df <- do.call(rbind, dist_parts)
      site_df <- if (length(site_parts) > 0) do.call(rbind, site_parts) else NULL
      
      # Exposure block on top, Sensitivity below, config order within each.
      # `lvl` is built in top-to-bottom reading order and then reversed, because
      # a plotly categorical y-axis fills from the bottom up.
      lvl <- unique(dist_df$label[order(match(dist_df$component, c("Exposure", "Sensitivity")))])
      lvl <- rev(lvl)
      # Tall enough that every indicator row stays legible, capped so the chart
      # cannot outgrow the scrollable modal body by an absurd margin.
      dist_height <- min(900, max(420, 30 * length(lvl) + 150))
      
      # One box trace per component rather than one for everything, so the boxes
      # carry the same Exposure green / Sensitivity purple used by the
      # Component Contribution bars, the All Levels icicle and every generated
      # report. Previously every box was the same blue-grey and the only cue
      # that the axis was split into two blocks was the ordering, which is
      # invisible once the reader scrolls into the middle of a 24-row chart.
      # The legend gains a real function too: it now names the components
      # instead of restating "Comparison group".
      dist_df$component <- factor(dist_df$component, levels = c("Exposure", "Sensitivity"))
      comp_fill <- c(Exposure = "rgba(91,140,110,0.45)", Sensitivity = "rgba(124,111,173,0.40)")
      comp_line <- c(Exposure = EXP_COLOR,               Sensitivity = SEN_COLOR)
      
      p_dist <- plot_ly()
      for (cmp_name in levels(dist_df$component)) {
        cmp_rows <- dist_df[dist_df$component == cmp_name, ]
        if (nrow(cmp_rows) == 0) next
        p_dist <- p_dist %>%
          add_trace(
            data = cmp_rows, x = ~value, y = ~label, type = "box", orientation = "h",
            boxpoints = FALSE, name = cmp_name, legendgroup = cmp_name,
            line = list(color = comp_line[[cmp_name]], width = 1.2),
            fillcolor = comp_fill[[cmp_name]],
            hoverinfo = "x"
          )
      }
      if (!is.null(site_df)) {
        p_dist <- p_dist %>%
          add_trace(
            data = site_df, x = ~value, y = ~label, type = "scatter", mode = "markers",
            name = "This water supply",
            marker = list(color = "#C05235", size = 11, symbol = "diamond",
                          line = list(color = "white", width = 1.2)),
            hovertemplate = ~paste0("<b>", label, "</b><br>Normalized value: ",
                                    sprintf("%.3f", value),
                                    "<br>Higher than ", pctl, "% of the comparison group",
                                    "<extra></extra>")
          )
      }
      p_dist <- p_dist %>% layout(
        title = list(
          text = paste0(site_title,
                        "<br><span style='font-size:11px;color:#696969;font-weight:400;'>",
                        "Normalized indicator values (0 = least vulnerable, 1 = most) \u00b7 ",
                        scope_label, " \u00b7 normalized over n = ", n_norm_group,
                        " supplies</span>"),
          font = list(size = 14, color = "#1D3557"), x = 0, xanchor = "left"),
        xaxis = list(title = "Normalized value (0-1)", range = c(-0.03, 1.03),
                     tickfont = list(size = 10), gridcolor = "#eee", zeroline = FALSE),
        yaxis = list(title = "", tickfont = list(size = 10),
                     categoryorder = "array", categoryarray = lvl),
        margin = list(t = 62, l = 10, r = 20, b = 50),
        legend = list(orientation = "h", y = -0.12, font = list(size = 11)),
        plot_bgcolor = "white", paper_bgcolor = "white", showlegend = TRUE
      )
    }
    
    # Named the same way in the caption as in the subtitle, and explicit about
    # the fact that a narrowed map view does not narrow this chart.
    narrowing_filters <- c(
      if (length(input$filter_park) > 0) "park" else NULL,
      if (isTRUE(input$filter_priority)) "priority" else NULL,
      if (length(input$filter_source_type) > 0) "source type" else NULL,
      if (length(input$filter_flags) > 0) "hazard flag" else NULL
    )
    # Site-specific and short. The general rule is one click away in the
    # indicator methodology modal; repeating it under every chart is noise.
    supply_var_caption <- if (length(skipped_branch) > 0) {
      paste0(
        "<br><b>Water Supply:</b> this system is scored on <b>",
        if (site_supply_var == "none") "neither runoff nor precipitation" else site_supply_var,
        "</b>, so the ",
        htmltools::htmlEscape(paste(unique(skipped_branch), collapse = ", ")),
        if (length(unique(skipped_branch)) == 1) " indicator is" else " indicators are",
        " not shown.")
    } else ""
    
    dist_caption <- paste0(
      "Each box shows the spread of a normalized indicator across the <b>normalization group</b> ",
      "(<b>", scope_label, "</b>, n = ", n_norm_group, " supplies), coloured ",
      "<span style='color:", EXP_COLOR, ";font-weight:600;'>green for Exposure</span> and ",
      "<span style='color:", SEN_COLOR, ";font-weight:600;'>purple for Sensitivity</span> to match the ",
      "other two tabs and the generated reports; Exposure indicators are grouped at the top of the ",
      "axis and Sensitivity below. The red diamond is this water ",
      "supply, and the percentile in each tooltip is its position within that same group. Values are ",
      "min-max normalized <i>within the normalization group</i> and direction-corrected, so 1 is ",
      "always the most vulnerable end regardless of whether the raw indicator increases or decreases ",
      "with risk. Model-agreement columns are not shown: ",
      "they are confidence weights applied to their parent indicator, not independent axes of the score.",
      if (length(narrowing_filters) > 0) paste0(
        "<br><b>Note:</b> your ", paste(narrowing_filters, collapse = " and "),
        " filter narrows the map to ", n_display_view, " of these ", n_norm_group,
        " supplies, but does not trigger renormalization. The boxes and percentiles above are ",
        "therefore still drawn over all ", n_norm_group, " supplies, which is the set these ",
        "normalized values were actually calculated from. Only a Region or State filter changes ",
        "the normalization group.") else "",
      supply_var_caption,
      if (length(missing_labels) > 0)
        paste0("<br><b>No data for this supply on:</b> ",
               htmltools::htmlEscape(paste(missing_labels, collapse = ", ")), ".")
      else "")
    
    # An inline renderPlotly() in modal UI gets an implicit plotlyOutput whose
    # container is 400px tall, regardless of what layout(height=) tells the SVG.
    # At ~900px the plot therefore overflowed its own container and painted over
    # whatever followed it in the DOM -- which is why the Provisional footnote
    # flashed on first paint and was then covered. Binding the plot to a named
    # output lets the CONTAINER carry the height (below), so the div actually
    # reserves the space and the footnote keeps its place in the flow.
    output$site_dist_chart <- renderPlotly({
      req(!is.null(p_dist))
      p_dist
    })
    
    # ===================================================================
    # Modal with all tabs
    # ===================================================================
    showModal(modalDialog(
      title = paste0("Score Contribution Breakdown \u2013 ", site_title),
      tabsetPanel(
        tabPanel("Indicator Distributions",
                 if (is.null(p_dist)) {
                   tags$p("No normalized indicator values are available for this comparison group.",
                          style = "font-size:13px; color:#888; padding:20px;")
                 } else {
                   div(style = "padding-top:6px;",
                       plotlyOutput("site_dist_chart",
                                    height = paste0(dist_height, "px"),
                                    width  = "100%"))
                 },
                 # The star on the axis labels needs its key adjacent to the
                 # chart, not buried in the paragraph below it: the chart is
                 # tall enough that the reader has to scroll past all of it
                 # before reaching any prose, so a mid-paragraph mention was a
                 # footnote marker with no reachable footnote. Rendered only
                 # when a starred indicator is actually on the axis.
                 if (isTRUE(has_provisional))
                   div(style = "background:#fff5e6; border-left:3px solid #B26B00;
                                border-radius:3px; padding:6px 10px; margin-top:8px;
                                font-size:11.5px; color:#7a4a00;",
                       HTML(paste0(
                         "<b>*&nbsp;Provisional indicator.</b> Calculated for all applicable ",
                         "supplies and included in every score shown here, but its methodology ",
                         "is still being revised, so its values are expected to change. See the ",
                         "Technical Documentation tab for what is under revision."))
                   ) else NULL,
                 tags$p(HTML(dist_caption),
                        style = "font-size:11.5px; color:#4f4f4f; margin-top:6px; margin-bottom:0;")
        ),
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
  
  ## Report Preview & Download (shared) ----
  # Every report -- single site, whole park, multi-park, batch -- funnels through
  # one preview modal. Nothing is written to the user's machine until they press
  # Download inside that modal, which is the point of the preview: report builds
  # are slow enough that silently handing back a file the user then discards is a
  # bad trade.
  staged_report <- reactiveVal(NULL)
  
  show_report_preview <- function(staged, title, subtitle = NULL) {
    staged_report(staged)
    showModal(modalDialog(
      title = title,
      size  = "l",
      easyClose = TRUE,
      if (!is.null(subtitle))
        tags$p(subtitle, style = "font-size:12px; color:#696969; margin:0 0 8px 0;"),
      tags$div(
        style = "display:flex; gap:10px; align-items:center; margin-bottom:10px; flex-wrap:wrap;",
        downloadButton("download_report", "Download Report (HTML)",
                       style = "background:#2a7f7f; color:white; border:none; border-radius:4px;
                                padding:8px 16px; font-size:13px; font-weight:600;"),
        tags$a(href = staged$url, target = "_blank",
               class = "btn btn-default btn-sm",
               style = "font-size:13px;",
               icon("up-right-from-square"), " Open in new tab"),
        tags$span(style = "font-size:11.5px; color:#888;",
                  "To save as PDF, open the report and use your browser's Print > Save as PDF.")
      ),
      tags$iframe(src = staged$url, class = "report-preview-frame",
                  title = "Report preview"),
      footer = modalButton("Close")
    ))
  }
  
  output$download_report <- downloadHandler(
    filename = function() {
      st <- staged_report()
      if (is.null(st)) "vulnerability_report.html" else st$download_name
    },
    content = function(file) {
      st <- staged_report()
      req(!is.null(st))
      file.copy(st$path, file, overwrite = TRUE)
    }
  )
  
  ## Single-Site Report (from map popup) ----
  observeEvent(input$generate_report_btn, {
    site_id <- input$generate_report_btn
    req(site_id %in% combined_raw$wsd_source_id)
    
    notif_id <- showNotification("Generating report...", duration = NULL,
                                 closeButton = FALSE, type = "message")
    tryCatch({
      site_meta <- combined_raw[combined_raw$wsd_source_id == site_id, ]
      html   <- build_site_report_html(site_id)
      staged <- stage_report(html, paste0("vulnerability_report_", site_id))
      removeNotification(notif_id)
      show_report_preview(
        staged,
        title = paste0("Report Preview: ", site_meta$park_name[1], " \u2013 ", site_id),
        subtitle = "Scored nationally against all CONUS NPS water supplies."
      )
    }, error = function(e) {
      removeNotification(notif_id)
      showNotification(paste0("Report generation failed: ", e$message),
                       type = "error", duration = 8)
    })
  })
  
  ## Advanced Report Controls ----
  # Park choices are drawn from the full dataset, not the filtered view, because
  # reports are deliberately national in scope; the "use the parks currently on
  # the map" link is the bridge for anyone who wants the filtered selection.
  observe({
    park_choices <- sort(unique(na.omit(as.data.frame(combined_data)$park_unit)))
    updateSelectizeInput(session, "report_park",
                         choices = park_choices,
                         selected = if (length(park_choices)) park_choices[1] else NULL,
                         server = TRUE)
    updateSelectizeInput(session, "report_parks_multi",
                         choices = park_choices, selected = character(0), server = TRUE)
  })
  
  observeEvent(input$report_use_filtered, {
    parks_now <- sort(unique(na.omit(as.data.frame(filtered_data_display())$park_unit)))
    if (length(parks_now) == 0) {
      showNotification("No parks are currently shown on the map.", type = "warning", duration = 5)
      return(invisible(NULL))
    }
    updateSelectizeInput(session, "report_parks_multi", selected = parks_now)
    showNotification(paste0("Selected ", length(parks_now), " park unit(s) from the current map view."),
                     type = "message", duration = 4)
  })
  
  output$report_scope_note <- renderUI({
    mode <- input$report_mode
    if (is.null(mode)) return(NULL)
    nat <- as.data.frame(combined_data)
    if (identical(mode, "park")) {
      req(input$report_park)
      n <- sum(!is.na(nat$park_unit) & nat$park_unit == input$report_park)
      div(style = "font-size:1.05rem; color:#457B9D; margin-top:6px;",
          paste0(n, " water supplies in this park unit."))
    } else {
      sel <- input$report_parks_multi
      if (length(sel) == 0)
        return(div(style = "font-size:1.05rem; color:#999; margin-top:6px; font-style:italic;",
                   "Select at least two park units to compare."))
      n <- sum(!is.na(nat$park_unit) & nat$park_unit %in% sel)
      warn <- if (identical(mode, "batch") && n > 60)
        " This is a large batch and may take several minutes to build." else ""
      div(style = "font-size:1.05rem; color:#457B9D; margin-top:6px;",
          paste0(length(sel), " park unit(s), ", n, " water supplies.", warn))
    }
  })
  
  ## Build Advanced Report ----
  observeEvent(input$build_advanced_report, {
    mode <- input$report_mode
    req(mode)
    nat <- as.data.frame(combined_data)
    
    if (identical(mode, "park")) {
      req(input$report_park)
      parks <- input$report_park
    } else {
      parks <- input$report_parks_multi
      if (length(parks) < 2) {
        showNotification("Select at least two park units for a multi-park or batch report.",
                         type = "warning", duration = 6)
        return(invisible(NULL))
      }
    }
    
    site_ids <- nat$wsd_source_id[!is.na(nat$park_unit) & nat$park_unit %in% parks]
    if (length(site_ids) == 0) {
      showNotification("No water supplies found for that selection.", type = "error", duration = 6)
      return(invisible(NULL))
    }
    
    # Hard ceiling on batch size. Each site in a batch triggers three
    # recalculations of the full index (park, state, region), so an unbounded
    # batch is an easy way to hang the session rather than a useful feature.
    BATCH_LIMIT <- 120
    if (identical(mode, "batch") && length(site_ids) > BATCH_LIMIT) {
      showNotification(
        paste0("That batch covers ", length(site_ids), " water supplies, above the ",
               BATCH_LIMIT, "-supply limit. Narrow the park selection, or use the ",
               "multi-park comparison report instead."),
        type = "error", duration = 10)
      return(invisible(NULL))
    }
    
    notif_id <- showNotification(
      paste0("Building ", switch(mode, park = "park", multipark = "multi-park", batch = "batch"),
             " report..."),
      duration = NULL, closeButton = FALSE, type = "message")
    
    tryCatch({
      if (identical(mode, "park")) {
        pname <- nat$park_name[!is.na(nat$park_unit) & nat$park_unit == parks][1]
        html   <- build_park_report_html(parks)
        staged <- stage_report(html, paste0("park_report_", parks))
        removeNotification(notif_id)
        show_report_preview(staged,
                            title = paste0("Report Preview: ", pname, " (", parks, ")"),
                            subtitle = paste0(length(site_ids),
                                              " water supplies, ranked nationally and within the park."))
        
      } else if (identical(mode, "multipark")) {
        html   <- build_multipark_report_html(parks)
        staged <- stage_report(html, paste0("multipark_report_", length(parks), "parks"))
        removeNotification(notif_id)
        show_report_preview(staged,
                            title = paste0("Report Preview: ", length(parks), "-Park Comparison"),
                            subtitle = paste0(length(site_ids),
                                              " water supplies across ", length(parks),
                                              " park units, all ranked on the same national scale."))
        
      } else {
        html <- build_batch_report_html(
          site_ids, scope_label = paste(sort(parks), collapse = ", "))
        staged <- stage_report(html, paste0("batch_report_", length(site_ids), "supplies"))
        removeNotification(notif_id)
        show_report_preview(staged,
                            title = paste0("Report Preview: Batch (", length(site_ids), " supplies)"),
                            subtitle = "One complete single-site report per supply, page-broken for printing.")
      }
    }, error = function(e) {
      removeNotification(notif_id)
      showNotification(paste0("Report generation failed: ", e$message),
                       type = "error", duration = 10)
    })
  })
  
  ## Feedback: Report a Bug / Request an Update ----
  # The form collects the report and the current app state, then hands it to the
  # user's mail client pre-addressed to FEEDBACK_EMAIL. Nothing is written to
  # disk and the app sends no mail itself, so the submission exists only in the
  # browser until the user presses send -- which is why the confirmation modal
  # says so plainly and also offers the raw text to copy.
  observeEvent(input$feedback_btn_footer, {
    # Same form, second entry point.
    showModal(modalDialog(
      title = tagList(icon("bug", style = "color:#8B4A2B; margin-right:6px;"),
                      "Report an Issue or Request an Update"),
      size  = "m",
      easyClose = TRUE,
      tags$p("Tell us what went wrong or what you would like the tool to do.
              Please be specific about which water supply, park, or view you were looking at.",
             style = "font-size:13px; color:#333;"),
      selectInput("feedback_type", "What kind of feedback is this?",
                  choices = c("Bug or unexpected behavior" = "bug",
                              "Data or score looks wrong"   = "data",
                              "Feature request / update"    = "feature",
                              "Documentation or wording"    = "docs",
                              "General comment"             = "general"),
                  selected = "bug"),
      selectInput("feedback_area", "Where in the tool?",
                  choices = c("Map / vulnerability scores" = "map",
                              "Indicator view"             = "indicator",
                              "Filters"                    = "filters",
                              "Data table"                 = "table",
                              "Score breakdown popup"      = "breakdown",
                              "Reports"                    = "reports",
                              "Water Balance Model Explorer" = "wbm",
                              "Technical Documentation"    = "docs",
                              "Other / not sure"           = "other"),
                  selected = "map"),
      textAreaInput("feedback_text", "Description", value = "", rows = 5,
                    placeholder = "What did you expect to happen, and what happened instead?"),
      textInput("feedback_contact", "Your name or email (optional)", value = "",
                placeholder = "So we can follow up if we need more detail"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("feedback_submit", "Submit Feedback",
                     class = "btn-primary", style = "font-weight:600;")
      )
    ))
  })
  
  observeEvent(input$feedback_btn, {
    showModal(modalDialog(
      title = tagList(icon("bug", style = "color:#8B4A2B; margin-right:6px;"),
                      "Report an Issue or Request an Update"),
      size  = "m",
      easyClose = TRUE,
      tags$p("Tell us what went wrong or what you would like the tool to do.
              Please be specific about which water supply, park, or view you were looking at.",
             style = "font-size:13px; color:#333;"),
      selectInput("feedback_type", "What kind of feedback is this?",
                  choices = c("Bug or unexpected behavior" = "bug",
                              "Data or score looks wrong"   = "data",
                              "Feature request / update"    = "feature",
                              "Documentation or wording"    = "docs",
                              "General comment"             = "general"),
                  selected = "bug"),
      selectInput("feedback_area", "Where in the tool?",
                  choices = c("Map / vulnerability scores" = "map",
                              "Indicator view"             = "indicator",
                              "Filters"                    = "filters",
                              "Data table"                 = "table",
                              "Score breakdown popup"      = "breakdown",
                              "Reports"                    = "reports",
                              "Water Balance Model Explorer" = "wbm",
                              "Technical Documentation"    = "docs",
                              "Other / not sure"           = "other"),
                  selected = "map"),
      textAreaInput("feedback_text", "Description", value = "", rows = 5,
                    placeholder = "What did you expect to happen, and what happened instead?"),
      textInput("feedback_contact", "Your name or email (optional)", value = "",
                placeholder = "So we can follow up if we need more detail"),
      tags$p(style = "font-size:11px; color:#888; margin-top:6px;",
             "We also record which region, state and park filters were active when you
              clicked, plus the app version, so we can reproduce what you were seeing."),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("feedback_submit", "Submit Feedback",
                     class = "btn-primary", style = "font-weight:600;")
      )
    ))
  })
  
  observeEvent(input$feedback_submit, {
    txt <- trimws(if (is.null(input$feedback_text)) "" else input$feedback_text)
    if (nchar(txt) < 10) {
      showNotification("Please add a bit more detail before submitting (at least a sentence).",
                       type = "warning", duration = 5)
      return(invisible(NULL))
    }
    
    ctx <- paste0(
      "Region: ",  if (length(input$filter_region) > 0) paste(input$filter_region, collapse = "; ") else "All",
      " | State: ", if (length(input$filter_state) > 0) paste(input$filter_state, collapse = "; ") else "All",
      " | Park: ",  if (length(input$filter_park) > 0) paste(input$filter_park, collapse = "; ") else "All",
      " | Source type: ", if (length(input$filter_source_type) > 0) paste(input$filter_source_type, collapse = "; ") else "All",
      " | Hazard flags: ", if (length(input$filter_flags) > 0) paste(input$filter_flags, collapse = "; ") else "None",
      " | Priority only: ", isTRUE(input$filter_priority),
      " | View mode: ", if (is.null(input$view_mode)) "" else input$view_mode,
      " | Selected supply: ", if (is.null(clicked_site())) "none" else clicked_site(),
      " | App version: ", APP_VERSION
    )
    
    # Nothing is written to disk. Whatever the user types stays in this browser
    # session until they send it themselves -- there is no server-side copy to
    # locate, no ambiguity about where a submission went, and nothing that could
    # quietly accumulate identifiable contact details in a file no one reads.
    # (This used to build a six-column data frame to append as a CSV row; only
    # the timestamp was ever read back into the message, so that is all that is
    # left. Every other field goes into the body or the subject directly.)
    submitted <- format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC")
    
    subject <- paste0("[NPS WSVA Tool] ", toupper(input$feedback_type), " - ", input$feedback_area)
    body    <- paste0(txt, "\n\n---\nContext: ", ctx,
                      "\nSubmitted: ", submitted, " UTC",
                      if (nzchar(input$feedback_contact))
                        paste0("\nContact: ", input$feedback_contact) else "")
    mailto  <- paste0("mailto:", FEEDBACK_EMAIL,
                      "?subject=", utils::URLencode(subject, reserved = TRUE),
                      "&body=",    utils::URLencode(body, reserved = TRUE))
    
    removeModal()
    showModal(modalDialog(
      title = tagList(icon("envelope-open-text", style = "color:#386150; margin-right:6px;"),
                      "Almost done"),
      size = "m", easyClose = TRUE,
      tags$div(
        style = "background:#fff8e6; border-left:3px solid #8B6914; border-radius:4px;
                 padding:10px 14px; font-size:12px; color:#5a4a1a; margin:0 0 10px 0;",
        HTML(paste0(
          "<b>Your feedback has not been sent yet.</b> The app does not store submissions or ",
          "send mail on its own. Use the button below: it opens a pre-filled message to <b>",
          htmltools::htmlEscape(FEEDBACK_EMAIL), "</b> in your mail client, and you still have ",
          "to press send there. Closing this window without sending discards it."))),
      tags$div(
        style = "display:flex; gap:10px; flex-wrap:wrap; margin-top:6px;",
        tags$a(href = mailto, class = "btn btn-primary btn-sm",
               style = "font-weight:600;", icon("envelope"), " Send as email")
      ),
      tags$details(
        style = "margin-top:12px;",
        # With no log and no server-side send, this fallback is the only thing
        # standing between a lost submission and a browser with no mail client
        # registered -- clicking mailto: there does nothing at all, with no error.
        # Long submissions can also exceed the URL length some clients accept,
        # silently truncating the body. Copying the text always works, so it is
        # open by default rather than hidden behind a click.
        open = NA,
        tags$summary("Or copy the text and email it yourself",
                     style = "font-size:12px; cursor:pointer; color:#457B9D;"),
        tags$p(HTML(paste0("Send to <b>", htmltools::htmlEscape(FEEDBACK_EMAIL), "</b>",
                           " with the subject <b>", htmltools::htmlEscape(subject), "</b>:")),
               style = "font-size:11px; color:#666; margin:6px 0 4px 0;"),
        tags$pre(body, style = "white-space:pre-wrap; font-size:11px; background:#f7fafc;
                                border:1px solid #dce8ef; border-radius:4px; padding:10px;")
      ),
      footer = modalButton("Close")
    ))
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
      df_sf     <- map_indicator_filtered_data()
      df        <- as.data.frame(df_sf)
      n_sites   <- nrow(df)
      
      # ---- Current map viewport bounds ----
      bounds    <- input$map_bounds
      xlim_use  <- if (!is.null(bounds)) c(bounds$west, bounds$east)  else c(-170, -65)
      ylim_use  <- if (!is.null(bounds)) c(bounds$south, bounds$north) else c(17, 72)
      
      # ---- Metadata strings ----
      if (input$view_mode == "score") {
        map_title    <- input$score_view
        map_subtitle <- "Percentile Rank"
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
        if (length(input$filter_flags) > 0) paste0(
          "Hazard Flags (",
          if (identical(input$filter_flags_mode, "all")) "all of" else "any of", "): ",
          paste(c(flag_fire = "Fire", flag_flood = "Flood", flag_slr = "Sea Level Rise",
                  flag_drought = "Drought")[input$filter_flags], collapse=", ")),
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
                           "sen_runoff_trend","sen_precip_trend", "sen_source_type", "sen_drought_trend")
      is_inverted <- (input$view_mode == "indicator") && col %in% invert_raw_cols
      
      pal_colors <- if (is_inverted) rev(c("#FFF3D6","#F0C75E","#DD8844","#C05235","#9B2226"))
      else                  c("#FFF3D6","#F0C75E","#DD8844","#C05235","#9B2226")
      
      is_rank    <- (input$view_mode == "score")
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
        map_decorations(xlim_use, ylim_use, deco_scale = 1.15) +
        ggplot2::coord_sf(xlim = xlim_use, ylim = ylim_use, expand = FALSE) +
        ggplot2::labs(
          title    = map_title,
          subtitle = paste0(
            map_subtitle, "  |  Scope: ", scope_label, "  |  Sites: ", n_sites,
            if (filter_str != "None (national)") paste0("\nFilters: ", filter_str) else "",
            "\nData: ", DATA_LAST_UPDATED, "  |  Exported: ", format(Sys.Date(), "%B %d, %Y")
          ),
          caption  = paste0(
            "NPS Water Supply Vulnerability Assessment Tool\n",
            "Generated from ", APP_URL_DISPLAY, " on ", format(Sys.Date(), "%B %d, %Y"), "\n",
            "Colorado State University Geospatial Centroid")
        ) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
          plot.title       = ggplot2::element_text(color = comp_color, face = "bold", size = 14),
          plot.subtitle    = ggplot2::element_text(color = "#555", size = 8.5, lineheight = 1.3),
          plot.caption     = ggplot2::element_text(color = "#888", size = 8, hjust = 0.5,
                                                   lineheight = 1.25),
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
    
    base_data <- map_indicator_filtered_data()
    coords    <- st_coordinates(base_data)
    in_view   <- coords[, 1] >= bounds$west  & coords[, 1] <= bounds$east &
      coords[, 2] >= bounds$south & coords[, 2] <= bounds$north
    
    df <- as.data.frame(base_data[in_view, ]) %>%
      select(wsd_source_id, water_system_name,
             park_unit, park_name, state, region,
             source_type,
             VULNERABILITY_rank, EXPOSURE_rank, SENSITIVITY_rank,
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
    
    #vuln_max <- max(df$VULNERABILITY_rank, na.rm = TRUE)
    
    # Move priority_flags to be right after the ID/geo columns, before score columns
    df <- df %>%
      relocate(priority_flags, .before = VULNERABILITY_rank)
    
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
                order          = list(list(which(names(df) == "VULNERABILITY_rank") - 1, "desc")),                dom            = "Bfrtip",
                buttons        = list("csv", "excel"),
                columnDefs     = list(
                  list(className = "dt-center", targets = "_all"),
                  list(width = "120px", targets = 0:6),   # ID/name/geo/type cols
                  list(width = "300px", targets = 7),     # flags col
                  list(width = "80px",  targets = 8:11)   # score + quartile cols
                )
              )) #%>% 
    # formatStyle("VULNERABILITY_rank",
    #             background         = styleColorBar(c(0, vuln_max), "#C05235"),
    #             backgroundSize     = "100% 80%",
    #             backgroundRepeat   = "no-repeat",
    #             backgroundPosition = "center")
  })
  
  ## Water Balance Model Explorer ----
  # Reads wbm_data (loaded once at startup), reshapes the selected
  # variable x period slice into a single value column, and draws it. Nothing
  # here feeds the vulnerability index: this tab exists so a reviewer can see the
  # hydroclimate the indicators are derived from, and check whether a surprising
  # score is being driven by a surprising input.
  
  # Park choices follow the region filter, same cascade as the main map.
  observe({
    regions <- input$wbm_region
    pool <- if (length(regions) > 0) wbm_data[wbm_data$region %in% regions, ] else wbm_data
    parks <- sort(unique(na.omit(pool$park_unit)))
    updateSelectizeInput(session, "wbm_park", choices = parks,
                         selected = intersect(input$wbm_park, parks), server = TRUE)
  })
  
  wbm_meta_current <- reactive({
    req(input$wbm_variable)
    wbm_var_meta[[input$wbm_variable]]
  })
  
  # Percent change in degrees Celsius is not a meaningful quantity, so the metric
  # is removed for temperature rather than left selectable and caveated. If the
  # user was already on it when they switched variable, move them to absolute.
  observeEvent(list(input$wbm_variable, input$wbm_metric), {
    meta <- wbm_meta_current()
    req(!is.null(meta))
    if (!isTRUE(meta$pct_ok) && identical(input$wbm_metric, "pct")) {
      updateRadioButtons(session, "wbm_metric", selected = "abs")
      showNotification(
        paste0(meta$label, " change is reported in ", meta$unit,
               ", not as a percentage. Switched to absolute change."),
        type = "message", duration = 6)
    }
  }, ignoreInit = TRUE)
  
  #' The selected slice, with a single `value` column and the units that go with
  #' it. Absolute change is derived as historical x (percent change / 100) rather
  #' than as (future mean - historical mean): the gpkg carries percentile change
  #' as a percentage and the raw values as ensemble means, so deriving it this way
  #' keeps the absolute number consistent with the percentile the user selected.
  #' For temperature the change columns are already in degrees C, so they are
  #' used directly.
  wbm_slice <- reactive({
    req(input$wbm_variable, input$wbm_period, input$wbm_metric)
    meta <- wbm_meta_current()
    stat <- if (is.null(input$wbm_stat)) "change_p50" else input$wbm_stat
    
    d <- wbm_data[wbm_data$variable == input$wbm_variable &
                    wbm_data$period == input$wbm_period, ]
    if (length(input$wbm_region) > 0) d <- d[d$region %in% input$wbm_region, ]
    if (length(input$wbm_park)   > 0) d <- d[d$park_unit %in% input$wbm_park, ]
    req(nrow(d) > 0)
    
    pct_is_degrees <- !isTRUE(meta$pct_ok)   # temperature
    
    d$value <- switch(
      input$wbm_metric,
      pct    = suppressWarnings(as.numeric(d[[stat]])),
      abs    = if (pct_is_degrees) suppressWarnings(as.numeric(d[[stat]]))
      else suppressWarnings(as.numeric(d$raw_historical_value) *
                              as.numeric(d[[stat]]) / 100),
      hist   = suppressWarnings(as.numeric(d$raw_historical_value)),
      future = suppressWarnings(as.numeric(d$raw_future_value))
    )
    # Non-finite values (a percent change against a zero baseline, most often
    # SWE in a snow-free location) are dropped to NA rather than plotted: an Inf
    # would take the whole colour ramp with it.
    d$value[!is.finite(d$value)] <- NA_real_
    
    units <- switch(
      input$wbm_metric,
      pct    = if (pct_is_degrees) meta$unit else "%",
      abs    = meta$unit,
      hist   = meta$unit,
      future = meta$unit
    )
    # Change metrics are signed and want a diverging ramp centred on zero;
    # raw values are one-sided and want a sequential one.
    list(data = d, units = units, meta = meta,
         diverging = input$wbm_metric %in% c("pct", "abs"), stat = stat)
  })
  
  wbm_metric_label <- reactive({
    s <- wbm_slice()
    stat_lbl <- c(change_p10 = "p10", change_p50 = "median",
                  change_p90 = "p90", change_min = "min", change_max = "max")[[s$stat]]
    switch(input$wbm_metric,
           pct    = paste0("Change (%, ", stat_lbl, ")"),
           abs    = paste0("Change (", s$units, ", ", stat_lbl, ")"),
           hist   = paste0("Historical mean (", s$units, ")"),
           future = paste0("Future mean (", s$units, ")"))
  })
  
  #' Colour domain. Clipping to the 2nd-98th percentile is on by default because
  #' the percent-change columns are genuinely long-tailed -- SWE reaches values in
  #' the millions where the historical baseline rounds to zero -- and an unclipped
  #' ramp renders every other site the same colour. Diverging ramps are forced
  #' symmetric about zero so that the midpoint colour always means "no change".
  wbm_domain <- reactive({
    s <- wbm_slice()
    v <- s$data$value[is.finite(s$data$value)]
    req(length(v) > 0)
    rng <- if (isTRUE(input$wbm_clip)) unname(quantile(v, c(0.02, 0.98), na.rm = TRUE)) else range(v)
    if (s$diverging) {
      m <- max(abs(rng), na.rm = TRUE)
      if (!is.finite(m) || m == 0) m <- 1
      c(-m, m)
    } else {
      if (!is.finite(rng[1]) || rng[1] == rng[2]) rng <- c(rng[1] - 0.5, rng[1] + 0.5)
      rng
    }
  })
  
  wbm_palette <- reactive({
    s   <- wbm_slice()
    dom <- wbm_domain()
    cols <- if (s$diverging) {
      # Brown (drying / loss) through white to blue-green (wetting / gain).
      # Reversed for temperature and PET, where "more" is the adverse direction
      # rather than the beneficial one, so warm colours always read as pressure.
      if (input$wbm_variable %in% c("temp", "pet"))
        c("#2A7F7F", "#A8DADC", "#F7F7F7", "#DD8844", "#9B2226")
      else
        c("#9B2226", "#DD8844", "#F7F7F7", "#7FC4C4", "#1D6E6E")
    } else {
      c("#FFF3D6", "#A8DADC", "#457B9D", "#1D3557")
    }
    colorNumeric(cols, domain = dom, na.color = "#cfcfcf")
  })
  
  output$wbm_var_desc <- renderUI({
    meta <- wbm_meta_current()
    req(!is.null(meta))
    div(
      div(style = "font-size:1.05rem; font-weight:700; color:#1D3557;", meta$label),
      div(style = "font-size:0.95rem; color:#666; margin-bottom:6px;",
          paste0("Units: ", meta$unit, " \u00b7 aggregated as ", meta$agg)),
      div(style = "font-size:1rem; color:#444; line-height:1.45;", meta$desc)
    )
  })
  
  output$wbm_summary_cards <- renderUI({
    s <- wbm_slice()
    v <- s$data$value[is.finite(s$data$value)]
    n_total <- nrow(s$data)
    n_cov   <- sum(!s$data$in_coverage, na.rm = TRUE)
    card <- function(label, val, color = "#1D3557") {
      div(style = paste0("flex:1;min-width:130px;background:white;border-radius:8px;padding:10px 12px;",
                         "border-top:3px solid ", color, ";box-shadow:0 1px 4px rgba(0,0,0,0.07);text-align:center;"),
          div(style = "font-size:0.9rem;color:#696969;text-transform:uppercase;letter-spacing:0.04em;", label),
          div(style = paste0("font-size:1.5rem;font-weight:800;color:", color, ";"), val))
    }
    fmt <- function(x) if (!is.finite(x)) "\u2014" else sprintf("%.1f", x)
    div(style = "display:flex;gap:10px;flex-wrap:wrap;margin-bottom:10px;",
        card("Supplies shown", n_total, "#386150"),
        card(paste0("Median (", s$units, ")"), fmt(stats::median(v)), "#1D3557"),
        card(paste0("10th pct (", s$units, ")"), fmt(unname(quantile(v, 0.1))), "#457B9D"),
        card(paste0("90th pct (", s$units, ")"), fmt(unname(quantile(v, 0.9))), "#C05235"),
        if (n_cov > 0) card("Outside model coverage", n_cov, "#8B6914") else NULL)
  })
  
  # Both maps are built from the same 1,407 water supplies, and the main map
  # identifies its markers with a bare `layerId = wsd_source_id`. Anything this
  # map adds is therefore namespaced with a "wbm_" prefix -- markers, groups and
  # controls alike -- so the two maps can never address each other's layers even
  # though they share an identifier space. Clicks come back on
  # input$wbm_map_marker_click carrying the prefixed id, so strip the prefix
  # before matching it against wsd_source_id.
  WBM_ID_PREFIX <- "wbm_"
  
  # Markers are drawn INSIDE renderLeaflet rather than pushed in afterwards with
  # leafletProxy. This tab is not the tab the app opens on, and Shiny suspends an
  # output whose container is hidden: renderLeaflet had not run, so the widget
  # did not exist on the client and every proxy message was delivered to nothing
  # and dropped. Forcing the output to stay live with suspendWhenHidden = FALSE
  # fixes that symptom but makes two leaflet widgets initialise in the same
  # flush, which is what knocked the markers off the main map. Building the
  # layer here instead means the points exist the moment the map paints, there
  # is no proxy ordering to lose, and nothing about the main map changes.
  output$wbm_map <- renderLeaflet({
    s   <- wbm_slice()
    pal <- wbm_palette()
    dom <- wbm_domain()
    d   <- s$data
    d   <- d[is.finite(d$source_longitude) & is.finite(d$source_latitude), ]
    
    # Re-rendering resets the viewport, which is jarring when the user has zoomed
    # into a park and is stepping through variables. The current view is read
    # back (isolated, so it is not itself a dependency) and restored; on first
    # paint there is no view yet and it falls back to CONUS.
    ctr <- isolate(input$wbm_map_center)
    zm  <- isolate(input$wbm_map_zoom)
    
    m <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Terrain") %>%
      addLayersControl(baseGroups = c("Light", "Satellite", "Terrain"),
                       options = layersControlOptions(position = "topleft"))
    
    m <- if (!is.null(ctr) && !is.null(zm)) {
      setView(m, lng = ctr$lng, lat = ctr$lat, zoom = zm)
    } else {
      setView(m, lng = -98.6, lat = 39.5, zoom = 4)
    }
    
    if (nrow(d) == 0) return(m)
    
    # Colours are clipped to the domain, values in the popup are not, so a site
    # in the tail reads as "at least this extreme" on the map and still reports
    # its true number when clicked.
    clipped <- pmin(pmax(d$value, dom[1]), dom[2])
    
    legend_note <- if (isTRUE(input$wbm_clip))
      "<div style='font-size:10px;color:#888;margin-top:4px;'>Colour clipped to 2nd-98th pct</div>" else ""
    
    m %>%
      addCircleMarkers(
        data = d,
        lng = ~source_longitude, lat = ~source_latitude,
        group = "wbm_points",
        layerId = paste0(WBM_ID_PREFIX, d$wsd_source_id),
        radius = 5, weight = 1, color = "#333333", opacity = 0.7,
        fillColor = pal(clipped), fillOpacity = 0.85,
        popup = wbm_popups(d, s),
        label = ~park_name,
        labelOptions = labelOptions(textsize = "12px", direction = "auto")
      ) %>%
      addLegend(position = "bottomright", pal = pal, values = dom,
                title = paste0(s$meta$label, "<br><span style='font-weight:400;font-size:11px;'>",
                               wbm_metric_label(), "</span>", legend_note),
                opacity = 0.9, layerId = "wbm_legend")
  })
  
  #' Popup HTML for the explorer markers. Split out of the map so the marker
  #' layer can be built inside renderLeaflet without that call growing to fifty
  #' lines, and so the popup text has one definition rather than one per caller.
  wbm_popups <- function(d, s) {
    unit_fmt <- function(x) if (is.na(x)) "no data" else sprintf("%.2f %s", x, s$units)
    raw_fmt  <- function(x) if (is.na(x)) "no data" else sprintf("%.1f %s", x, s$meta$unit)
    num_fmt  <- function(x) ifelse(is.na(x), "\u2014", sprintf("%.1f", as.numeric(x)))
    
    paste0(
      "<div style='font-family:Arial;font-size:12px;min-width:220px;'>",
      "<b style='color:#1D3557;font-size:13px;'>", htmltools::htmlEscape(d$park_name), "</b><br>",
      "<span style='font-size:11px;color:#666;'>",
      htmltools::htmlEscape(ifelse(is.na(d$water_system_name), "", d$water_system_name)),
      " \u00b7 ", htmltools::htmlEscape(d$wsd_source_id), "</span>",
      "<hr style='margin:6px 0;border-color:#ddd;'>",
      "<b>", htmltools::htmlEscape(s$meta$label), "</b> \u2014 ",
      htmltools::htmlEscape(wbm_metric_label()), "<br>",
      "<span style='font-size:17px;font-weight:800;color:#1D3557;'>",
      vapply(d$value, unit_fmt, character(1)), "</span>",
      "<hr style='margin:6px 0;border-color:#ddd;'>",
      "<b>Historical (", WBM_HIST_PERIOD, "):</b> ",
      vapply(as.numeric(d$raw_historical_value), raw_fmt, character(1)), "<br>",
      "<b>Future (", htmltools::htmlEscape(d$future_period), "):</b> ",
      vapply(as.numeric(d$raw_future_value), raw_fmt, character(1)), "<br>",
      "<b>Ensemble change:</b> p10 ", num_fmt(d$change_p10),
      " \u00b7 p50 ", num_fmt(d$change_p50),
      " \u00b7 p90 ", num_fmt(d$change_p90),
      " <span style='color:#888;'>(", htmltools::htmlEscape(d$change_units), ")</span><br>",
      "<span style='font-size:11px;color:#888;'>n = ",
      ifelse(is.na(d$n_projections), "\u2014", as.character(round(d$n_projections))),
      " projections \u00b7 source type: ",
      htmltools::htmlEscape(ifelse(is.na(d$source_type), "N/A", d$source_type)), "</span>",
      ifelse(d$in_coverage, "",
             "<br><span style='font-size:11px;color:#8B6914;'>Outside MWBM grid coverage.</span>"),
      "</div>")
  }
  
  output$wbm_hist <- renderPlotly({
    s <- wbm_slice()
    v <- s$data$value[is.finite(s$data$value)]
    req(length(v) > 0)
    dom <- wbm_domain()
    plot_ly(x = v, type = "histogram", nbinsx = 45,
            marker = list(color = "#457B9D", line = list(color = "white", width = 0.5)),
            hovertemplate = paste0("%{x:.1f} ", s$units, "<br>%{y} supplies<extra></extra>")) %>%
      layout(
        title = list(text = paste0("Distribution across the ", length(v), " supplies shown"),
                     font = list(size = 12, color = "#1D3557"), x = 0, xanchor = "left"),
        xaxis = list(title = list(text = wbm_metric_label(), standoff = 8),
                     range = if (isTRUE(input$wbm_clip)) dom else NULL,
                     tickfont = list(size = 10), gridcolor = "#eee", zeroline = s$diverging,
                     zerolinecolor = "#999"),
        yaxis = list(title = "Supplies", tickfont = list(size = 10), gridcolor = "#eee"),
        margin = list(t = 34, l = 50, r = 20, b = 44),
        bargap = 0.04, plot_bgcolor = "white", paper_bgcolor = "white", showlegend = FALSE)
  })
  
  output$wbm_caption <- renderUI({
    s <- wbm_slice()
    n_na <- sum(!is.finite(s$data$value))
    tags$p(HTML(paste0(
      "Values are LOCA2 projections processed through the USGS Monthly Water Balance Model ",
      "(Alder &amp; USGS, 2023; Hostetler &amp; Alder, 2016), extracted from the 3x3 grid-cell ",
      "neighbourhood centred on each water supply and aggregated to a <b>", s$meta$agg,
      "</b>, then averaged across the historical (", WBM_HIST_PERIOD, ") and future (",
      s$data$future_period[1], ") periods for each of the ",
      "GCM x SSP combinations. Each future projection is compared against the hindcast from the ",
      "same climate model. Percentile statistics describe the spread across that ensemble and are ",
      "<b>not probabilities</b>: 72 projections are not 72 independent futures, and the SSPs are ",
      "scenarios rather than outcomes with assigned likelihoods. ",
      "The MWBM generates runoff locally without routing it through a stream network and does not ",
      "represent streamflow, groundwater, reservoirs or diversions, so these are hydroclimatic ",
      "indicators rather than forecasts of water yield at an intake. ",
      if (identical(input$wbm_metric, "pct"))
        paste0("Percent change becomes unstable where the historical baseline is near zero &mdash; ",
               "switch to <i>Historical value</i> to see which sites those are. ") else "",
      if (n_na > 0) paste0("<b>", n_na, "</b> of the supplies shown have no value for this ",
                           "selection and are drawn in grey. ") else "",
      "Nothing on this tab enters the vulnerability index.")),
      style = "font-size:11.5px; color:#4f4f4f; margin-top:8px;")
  })
  
  ## Technical Report PDF download ----
  # Reads the PDF straight out of www/. The "Open in new tab" link on the same
  # panel hits the identical file over Shiny's static handler; this route exists
  # so the browser is told to save rather than to display it inline, and so the
  # saved file carries the canonical name. req() rather than a hard failure so
  # that if the PDF is removed from the deployment between restarts the button
  # does nothing instead of throwing an error into the UI.
  output$download_tech_report <- downloadHandler(
    filename    = function() TECH_REPORT_FILE,
    contentType = "application/pdf",
    content     = function(file) {
      req(TECH_REPORT_AVAILABLE, file.exists(TECH_REPORT_SRC))
      file.copy(TECH_REPORT_SRC, file, overwrite = TRUE)
    }
  )
  
  ## Popup Builder Function ----
  create_popup <- function(view_mode, score_label, component, factor_name,
                           indicator_name, raw_value, raw_label, col_value,
                           vuln_rank, exp_rank, sen_rank,
                           priority, flag_fire, flag_flood, flag_slr, flag_drought,
                           wsd_source_id, park_unit, park_name, water_system_name,
                           source_type, description, state,
                           fmss_number, state_water_rights_id,
                           surface_water_law, ground_water_law) {
    
    blank_or <- function(x, fallback = "<span style='color:#999;'>Not recorded</span>") {
      if (length(x) == 0 || is.na(x) || !nzchar(trimws(as.character(x)))) fallback
      else htmltools::htmlEscape(trimws(as.character(x)))
    }
    
    top_section <- if (view_mode == "score") {
      paste0("<b style='color:#386150; font-size:14px;'>", score_label, "</b><br>",
             "<b>Percentile Rank:</b> <span style='font-size:14px; font-weight:bold;'>",
             round(col_value, 1), "%</span>")
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
    
    # Water rights block. Both doctrines are shown, with the one that governs
    # this source type marked, because a reader who only sees "Prior
    # Appropriation" cannot tell whether that is the surface or groundwater rule.
    regime <- wr_primary_regime(source_type)
    mark <- function(which_one) {
      if (!is.na(regime) && identical(regime, which_one))
        " <span style='background:#2a7f7f;color:white;padding:0 4px;border-radius:2px;font-size:9px;'>APPLIES</span>"
      else ""
    }
    rights_section <- paste0(
      "<hr style='margin:6px 0; border-color:#ddd;'>",
      "<div style='font-size:10px; font-weight:700; text-transform:uppercase; ",
      "letter-spacing:0.05em; color:#1D3557; margin-bottom:2px;'>Water Rights Context</div>",
      "<b>Surface Water Law:</b> ", blank_or(surface_water_law), mark("surface"), "<br>",
      "<b>Groundwater Law:</b> ",   blank_or(ground_water_law),  mark("groundwater"), "<br>",
      "<b>State Water Right ID:</b> ", blank_or(state_water_rights_id), "<br>",
      if (!is.na(regime) && identical(regime, "groundwater") &&
          !is.na(source_type) && grepl("spring", tolower(source_type)))
        paste0("<span style='font-size:10px; color:#7a4a00;'>Spring sources are treated ",
               "under the groundwater regime here; several states classify them differently.</span><br>")
      else "",
      "<span style='font-size:9px; color:#888; font-style:italic; display:block; line-height:1.35;'>",
      "State legal framework for context only, not a determination of this system's water right. ",
      "Contributes nothing to any score.</span>")
    
    site_section <- paste0(
      "<hr style='margin:6px 0; border-color:#ddd;'>",
      "<b>Water System:</b> ",    water_system_name, "<br>",
      "<b>Water Supply ID:</b> ", wsd_source_id, "<br>",
      "<b>FMSS Number:</b> ",     blank_or(fmss_number), "<br>",
      "<b>Park Unit:</b> ",       park_unit,      "<br>",
      "<b>Park Name:</b> ",       park_name,      "<br>",
      "<b>Source Type:</b> ",     ifelse(is.na(source_type), "<span style='color:#999;'>N/A</span>", source_type), "<br>",
      "<b>State:</b> ",           state,           "<br>",
      if (!is.na(description) && nchar(trimws(description)) > 0)
        paste0("<a href='#' onclick=\"Shiny.setInputValue('show_description_btn', '",
               wsd_source_id, "', {priority:'event'}); return false;\" ",
               "style='font-size:11px; color:#457B9D;'><span aria-hidden='true'>&#x1F4C4;</span> View Full Description</a><br>")
      else paste0("<span style='font-size:11px; color:#999; font-style:italic;'>",
                  "No water supply description available.</span><br>"))
    
    button_section <- paste0(
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
           top_section, score_section, flag_section, site_section,
           rights_section, button_section, "</div>")
  }
}

shinyApp(ui = ui, server = server)