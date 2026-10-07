# End-to-end smoke tests: the real rapid_app running in headless Chrome.
#
# These launch the full app (app.R sources calc_vulnerability_index.R and reads
# everything in app_data/), so they are slower and need shinytest2 + Chrome on
# the machine. The whole file is skipped where either is missing.

testthat::skip_if_not_installed("shinytest2")
testthat::skip_if_not_installed("chromote")
library(shinytest2)

# AppDriver$new() skips itself unless NOT_CRAN is "true". This app is never
# checked on CRAN, so run wherever Chrome is available.
withr::local_envvar(NOT_CRAN = "true")

app_dir <- testthat::test_path("..", "..")

skip_if_no_chrome <- function() {
  chrome <- tryCatch(chromote::find_chrome(), error = function(e) "")
  if (is.null(chrome) || !nzchar(chrome)) skip("Chrome/Chromium not available")
}

# "Showing 1 to 25 of 1,409 entries" -> 1409
table_total <- function(app) {
  txt <- app$get_text(".dataTables_info")
  hit <- regmatches(txt, regexec("of ([0-9,]+) entries", txt))[[1]]
  if (length(hit) < 2) return(NA_integer_)
  as.integer(gsub(",", "", hit[2]))
}

# Any filter change re-fits the map to the new extent (see app.R's "Zoom to
# Filtered Extent" observer), and the table re-renders against
# map_bounds_debounced() -- a 400ms debounce on the resulting viewport change.
# The table therefore re-renders TWICE per filter change: once immediately
# against the still-stale bounds, then again once the debounce fires. The
# server is genuinely idle in between, so wait_for_idle() can return before
# the second render and read the stale count. Poll instead of trusting one
# idle check.
wait_until <- function(condition, timeout = 10, interval = 0.25) {
  deadline <- Sys.time() + timeout
  repeat {
    if (isTRUE(condition())) return(invisible(TRUE))
    if (Sys.time() > deadline) return(invisible(FALSE))
    Sys.sleep(interval)
  }
}


test_that("the app boots and the default map renders", {
  skip_if_no_chrome()

  app <- AppDriver$new(app_dir, name = "boot", timeout = 60 * 1000)
  withr::defer(app$stop())
  app$wait_for_idle(timeout = 60 * 1000)

  # no Shiny-side errors surfaced to the browser console
  logs <- app$get_logs()
  shiny_errors <- logs[logs$location == "shiny" & logs$level == "error", ]
  expect_equal(nrow(shiny_errors), 0)

  # the Leaflet map output produced a value
  expect_false(is.null(app$get_value(output = "map")))
})


test_that("the data table renders a non-empty baseline", {
  skip_if_no_chrome()

  app <- AppDriver$new(app_dir, name = "table-baseline", timeout = 60 * 1000)
  withr::defer(app$stop())
  app$wait_for_idle(timeout = 60 * 1000)

  total <- table_total(app)
  expect_false(is.na(total))
  expect_gt(total, 0)

  # the table can never show more rows than there are water supplies
  supplies <- readr::read_csv(
    file.path(app_dir, "app_data", "water_supplies.csv"),
    show_col_types = FALSE
  )
  expect_lte(total, nrow(supplies))
})


test_that("a region filter narrows the table and Clear All Filters restores it", {
  skip_if_no_chrome()

  supplies <- readr::read_csv(
    file.path(app_dir, "app_data", "water_supplies.csv"),
    show_col_types = FALSE
  )
  region_counts <- sort(table(supplies$region), decreasing = TRUE)
  skip_if(length(region_counts) < 2, "only one region in the data")
  busiest_region <- names(region_counts)[1]

  app <- AppDriver$new(app_dir, name = "region-filter", timeout = 60 * 1000)
  withr::defer(app$stop())
  app$wait_for_idle(timeout = 60 * 1000)

  baseline <- table_total(app)

  app$set_inputs(filter_region = busiest_region)
  app$wait_for_idle(timeout = 60 * 1000)
  wait_until(function() {
    total <- table_total(app)
    !is.na(total) && total < baseline
  })
  expect_lt(table_total(app), baseline)

  app$click("clear_filters")
  app$wait_for_idle(timeout = 60 * 1000)
  wait_until(function() identical(table_total(app), baseline))
  expect_equal(table_total(app), baseline)
})
