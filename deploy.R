#' ──────────────────────────────────────────────────────────────────────────────
#' deploy.R
#'
#' The one list of files that make up the deployed app, and the two things done
#' with it:
#'
#'   Rscript deploy.R manifest   # regenerate manifest.json from APP_FILES
#'   Rscript deploy.R deploy     # publish APP_FILES to shinyapps.io
#'
#' The app is hosted on shinyapps.io as `rapid_app` under the `geocentroid`
#' account, and served publicly through CSU's proxy at
#' https://apps.gis.colostate.edu/WSVA_tool/. Deploying under the same account
#' and app name updates that app in place.
#'
#' Deploying with an explicit file list means nothing else in the repo is ever
#' bundled. The RStudio "Publish" button would instead bundle the whole folder
#' minus .rscignore.
#'
#' Deploying from this script also means every change to the app can be
#' reviewed before it goes live. Any developer with the account credentials can
#' deploy, and the changes can be recorded in git. For example, a package
#' update shows up in manifest.json in a pull request and is tested there,
#' before anyone deploys it.
#'
#' Known gap: deployApp() does not read the committed manifest.json. It builds
#' its own from the packages installed on the deploying machine, so what ships
#' is that machine's library, not necessarily what was reviewed. Making the
#' reviewed manifest binding is the goal; the real fix is pinning packages with
#' renv, which is out of scope for now.
#'
#' When app.R starts reading a new file, add it to APP_FILES and rerun
#' `Rscript deploy.R manifest`. tests/testthat/test-deploy-manifest.R fails
#' until both are done.
#' ──────────────────────────────────────────────────────────────────────────────

APP_FILES <- c(
  "app.R",
  "calc_vulnerability_index.R",
  "app_data/final_indicators.csv",
  "app_data/indicator_details.csv",
  "app_data/municipal_hauled.gpkg",
  "app_data/parks_simplified.gpkg",
  "app_data/water_supplies.csv",
  "app_data/wbm_data.gpkg",
  "www"
)

SHINYAPPS_ACCOUNT <- "geocentroid"
SHINYAPPS_APP     <- "rapid_app"

#' APP_FILES with directories expanded to the files inside them, as rsconnect
#' records them in manifest.json.
app_file_list <- function(app_dir = ".") {
  out <- unlist(lapply(APP_FILES, function(f) {
    path <- file.path(app_dir, f)
    if (dir.exists(path)) {
      file.path(f, list.files(path, recursive = TRUE, all.files = FALSE))
    } else {
      f
    }
  }))
  sort(out)
}

write_app_manifest <- function(app_dir = ".") {
  rsconnect::writeManifest(appDir = app_dir, appFiles = APP_FILES)
}

#' Publish to shinyapps.io, updating the existing app.
#'
#' Needs the geocentroid account token on this machine, set once with
#' rsconnect::setAccountInfo() (shinyapps.io dashboard -> Account -> Tokens).
deploy_app <- function(app_dir = ".") {
  rsconnect::deployApp(
    appDir      = app_dir,
    appFiles    = APP_FILES,
    appName     = SHINYAPPS_APP,
    account     = SHINYAPPS_ACCOUNT,
    server      = "shinyapps.io",
    forceUpdate = TRUE
  )
}

if (sys.nframe() == 0L) {
  cmd <- commandArgs(trailingOnly = TRUE)
  if (identical(cmd, "manifest")) {
    write_app_manifest()
  } else if (identical(cmd, "deploy")) {
    deploy_app()
  } else {
    stop("Usage: Rscript deploy.R manifest | deploy", call. = FALSE)
  }
}
