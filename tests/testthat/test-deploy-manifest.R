# Guards the deployment file list in deploy.R.
#
# The app is published with an explicit list of files (APP_FILES) so that the
# symlinked data/ folder and anything else lying around the repo never reach
# the server. That only works if the list stays in step with what app.R reads
# and with manifest.json. Every failure here is fixed by editing APP_FILES and
# rerunning `Rscript deploy.R manifest`.

repo_root <- testthat::test_path("..", "..")
source(file.path(repo_root, "deploy.R"), local = TRUE)

app_src <- paste(
  readLines(file.path(repo_root, "app.R"), warn = FALSE),
  readLines(file.path(repo_root, "calc_vulnerability_index.R"), warn = FALSE),
  collapse = "\n"
)
# Drop comment lines so commented-out reads and library() calls don't count.
app_code <- gsub("(^|\n)\\s*#[^\n]*", "\\1", app_src)


test_that("every file in APP_FILES exists", {
  missing <- APP_FILES[!file.exists(file.path(repo_root, APP_FILES))]
  expect_equal(missing, character(0))
})


test_that("every app_data/ file app.R reads is deployed", {
  read_paths <- unique(regmatches(
    app_code, gregexpr("app_data/[A-Za-z0-9_.-]+", app_code)
  )[[1]])
  expect_gt(length(read_paths), 0)
  expect_equal(setdiff(read_paths, APP_FILES), character(0))
})


test_that("manifest.json lists exactly the deployed files", {
  manifest <- jsonlite::read_json(file.path(repo_root, "manifest.json"))
  expect_equal(sort(names(manifest$files)), app_file_list(repo_root))
})


test_that("manifest.json records every package app.R loads", {
  manifest <- jsonlite::read_json(file.path(repo_root, "manifest.json"))
  attached <- regmatches(app_code, gregexpr("library\\(([A-Za-z0-9.]+)\\)", app_code))[[1]]
  attached <- gsub("library\\(|\\)", "", attached)
  # `pkg::fn` only -- the lookahead skips CSS such as `summary::-webkit-...`
  namespaced <- regmatches(app_code, gregexpr("\\b[A-Za-z][A-Za-z0-9.]*(?=::[A-Za-z.])", app_code, perl = TRUE))[[1]]
  base_pkgs <- rownames(installed.packages(priority = "base"))
  needed <- setdiff(unique(c(attached, namespaced)), base_pkgs)
  expect_equal(setdiff(needed, names(manifest$packages)), character(0))
})


# The reverse of the app_data/ check above: a file in the app's folders that
# no app code mentions is probably dead weight, whether or not it is deployed.
# Entry points are run by Shiny itself and deploy.R is dev tooling, so nothing
# references them. This only warns, since a file can be used in ways a
# filename search won't see.
NOT_REFERENCED <- c("app.R", "deploy.R")

test_that("every file in the app's folders is referenced by app code", {
  candidates <- c(
    list.files(repo_root, pattern = "\\.R$"),
    file.path("app_data", list.files(file.path(repo_root, "app_data"), recursive = TRUE)),
    file.path("www", list.files(file.path(repo_root, "www"), recursive = TRUE))
  )
  r_files <- setdiff(list.files(repo_root, pattern = "\\.R$"), "deploy.R")
  code <- vapply(r_files, function(f) {
    src <- paste(readLines(file.path(repo_root, f), warn = FALSE), collapse = "\n")
    gsub("(^|\n)\\s*#[^\n]*", "\\1", src)
  }, character(1))

  unused <- Filter(function(f) {
    others <- code[names(code) != f]
    !any(grepl(basename(f), others, fixed = TRUE))
  }, setdiff(candidates, NOT_REFERENCED))

  if (length(unused) > 0) {
    warning("Never referenced by app code: ", paste(unused, collapse = ", "),
            call. = FALSE)
  }
  succeed()
})
