# Entrypoint for the rapid_app test suite.
#
# Run from the repo root with either:
#   Rscript -e 'shinytest2::test_app("rapid_app")'          # whole suite
#   Rscript -e 'testthat::test_dir("rapid_app/tests/testthat")'
#
# The unit tests (test-calc-vulnerability-index.R) need only
# rapid_app/calc_vulnerability_index.R + dplyr and run in a few seconds.
# The browser tests (test-app-smoke.R) launch the real app in headless
# Chrome via shinytest2 and are skipped automatically where Chrome is absent.

library(shinytest2)

test_app()
