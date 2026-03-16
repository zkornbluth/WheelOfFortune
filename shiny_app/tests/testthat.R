# testthat runner for Wheel of Fortune Shiny app
# Run from shiny_app: source("tests/testthat.R")
# Or from repo root: setwd("shiny_app"); source("tests/testthat.R")

library(testthat)
local_edition(3)

# Find app directory so app.R can find wheeloffortune.csv
if (file.exists("app.R")) {
  app_dir <- getwd()
} else {
  app_dir <- "shiny_app"
  if (!file.exists(file.path(app_dir, "app.R"))) {
    stop("Cannot find app.R. Run tests from repo root or from shiny_app.")
  }
}

orig_wd <- getwd()
setwd(app_dir)
on.exit(setwd(orig_wd), add = TRUE)
source("app.R", local = .GlobalEnv)

test_dir("tests/testthat")
