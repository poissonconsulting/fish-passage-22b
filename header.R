library(poispkgs)
library(reticulate)
library(ncdf4)
library(tidync)
library(jsonlite)
library(zoo)
library(gsdd)
library(cowplot)

stopifnot(packageVersion("poispkgs") >= "0.0.1.9017")

dtt_set_default_tz("Etc/GMT+8")

theme_set(theme_Poisson())

options(sbf.ask = FALSE)

sbf_set_main("output")
sbf_reset_sub()

if (getDoParWorkers() == 1) {
  message("registering 4 workers")
  registerDoParallel(4)
}

set_analysis_mode("report")

rm(list = ls())
graphics.off()

sbf_set_db_name("fish-passage-22b")

source("functions.R")

if (dir.exists("~/Poisson")) {
  # Poisson directory
  dir <- "~/Poisson/Clients - Transfer/New Graph/fish-passage-22b"
} else {
  # New Graph directory
  dir <- "~/Dropbox/New Graph/fish-passage-22b"
}

crs <- 3005 # BC Albers
options("fwa.epsg" = 3005)

bounding_dates <- dtt_date(c("2019-01-01", "2021-12-31"))

tz_data <- "UTC"
tz_analysis <- "Etc/GMT+8"
