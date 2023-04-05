library(poispkgs)

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

set_analysis_mode("quick")

rm(list = ls())
graphics.off()

source("functions.R")

dir <- "~/Poisson/Data/fish-passage/2022"
