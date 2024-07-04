source("header.R")

sbf_set_sub("temperature")
analysis <- sbf_load_object("analysis")

install.packages("shinystan")
library(shinystan)
fit <- analysis$stanfit
launch_shinystan(fit)