source("header.R")

poisreport::knit_report("fish-passage-22b.Rmd", ask = FALSE)
poisreport::report_to_directory()

dir <- "~/Poisson/Clients - Transfer/New Graph"
poisreport::report_to_directory(dir = dir)

if (require(poisblogdown)) poisblogdown::report_to_blogdown()
