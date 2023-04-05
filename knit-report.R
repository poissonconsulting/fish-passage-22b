source("header.R")

poisreport::knit_report("fish-passage-22.Rmd", ask = FALSE)

poisreport::report_to_directory()

if(FALSE) {
  if (require(poisblogdown)) poisblogdown::report_to_blogdown()
}
