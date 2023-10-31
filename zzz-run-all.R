if(FALSE) {
  source("header.R")
  source("install-packages.R")
  source("update-packages.R")
  
  source("archive-main.R")
  source("remove-main.R")
}

source("read-flow-diagrams.R")
source("read-dags.R")

source("read-water-temp.R")
source("clean-water-temp.R")
source("tidy-water-temp.R")

source("read-air-temp.R")
source("clean-air-temp.R")
source("tidy-air-temp.R")

source("read-discharge.R")
source("clean-discharge.R")
source("tidy-discharge.R")

source("create-database.R")
source("query-database.R")

source("plot-water-temp.R")
# source("plot-air-temp.R")
source("plot-discharge.R")

source("download-distances.R")
source("calculate-shortwave-radiation.R")

source("manipulate-temperature.R")
source("analyse-temperature.R")
source("predict-temperature.R")
# source("residuals-temperature.R")
source("sensitivity-temperature.R")

if(FALSE) {
  source("knit-report.R")
  source("session-info.R")
  source("archive-database.R")
}