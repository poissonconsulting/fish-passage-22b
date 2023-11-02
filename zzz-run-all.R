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

source("read-discharge.R")
source("clean-discharge.R")
source("tidy-discharge.R")

source("read-weather.R")
source("clean-weather.R")
source("tidy-weather.R")

source("create-database.R")
source("query-database.R")

source("plot-water-temp.R")
source("plot-discharge.R")
source("plot-weather.R")

source("SSN-distances.R")

source("prepare-data.R")

source("manipulate-discharge.R")
source("analyse-discharge.R")
source("predict-discharge.R")
# source("residuals-discharge.R")
source("sensitivity-discharge.R")

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