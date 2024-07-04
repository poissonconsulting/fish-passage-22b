if(FALSE) {
  source("header.R")
  source("install-packages.R")
  source("update-packages.R")
  
  source("archive-main.R")
  source("remove-main.R")
}

source("read-flow-diagrams.R")
source("read-dags.R")

if (FALSE) {
  # Downloads discharge data from PCIC to dropbox Data folder
  source("download-discharge-pcic.R")
  # Downloads air temperature from ERA5 dataset (requires API key)
  # Not sure where will save it yet.
  source("download-air-temp.R") # not sure if this works yet
}

source("read-water-temp.R")
source("clean-water-temp.R")
source("tidy-water-temp.R")

source("read-discharge2.R")
source("clean-discharge2.R")
source("tidy-discharge2.R")

source("read-weather.R")
source("clean-weather.R")
source("tidy-weather.R")

source("create-database.R")
source("query-database.R")

source("plot-water-temp.R")
source("plot-discharge.R")

source("SSN-distances.R")

source("prepare-data.R")

source("manipulate-temperature.R")
source("plot-relationships.R")

# Temperature models ----
source("analyse-temperature-glm.R")
source("predict-temperature-glm.R")

source("analyse-temperature-logistic.R") # site-wise phi
source("predict-temperature-logistic.R") # site-wise phi

source("analyse-temperature-logistic-fixed-phi.R")
source("predict-temperature-logistic-fixed-phi.R")

source("analyse-temperature-logistic-fixed-phi-re-mu-gamma.R")
source("predict-temperature-logistic-fixed-phi-re-mu-gamma.R")

source("analyse-temperature-logistic-fixed-phi-re-alpha-beta-gamma.R")
source("predict-temperature-logistic-fixed-phi-re-alpha-beta-gamma.R")

source("analyse-temperature-logistic-no-phi.R")
source("predict-temperature-logistic-no-phi.R")

source("analyse-temperature-logistic-no-phi-re-gamma.R")
source("predict-temperature-logistic-no-phi-re-gamma.R")

source("analyse-temperature-logistic-no-phi-re-alpha-beta.R")
source("predict-temperature-logistic-no-phi-re-alpha-beta.R")

source("analyse-temperature-logistic-no-phi-re-alpha-beta-gamma.R")
source("predict-temperature-logistic-no-phi-re-alpha-beta-gamma.R")

source("compare-acf.R")

source("test-temperature-heat.R")
source("analyse-temperature-heat.R")
source("predict-temperature-heat.R")

source("analyse-temperature-rate.R")
source("predict-temperature-rate.R")

source("compare-temperature.R")

source("predict-gsdd.R")

source("sensitivity-temperature.R")

if(FALSE) {
  source("knit-report.R")
  source("session-info.R")
  source("archive-database.R")
}
