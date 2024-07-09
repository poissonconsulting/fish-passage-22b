if(FALSE) {
  source("header.R")
  source("install-packages.R")
  source("update-packages.R")
  
  source("archive-main.R")
  source("remove-main.R")
}

source("read-flow-diagrams.R")
source("read-dags.R")

# Download data? 
download <- FALSE  # This uses previously downloaded versions in Dropbox
# download <- TRUE   # This programatically downloads data
if (download) {
  # Downloads stream temperature data
  source("download-water-temp.R")
  # Downloads discharge data from PCIC to dropbox Data folder
  source("download-discharge-pcic.R")
  # Downloads air temperature from ERA5 dataset (requires API key)
  # follow directions from https://github.com/joaohenry23/Download_ERA5_with_python
  source("download-air-temp.R") 
}

# Make sure the download T/F above will trigger the following scripts to figure themselves out automatically.
source("read-water-temp.R")
source("clean-water-temp.R")
source("tidy-water-temp.R")

source("read-air-temp.R")
source("clean-air-temp.R")
source("tidy-air-temp.R")

source("read-discharge2.R") # Change names to just discharge
source("clean-discharge2.R")
source("tidy-discharge2.R")

# if (FALSE) { # don't really need these - just for air temp!
#   source("read-weather.R")
#   source("clean-weather.R")
#   source("tidy-weather.R")
# }

source("create-database.R")
source("query-database.R")

source("plot-water-temp.R")
source("plot-air-temp.R")
source("plot-discharge.R")

source("SSN-distances-temp.R")

source("prepare-data.R")

source("manipulate-temperature.R")
# source("plot-relationships.R") # save plots with report = FALSE/get rid of old vars

# Temperature models ----

## Delete old models; remove other covariance structures.
## Delete other old files!!
## Make sure repo runs through in full over the weekend
## Report!
## Copy air temperature data to Al's dropbox

# source("analyse-temperature-glm.R")
# source("predict-temperature-glm.R")
# 
# source("analyse-temperature-logistic.R") # site-wise phi
# source("predict-temperature-logistic.R") # site-wise phi
# 
# source("analyse-temperature-logistic-fixed-phi.R")
# source("predict-temperature-logistic-fixed-phi.R")
# 
# source("analyse-temperature-logistic-fixed-phi-re-mu-gamma.R")
# source("predict-temperature-logistic-fixed-phi-re-mu-gamma.R")
# 
# source("analyse-temperature-logistic-fixed-phi-re-alpha-beta-gamma.R")
# source("predict-temperature-logistic-fixed-phi-re-alpha-beta-gamma.R")
# 
# source("analyse-temperature-logistic-no-phi.R")
# source("predict-temperature-logistic-no-phi.R")
# 
# source("analyse-temperature-logistic-no-phi-re-gamma.R")
# source("predict-temperature-logistic-no-phi-re-gamma.R")
# 
# source("analyse-temperature-logistic-no-phi-re-alpha-beta.R")
# source("predict-temperature-logistic-no-phi-re-alpha-beta.R")

# source("analyse-temperature-logistic-no-phi-re-alpha-beta-gamma.R")
# source("predict-temperature-logistic-no-phi-re-alpha-beta-gamma.R")

source("analyse-temperature.R") 
source("predict-temperature.R")
source("sensitivity-temperature.R")

source("predict-gsdd.R")

# source("compare-acf.R")

# source("test-temperature-heat.R")
# source("analyse-temperature-heat.R")
# source("predict-temperature-heat.R")
# 
# source("analyse-temperature-rate.R")
# source("predict-temperature-rate.R")

# source("compare-temperature.R")

if(FALSE) {
  source("knit-report.R")
  source("session-info.R")
  source("archive-database.R")
}
