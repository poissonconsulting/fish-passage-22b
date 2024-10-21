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
download <- FALSE  # This uses previously downloaded versions of data in Dropbox
# download <- TRUE   # This programatically downloads data
if (download) {
  # Downloads stream temperature data
  source("download-water-temp.R")
  # Downloads discharge data from PCIC to dropbox Data folder
  source("download-discharge-pcic.R")
  # Downloads climate data from ERA5 dataset (requires API key)
  # follow directions from https://github.com/joaohenry23/Download_ERA5_with_python
  source("download-air-temp.R")
  source("download-solar-rad.R")
  source("download-dewpoint-temp.R")
  source("download-surface-pressure.R")
}

source("read-water-temp.R")
source("clean-water-temp.R")
source("tidy-water-temp.R")

source("read-air-temp.R")
source("clean-air-temp.R")
source("tidy-air-temp.R")

source("read-discharge.R")
source("clean-discharge.R")
source("tidy-discharge.R")

source("read-solar-rad.R")
source("clean-solar-rad.R")
source("tidy-solar-rad.R")

source("read-dewpoint-temp.R")
source("clean-dewpoint-temp.R")
source("tidy-dewpoint-temp.R")

source("read-surface-pressure.R")
source("clean-surface-pressure.R")
source("tidy-surface-pressure.R")

source("create-database.R")
source("query-database.R")

source("plot-water-temp.R")
source("plot-air-temp.R")
source("plot-solar-rad.R")
source("plot-discharge.R")

source("SSN-distances-temp.R")
source("prepare-data.R")

source("plot-relationships.R")
source("plot-relationships-spatial.R")

source("manipulate-temperature.R")
source("analyse-temperature.R")
source("predict-temperature.R")
source("sensitivity-temperature.R")
source("predict-gsdd.R")

source("manipulate-temperature-mechanistic.R")
source("analyse-temperature-mechanistic.R")
source("predict-temperature-mechanistic.R")
source("sensitivity-temperature-mechanistic.R")

source("manipulate-air2stream.R")
source("analyse-air2stream.R")
source("predict-air2stream.R")
source("sensitivity-air2stream.R")

if(FALSE) {
  source("knit-report.R")
  source("session-info.R")
  source("archive-database.R")
  source("compare-discharge.R")
}
