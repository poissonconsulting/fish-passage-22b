if(FALSE) {
  source("header.R")
  source("install-packages.R")
  source("update-packages.R")
  
  source("archive-main.R")
  source("remove-main.R")
}

source("read-flow-diagrams.R")
source("read-dags.R")

source("read-data.R")
source("clean-data.R")
source("tidy-data.R")

# .ssn object for SSN and SSNbayes 
source("read-elev.R")
source("get-ssn.R") # takes > 1 day 
# source("ssnbayes.R")

source("plot-water-temp.R")

source("import-database.R")
source("create-database.R")
source("populate-database.R")

source("session-info.R")

if(FALSE) {
  source("knit-report.R")
  source("session-info.R")
  source("archive-database.R")
}