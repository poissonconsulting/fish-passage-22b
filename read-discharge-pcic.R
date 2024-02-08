library(ncdf4)
library(tidyverse)
library(jsonlite)
library(stringr)
library(lubridate)


# Read the JSON data into R
json_data <- fromJSON("https://data.pacificclimate.org/portal/hydro_model_out/catalog/catalog.json") 

dat  <- tibble(
    name = names(json_data),
    url = unlist(json_data)
  )

# have a look at baseflow and runoff only
dat_br <- dat %>% 
  dplyr::filter(str_detect(name, "BASEFLOW") | str_detect(name, "RUNOFF"))

# make a bash file that can be run with `bash pcic_dl.sh` on commandline.
pcic_dl_sh()
pcic_dl_sh(var = "RUNOFF", 
           append = TRUE)

## Add baseflow and runoff to create discharge.  Not sure if we can/should do that first as it was
# done after getting averages in bcfishpass.  
