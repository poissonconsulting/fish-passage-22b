library(ncdf4)
library(tidyverse)
library(jsonlite)
library(stringr)
library(lubridate)


# Read the JSON data into R
# json_data <- jsonlite::fromJSON("https://data.pacificclimate.org/portal/hydro_model_out/catalog/catalog.json") 
# 
# dat  <- tibble::tibble(
#     name = names(json_data),
#     url = unlist(json_data)
#   )
# 
# # have a look at baseflow and runoff only
# dat_br <- dat %>% 
#   dplyr::filter(str_detect(name, "BASEFLOW") | str_detect(name, "RUNOFF"))
# 
# # select the url of specific model and variable
# url <- dat %>% 
#   dplyr::filter(str_detect(name, model) & str_detect(name, var)) %>% 
#   pull(url)
# 
# # Open the netCDF file
# nc_data <- nc_open(url)
# 
# # Print an overview of the file
# # print(nc_data)
# 
# # Read variables
# time <- ncvar_get(nc_data, "time")
# lat <- ncvar_get(nc_data, "lat")
# lon <- ncvar_get(nc_data, "lon")

# Inspect the variables
# print(time)
# print(lat)
# print(lon)


# make a bash file that can be run with `bash pcic_dl.sh` on commandline.
pcic_dl_sh()
pcic_dl_sh(var = "RUNOFF", 
           append = TRUE)

# run the file to generate the output
system2("bash", args = "pcic_dl.sh")

## Add baseflow and runoff to create discharge.  Not sure if we can/should do that first as it was
# done after getting averages in bcfishpass.  
