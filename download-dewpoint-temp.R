source("header.R")

message("Need API for Climate Data Store - follow directions from https://github.com/joaohenry23/Download_ERA5_with_python")
message("This could take a while to download")

sbf_set_sub("dewpoint-temp")
x <- tibble(x = 0)
sbf_save_data(x)
sbf_reset_sub()

reticulate::use_virtualenv("r-reticulate")
cdsapi <- reticulate::import("cdsapi")
reticulate::source_python("download-dewpoint-temp.py")
