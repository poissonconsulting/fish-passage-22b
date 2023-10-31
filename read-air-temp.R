source("header.R")

sbf_set_sub("read", "air-temp")

# Air temp ----
# This is air temp for July 2019 downloaded on 04/25/23 for the area 
# longitude: -127.53 to -122.92
# latitude: 23.32 to 54.89 
# from: https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land?tab=form
### Not going to be able to programatically download this data. 
dir <- file.path("~/Poisson/Data/fish-passage/2022/Data/Air temp/data.nc")
# dir <- file.path("~/Poisson/Clients - Transfer/New Graph/xxxxxxxx")

air_temp <- tidync(dir) %>%  # Can filter this before making it a tibble using 
  # `hyper_filter()` https://rdrr.io/cran/tidync/f/vignettes/netcdf-with-tidync.Rmd
  hyper_tibble()

ncin <- nc_open(dir)
air_temp_origin <- ncin$dim$time$units # UTC
nc_close(ncin)

rm(dir, ncin)

sbf_save_datas()
sbf_save_object(air_temp_origin)

if(FALSE) {
  sbf_compare_data_archive()
}
