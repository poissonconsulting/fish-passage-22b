source("header.R")

sbf_set_sub("read", "weather")

# This is weather data for 
# July 2019 - October 2021 downloaded on 10/31/23 
# for the area 
# longitude: -127.53 to -122.92
# latitude: 23.32 to 54.89 
# from: https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land?tab=form

# Including the following variables:
# - 2m temperature
# - Snowmelt
# - Volumetric soil water layer 1
# - Surface net solar radiation
# - Evaporation from open water surfaces excluding oceans
# - Total evaporation
# - Surface runoff
# - Sub-surface runoff
# - Total precipitation

dir <- file.path(dir, "Data/ERA5")
files <- list.files(dir, full.names = TRUE) %>% 
  as_list()

weather <-
  map(
    .x = files,
    .f = \(x) {
      y <- tidync(x) %>%
        hyper_tibble()
      
      ncin <- nc_open(x)
      x_origin <- ncin$dim$time$units # UTC
      nc_close(ncin)
      x_origin <- str_extract(x_origin, "(?<=hours since ).*")
      
      y <- y %>% 
        mutate(origin = x_origin)
      y
    }
  ) %>% 
  list_rbind()

rm(dir, files)

sbf_save_datas()

if (FALSE) {
  sbf_compare_data_archive()
}