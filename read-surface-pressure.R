source("header.R")

# This is hourly surface net solar radiation data for 
# January 2019 - December 2021
# for the area 
# longitude: -127.53 to -122.92
# latitude: 53.32 to 54.89 
# from: https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land?tab=form
# using an API 
dir2 <- file.path(getwd(), "output/data/surface-pressure")
if (!file.exists(file.path(dir2, "2019.nc"))) {
  # Change to directory to dropbox folder if API doesn't work
  dir2 <- file.path(dir, "Data/ERA5/surface-pressure")
}

files <- list.files(dir2, full.names = TRUE) %>% 
  str_subset(., pattern = "x.rds$", negate = TRUE) %>% 
  as_list()

surface_pressure <-
  map(
    .x = files,
    .f = \(x) {
      y <- tidync(x) %>%
        hyper_tibble()
    }
  ) %>% 
  list_rbind()

rm(dir, files)

sbf_set_sub("read", "surface-pressure", rm = TRUE)
sbf_save_datas()

if (FALSE) {
  sbf_compare_data_archive()
}
