source("header.R")

sbf_set_sub("read")

# Water temp ----
# Data downloaded from https://zenodo.org/record/6426024#.ZEAqr-zMI0Q on 04-19-2023
dir <- file.path("~/Poisson/Data/fish-passage/2022/Data/Nechako water temp")

water_temp <-
  list.files(
    path = file.path(dir, "Data"),
    full.names = TRUE,
    pattern = "*.csv"
  ) %>% 
  map(.x = ., \(x) read_csv(x, id = "file")) %>%
  list_rbind()

water_temp_meta <- 
  read_xlsx(file.path(dir, "water_temp_meta_data.xlsx"), col_names = "meta")
  
# Air temp ----
# This is air temp for July 2019 downloaded on 04/25/23 for the area 
# longitude: -127.53 to -122.92
# latitude: 23.32 to 54.89 
# from: https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land?tab=form
dir <- file.path("~/Poisson/Data/fish-passage/2022/Data/Air temp/data.nc")

ncin <- nc_open(dir)
origin <- ncin$dim$time$units # UTC
nc_close(ncin)

rm(ncin)

air_temp <- tidync(dir) %>%  # Can filter this before making it a tibble using 
  # `hyper_filter()` https://rdrr.io/cran/tidync/f/vignettes/netcdf-with-tidync.Rmd
  hyper_tibble()

rm(dir)

sbf_save_datas()
sbf_save_objects()

if(FALSE) {
  sbf_compare_data_archive()
}
