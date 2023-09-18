source("header.R")

sbf_set_sub("read")

# Water temp ----
# Data downloaded programatically f
# Backup data downloaded from https://zenodo.org/record/6426024#.ZEAqr-zMI0Q on 04-19-2023
dir <- "~/Poisson/Data/fish-passage/2022/Data/Nechako water temp"

# Water temp ----
temp <- tempfile()
download.file("https://zenodo.org/record/6426024/files/Data.zip?download=1", temp)
file_names <- unzip(temp, list = TRUE)$Name
unzip(temp)

water_temp <- list()
for (i in 1:length(file_names)) {
  water_temp[[i]] <- read_csv(file_names[i], id = "file")
}

water_temp <- list_rbind(water_temp)

unlink(temp)
rm(temp)

# metadata
download.file(
  "https://zenodo.org/record/6426024/files/NHG%20Data%20ReadMe.txt?download=1",
  "output/data/read/water_temp_meta_data.txt"
)

water_temp_meta_data <- read_tsv("output/data/read/water_temp_meta_data.txt", skip = 3)
  
# Air temp ----
# This is air temp for July 2019 downloaded on 04/25/23 for the area 
# longitude: -127.53 to -122.92
# latitude: 23.32 to 54.89 
# from: https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land?tab=form
### Not going to be able to programatically download this data. 
dir <- file.path("~/Poisson/Data/fish-passage/2022/Data/Air temp/data.nc")
# dir <- file.path("~/Poisson/Clients - Transfer/New Graph/xxxxxxxx")

# ncin <- nc_open(dir)
# origin <- ncin$dim$time$units # UTC
# nc_close(ncin)
# 
# rm(ncin)

air_temp <- tidync(dir) %>%  # Can filter this before making it a tibble using 
  # `hyper_filter()` https://rdrr.io/cran/tidync/f/vignettes/netcdf-with-tidync.Rmd
  hyper_tibble()

rm(dir)

sbf_save_datas()
sbf_save_objects()

if(FALSE) {
  sbf_compare_data_archive()
}
