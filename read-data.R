source("header.R")

sbf_set_sub("read")

# Water temp ----
# Data downloaded programatically below
# Backup data downloaded from https://zenodo.org/record/6426024#.ZEAqr-zMI0Q on 04-19-2023
# dir <- file.path("~/Poisson/Data/fish-passage/2022/Data/Nechako water temp/Data")
# dir <- file.path("~/Poisson/Clients - Transfer/New Graph/xxxxxxxx")
# file_names <- list.files(dir, ".csv", full.names = TRUE)

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

# Water temp meta data
# Downloaded programatically below
# Backup data downloaded from https://zenodo.org/record/6426024#.ZEAqr-zMI0Q on 04-19-2023
# dir <- file.path("~/Poisson/Data/fish-passage/2022/Data/Nechako water temp/NHG Data ReadMe.txt")
# dir <- file.path("~/Poisson/Clients - Transfer/New Graph/xxxxxxxx")
# water_temp_meta_data <- read_tsv(dir, skip = 3)

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


air_temp <- tidync(dir) %>%  # Can filter this before making it a tibble using 
  # `hyper_filter()` https://rdrr.io/cran/tidync/f/vignettes/netcdf-with-tidync.Rmd
  hyper_tibble()

rm(dir)

# Discharge
paths <- list.files(
  "~/Poisson/Data/fish-passage/2022/Data/Discharge", 
  full.names = TRUE
)

discharge <- bind_rows(
  read_csv(paths[1], skip = 1),
  read_csv(paths[2], skip = 1),
  read_csv(paths[3], skip = 1)
)

discharge_meta <- read_csv(paths[4])

discharge_flag <- 
  tribble(
    ~flag,  ~flag_description,
      "A",      "Partial Day",
      "D",              "Dry",
      "R",          "Revised",
      "B",   "Ice Conditions",
      "E",        "Estimated"
  )

rm(paths)

sbf_save_datas()
sbf_save_objects()

if(FALSE) {
  sbf_compare_data_archive()
}
