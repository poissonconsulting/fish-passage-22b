source("header.R")

sbf_set_sub("read", "water-temp")

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

sbf_save_data(water_temp)

# Water temp meta data
# Downloaded programatically below
# Backup data downloaded from https://zenodo.org/record/6426024#.ZEAqr-zMI0Q on 04-19-2023
# dir <- file.path("~/Poisson/Data/fish-passage/2022/Data/Nechako water temp/NHG Data ReadMe.txt")
# dir <- file.path("~/Poisson/Clients - Transfer/New Graph/xxxxxxxx")
# water_temp_meta_data <- read_tsv(dir, skip = 3)

download.file(
  "https://zenodo.org/record/6426024/files/NHG%20Data%20ReadMe.txt?download=1",
  "output/data/read/water-temp/water_temp_meta_data.txt"
)

water_temp_meta_data <- 
  read_tsv(
    "output/data/read/water-temp/water_temp_meta_data.txt", 
    skip = 3
  )

sbf_save_data(water_temp_meta_data)

if(FALSE) {
  sbf_compare_data_archive()
}