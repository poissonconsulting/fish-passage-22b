source("header.R")

sbf_set_sub("download", "water-temp")

# Water temp ----
# Data downloaded programatically below
temp <- tempfile()
download.file("https://zenodo.org/record/6426024/files/Data.zip?download=1", temp)
file_names <- unzip(temp, list = TRUE)$Name
unzip(temp)

water_temp <- map(
  .x = file_names,
  .f = \(x) read_csv(x, id = "file")
)

water_temp <- list_rbind(water_temp)

unlink(temp)
rm(temp)

sbf_save_data(water_temp)

# Water temp meta data
# Downloaded programatically below
download.file(
  "https://zenodo.org/record/6426024/files/NHG%20Data%20ReadMe.txt?download=1",
  "output/data/download/water-temp/water_temp_meta_data.txt"
)

water_temp_meta_data <- 
  read_tsv(
    "output/data/download/water-temp/water_temp_meta_data.txt",
    skip = 3
  )

sbf_save_data(water_temp_meta_data)
