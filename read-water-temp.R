source("header.R")

sbf_set_sub("download", "water-temp")

if (file.exists(file.path(getwd(), "download/water-temp"))) {
  sbf_load_datas()
} else {
  dir_data <- file.path(dir, "Data/Nechako water temp/Data")
  dir_meta <- file.path(dir, "Data/Nechako water temp/NHG Data ReadMe.txt")
  
  file_names <- list.files(dir_data, ".csv", full.names = TRUE)
  water_temp <- map(
    .x = file_names,
    .f = \(x) read_csv(x, id = "file")
  )
  water_temp <- list_rbind(water_temp)
  
  water_temp_meta_data <- read_tsv(dir_meta, skip = 3)
}


sbf_set_sub("read", "water-temp")
sbf_save_datas()

if(FALSE) {
  sbf_compare_data_archive()
}