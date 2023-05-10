source("header.R")

sbf_set_sub("clean")
sbf_load_datas()

# Water temp ----


# Air temp ----


sbf_set_sub("tidy", rm = TRUE)
sbf_save_datas()

if(FALSE) {
  sbf_compare_data_archive()
}
