source("header.R")

sbf_set_sub("read", sub = "data")
sbf_load_datas()

sbf_set_sub("clean", sub = "data", rm = TRUE)
sbf_save_datas()

if(FALSE) {
  sbf_compare_data_archive()
}
