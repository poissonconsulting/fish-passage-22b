source("header.R")

sbf_set_sub("clean", "discharge")
sbf_load_datas()

discharge %<>% 
  select(site, date, discharge)

sbf_set_sub("tidy", "discharge", rm = TRUE)
sbf_save_datas()

if (FALSE) {
  sbf_compare_data_archive()
}
