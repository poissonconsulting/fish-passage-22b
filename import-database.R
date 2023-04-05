source("header.R")

sbf_load_datas_from_db("fish-passage-21")

sbf_set_sub("import", rm = TRUE)
sbf_save_datas()
