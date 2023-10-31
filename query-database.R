source("header.R")

sbf_load_datas_from_db()

water_temp <- drop_units(water_temp)
discharge <- drop_units(discharge)
discharge_site <- drop_units(discharge_site)

sbf_set_sub("query")
sbf_save_datas()