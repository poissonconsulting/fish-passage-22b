source("header.R")

sbf_load_datas_from_db()

water_temp <- drop_units(water_temp)
water_temp_site <- drop_units(water_temp_site)
discharge <- drop_units(discharge)
air_temp <- drop_units(air_temp)
solar_rad <- drop_units(solar_rad)
dewpoint_temp <- drop_units(dewpoint_temp)
surface_pressure <- drop_units(surface_pressure)

sbf_set_sub("query")
sbf_save_datas()
