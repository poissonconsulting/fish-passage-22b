source("header.R")

sbf_load_datas_from_db()

water_temp <- drop_units(water_temp)
discharge <- drop_units(discharge)
discharge_site <- drop_units(discharge_site)
weather_temp <- drop_units(weather_temp)
weather_discharge <- drop_units(weather_discharge)
us_weather_discharge_obs <- drop_units(us_weather_discharge_obs)
us_weather_discharge_pred <- drop_units(us_weather_discharge_pred)

sbf_set_sub("query")
sbf_save_datas()