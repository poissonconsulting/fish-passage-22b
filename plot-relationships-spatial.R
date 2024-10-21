source("header.R")

sbf_set_sub("prepare")
sbf_load_datas()

water_temp %<>% 
  left_join(water_temp_site, join_by(site, elev)) %>% 
  ps_activate_sfc()

water_temp %<>% 
  group_by(week) %>% 
  mutate(
    across(
      c(air_temp, solar_rad, discharge, water_temp),
      \(x) as.vector(scale(x))
    )
  ) %>% 
  ungroup()

mapview(water_temp, zcol = "discharge")
mapview(water_temp, zcol = "solar_rad")
mapview(water_temp, zcol = "air_temp")
mapview(water_temp, zcol = "water_temp")
