source("header.R")

sbf_set_sub("clean", "discharge")
sbf_load_datas()

# Filter to sites in the nechako watershed
discharge %<>%
  filter(longitude < -122.92 & longitude > -127.53 & 
           latitude > 53.32 & latitude < 54.89)

discharge_site <- 
  discharge %>% 
  mutate(
    altitude = set_units(altitude_m_asl, "m")
  ) %>% 
  select(
    station_id, river, station, latitude, longitude, altitude
  ) %>% 
  distinct()

discharge %<>% 
  group_by(station_id) %>% 
  mutate(missing = if_else2(any(is.na(value)), TRUE, FALSE)) %>% 
  ungroup() %>% 
  filter(!missing) %>% 
  select(station_id, date, mean_discharge = value) %>% 
  mutate(
    mean_discharge = set_units(mean_discharge, "m^3/s")
  )


sbf_set_sub("tidy", "discharge")
sbf_save_datas()

if(FALSE) {
  sbf_compare_data_archive()
}