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
    station_id, river, station, Latitude = latitude, Longitude = longitude, 
    altitude
  ) %>% 
  distinct() %>% 
  ps_longlat_to_sfc()

discharge_site_upstream <-
  discharge_site %>% 
  ps_sfc_to_longlat() %>% 
  rowwise() %>% 
  mutate(
    geometry = hydroshed(Longitude, Latitude)$geometry
  ) %>% 
  ungroup() %>% 
  ps_activate_sfc() %>% 
  mutate(
    upstream_area = st_area(geometry)
  )

discharge %<>% 
  group_by(station_id) %>% 
  mutate(missing = if_else2(any(is.na(value)), TRUE, FALSE)) %>% 
  ungroup() %>% 
  filter(!missing) %>% 
  select(station_id, date, discharge = value) %>% 
  mutate(
    discharge = set_units(discharge, "m^3/s")
  )

sbf_set_sub("tidy", "discharge")
sbf_save_datas()

if(FALSE) {
  sbf_compare_data_archive()
}