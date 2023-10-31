source("header.R")

sbf_set_sub("clean", "discharge")
sbf_load_datas()

water_temp_site <- sbf_load_data("water_temp_site", sub = "tidy/water-temp")

discharge %<>% 
  left_join(discharge_meta, join_by(id)) %>%
  mutate(
    param = if_else(param == 1, "discharge", "level")
  ) %>% 
  rename(flag = sym) %>% 
  pivot_wider(
    names_from = c("param"),
    values_from = c("value", "flag")
  ) %>% 
  rename(
    discharge = value_discharge,
    level = value_level
  ) %>% 
  filter(date >= bounding_dates[1] & date <= bounding_dates[2]) %>% 
  ps_longlat_to_sfc() %>% 
  st_join(
    water_temp_site %>% select(site, geometry),
    join = st_is_within_distance, 
    dist = 1000
  ) %>% 
  as_tibble() %>% 
  select(
    station_id = id, site, date, discharge, level, flag_discharge, flag_level
  )

rm(discharge_meta, water_temp_site)

sbf_set_sub("tidy", "discharge")
sbf_save_datas()

if(FALSE) {
  sbf_compare_data_archive()
}