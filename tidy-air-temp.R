source("header.R")

sbf_set_sub("clean", "air-temp")
sbf_load_datas()

sites <- sbf_load_data("sites", sub = "tidy/water-temp")

air_temp <- 
  air_temp %>% 
  group_by(date, longitude, latitude) %>% 
  summarize(mean_temp = mean(temp), .groups = "keep") %>% 
  select(date, longitude, latitude, mean_temp) %>% 
  rename(
    Longitude = longitude,
    Latitude = latitude
  ) %>% 
  ps_longlat_to_sfc() %>% 
  ps_sfcs_to_crs(crs = crs)

### Get long/lat closest to water temp sites
air_temp_geom <- 
  air_temp %>% 
  distinct(geometry) %>% 
  select(geometry)

site_to_temp <- 
  ps_nearest_feature(sites, air_temp_geom, dist_col = "dist") %>% 
  rename(
    geom_site = geometry,
    geom_temp = geometry.y,
  )

air_temp <- 
  air_temp %>% 
  filter(geometry %in% site_to_temp$geom_temp) %>% 
  ps_sfc_to_coords() %>% 
  as_tibble()

site_to_temp <- 
  site_to_temp %>% 
  select(-geom_site) %>% 
  ps_activate_sfc(sfc_name = "geom_temp") %>% 
  ps_sfc_to_coords() %>% 
  as_tibble() %>% 
  select(site, dist, X, Y)

air_temp <- 
  air_temp %>% 
  left_join(
    site_to_temp, 
    by = c("X", "Y"), 
    relationship = "many-to-many"
  ) %>% 
  rename(x = X, y = Y, dist_from_site = dist)

rm(air_temp_geom, site_to_temp, sites)

sbf_set_sub("tidy", "air-temp")
sbf_save_datas()

if(FALSE) {
  sbf_compare_data_archive()
}