source("header.R")

sbf_set_sub("clean", "weather")
sbf_load_datas()

water_temp_site <- sbf_load_data("water_temp_site", sub = "tidy/water-temp")
water_temp_site_upstream <- sbf_load_data("water_temp_site_upstream", sub = "tidy/water-temp")
discharge_site <- sbf_load_data("discharge_site", sub = "tidy/discharge")
discharge_site_upstream <- sbf_load_data("discharge_site_upstream", sub = "tidy/discharge")

weather <- 
  weather %>% 
  group_by(longitude, latitude) %>% 
  mutate(
    cell_id = cur_group_id(),
    cell_id = as.character(cell_id)
  ) %>% 
  ungroup()

weather_site <- 
  weather %>% 
  select(cell_id, Longitude = longitude, Latitude = latitude) %>% 
  distinct() %>% 
  arrange(cell_id) %>% 
  ps_longlat_to_sfc()

# Filter to areas closest to water temp sites
closest_site_water_temp <- 
  ps_nearest_feature(water_temp_site, weather_site, dist_col = "dist") %>% 
  as_tibble() %>% 
  select(site, cell_id, dist)

max_dist <- max(closest_site_water_temp$dist)
message("maximum distance between grid and water temp sites: ", max_dist, " m")

weather_temp <-
  weather %>%
  filter(cell_id %in% closest_site_water_temp$cell_id) %>%
  left_join(closest_site_water_temp, by = "cell_id", relationship = "many-to-many") %>%
  select(
    cell_id, site, air_temp, evap_water, evap_total, snowmelt,
    runoff_subsurface, runoff_surface, net_solar_rad, precip, soil_vol_1,
    date, time
  )

rm(max_dist, closest_site_water_temp, water_temp_site)

# Filter to areas closest to discharge sites
closest_site_discharge <-
  ps_nearest_feature(discharge_site, weather_site, dist_col = "dist") %>%
  as_tibble() %>%
  select(station_id, cell_id, dist)

max_dist <- max(closest_site_discharge$dist)
message("maximum distance between grid and discharge sites: ", max_dist, " m")

weather_discharge <-
  weather %>%
  filter(cell_id %in% closest_site_discharge$cell_id) %>%
  left_join(closest_site_discharge, by = "cell_id") %>%
  select(
    cell_id, station_id, air_temp, evap_water, evap_total, snowmelt,
    runoff_subsurface, runoff_surface, net_solar_rad, precip, soil_vol_1,
    date, time
  )

# Calculate mean of 
us_weather_discharge_obs <- 
  discharge_site_upstream %>% 
  select(station_id, upstream_area, geometry) %>% 
  rowwise() %>% 
  group_split() %>% 
  map(
    .f = \(x) {
      upstream_cells <- 
        st_join(x, weather_site, join = st_intersects) %>% 
        select(station_id, cell_id)
      weather_upstream <- 
        weather %>% 
        filter(cell_id %in% upstream_cells$cell_id) %>% 
        group_by(cell_id, date) %>% 
        summarize(
          daily_snowmelt = sum(snowmelt),
          daily_runoff_surface = sum(runoff_surface),
          daily_precip = sum(precip),
          .groups = "drop"
        ) %>% 
        group_by(date) %>% 
        summarize(
          mean_us_precip = mean(daily_precip),
          mean_us_snowmelt = mean(daily_snowmelt),
          mean_us_runoff_surface = mean(daily_runoff_surface),
          .groups = "drop"
        ) %>% 
        mutate(
          station_id = x$station_id,
          upstream_area = x$upstream_area
        ) %>% 
        select(
          station_id, date, mean_us_snowmelt, mean_us_runoff_surface,
          mean_us_precip, upstream_area
        )
      weather_upstream
    }
  ) %>% 
  list_rbind()

us_weather_discharge_pred <- 
  water_temp_site_upstream %>% 
  select(site, upstream_area, geometry) %>% 
  rowwise() %>% 
  group_split() %>% 
  map(
    .f = \(x) {
      upstream_cells <- 
        st_join(x, weather_site, join = st_intersects) %>% 
        select(site, cell_id)
      weather_upstream <- 
        weather %>% 
        filter(cell_id %in% upstream_cells$cell_id) %>% 
        group_by(cell_id, date) %>% 
        summarize(
          daily_snowmelt = sum(snowmelt),
          daily_runoff_surface = sum(runoff_surface),
          daily_precip = sum(precip),
          .groups = "drop"
        ) %>% 
        group_by(date) %>% 
        summarize(
          mean_us_precip = mean(daily_precip),
          mean_us_snowmelt = mean(daily_snowmelt),
          mean_us_runoff_surface = mean(daily_runoff_surface),
          .groups = "drop"
        ) %>% 
        mutate(
          site = x$site,
          upstream_area = x$upstream_area
        ) %>% 
        select(
          site, date, mean_us_snowmelt, mean_us_runoff_surface,
          mean_us_precip, upstream_area
        )
      weather_upstream
    }
  ) %>% 
  list_rbind()

rm(max_dist, closest_site_discharge, weather, discharge_site, 
   discharge_site_upstream, water_temp_site_upstream)

sbf_set_sub("tidy", "weather")
sbf_save_datas()

if(FALSE) {
  sbf_compare_data_archive()
}