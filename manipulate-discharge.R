source("header.R")

sbf_load_objects("distance/discharge")
# Convert to km
D <- downstream_hydrologic_distance / 1000
W <- weight_matrix
H <- total_hydrologic_distance / 1000
flow_con_mat <- flow_connected
E <- euclidean_distance / 1000

sbf_set_sub("prepare")
sbf_load_datas()

# Aggregate weather data by week
weather_discharge %<>% 
  right_join(date_ref, join_by(date)) %>% 
  group_by(station_id, week) %>% 
  summarize(
    across(
      c("air_temp", "evap_water", "evap_total", "snowmelt", "runoff_subsurface",
        "runoff_surface", "net_solar_rad", "precip", "soil_vol_1"),
      \(x) mean(x)
    ),
    .groups = "drop"
  ) %>% 
  arrange(station_id, week) %>% 
  rename(site = station_id) %>% 
  bind_rows(weather_temp)

us_weather <- 
  bind_rows(
    us_weather_discharge_obs %>% rename(site = station_id), 
    us_weather_discharge_pred
  ) %>% 
  right_join(date_ref, join_by(date)) %>% 
  group_by(site, week) %>% 
  summarize(
    across(
      c(mean_us_snowmelt, mean_us_runoff_surface, mean_us_precip),
      \(x) mean(x)
    ),
    upstream_area = first(upstream_area),
    .groups = "drop"
  ) %>% 
  arrange(site, week)
  
discharge %<>% 
  right_join(date_ref, join_by(date)) %>% 
  rename(site = station_id) %>% 
  group_by(site, week) %>% 
  summarize(
    discharge = mean(discharge),
    .groups = "drop"
  )

water_temp_dummy <- 
  tibble(
    site = unique(water_temp$site),
    week = min(date_ref$week),
    discharge = NA_real_
  )

discharge %<>% 
  bind_rows(water_temp_dummy) %>% 
  expand(site, week = full_seq(week, 1)) %>% 
  left_join(discharge, join_by(site, week)) %>% 
  left_join(weather_discharge, join_by(site, week)) %>% 
  left_join(us_weather, join_by(site, week)) %>% 
  mutate(
    precip = mean_us_precip * upstream_area / (60 * 60 * 24),
    H = seq(min(H), max(H) * 50, length.out = n()),
    E = seq(min(E), max(E) * 50, length.out = n()),
    site = factor(site),
    week = factor(week)
  ) %>% 
  ### Must be arranged in this order for the model
  arrange(week, site)

chk_equal(
  as.integer(levels(discharge$week)),
  min(as.integer(levels(discharge$week))):max(as.integer(levels(discharge$week)))
)

discharge_site %<>% 
  pull(station_id)

discharge %<>% 
  filter(site %in% discharge_site) %>% 
  mutate(
    across(c(site, week), \(x) fct_drop(x))
  )

D <- D[discharge_site, discharge_site]
W <- W[discharge_site, discharge_site]
H <- H[discharge_site, discharge_site]
flow_con_mat <- flow_con_mat[discharge_site, discharge_site]
E <- E[discharge_site, discharge_site]

gp <- ggplot(discharge) +
  geom_line(aes(x = week, y = discharge, colour = site, group = site)) +
  xlab("Week") +
  ylab(expression("Discharge"~(m^3/s))) +
  labs(colour = "Site") +
  NULL

sbf_open_window(10, 6)
sbf_print(gp)

gp <- ggplot(discharge) +
  geom_point(aes(x = precip, y = discharge, colour = site)) + 
  xlab(expression("Precipitation"~(m^3/s))) +
  ylab(expression("Discharge"~(m^3/s))) +
  NULL

sbf_open_window()
sbf_print(gp)



sbf_set_sub("discharge")
sbf_save_data(discharge, "data")
sbf_save_object(D)
sbf_save_object(W)
sbf_save_object(H)
sbf_save_object(flow_con_mat)
sbf_save_object(E)

if(FALSE) {
  sbf_compare_data_archive()
}