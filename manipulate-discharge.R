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
  )

lag_number_weeks <- 4
us_weather_lag <-
  us_weather %>%
  left_join(
    date_ref_full %>% 
      distinct(date, week, annual), 
    join_by(date)
  ) %>%
  mutate(
    day = as.integer(date) - as.integer(bounding_dates[1]),
    week = day %/% 7 + 1,
    annual = dtt_year(date)
  ) %>% 
  summarize(
    across(
      c(sum_us_snowmelt, sum_us_runoff_surface, sum_us_precip),
      # c(mean_us_snowmelt, mean_us_runoff_surface, mean_us_precip),
      \(x) mean(x)
    ),
    .by = c(site, week)
  ) %>% 
  group_by(site) %>% 
  arrange(week, .by_group = TRUE) %>% 
  mutate(
    lag_us_snowmelt = zoo::rollsum(
      sum_us_snowmelt,
      k = lag_number_weeks, 
      fill = NA, 
      align = "right"
    ),
    lag_us_runoff_surface = zoo::rollsum(
      sum_us_runoff_surface,
      k = lag_number_weeks, 
      fill = NA, 
      align = "right"
    ),
    lag_us_precip = zoo::rollsum(
      sum_us_precip,
      k = lag_number_weeks, 
      fill = NA, 
      align = "right"
    )
      # lag_us_snowmelt = zoo::rollsum(
      #   mean_us_snowmelt,
      #   k = lag_number_weeks, 
      #   fill = NA, 
      #   align = "right"
      # ),
      # lag_us_runoff_surface = zoo::rollsum(
      #   mean_us_runoff_surface,
      #   k = lag_number_weeks, 
      #   fill = NA, 
      #   align = "right"
      # ),
      # lag_us_precip = zoo::rollsum(
      #   mean_us_precip,
      #   k = lag_number_weeks, 
      #   fill = NA, 
      #   align = "right"
      # )
  ) %>% 
  ungroup() %>% 
  select(site, week, lag_us_snowmelt, lag_us_runoff_surface, lag_us_precip) %>% 
  mutate(across(starts_with("lag"), \(x) if_else2(x < 0, 0, x)))
  
us_weather %<>% 
  right_join(date_ref, join_by(date)) %>% 
  group_by(site, week) %>% 
  summarize(
    across(
      c(mean_us_snowmelt, mean_us_runoff_surface, mean_us_precip),
      \(x) mean(x)
    ),
    across(
      c(sum_us_snowmelt, sum_us_runoff_surface, sum_us_precip),
      \(x) sum(x)
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
  left_join(us_weather_lag, join_by(site, week)) %>% 
  filter(!is.na(lag_us_precip)) %>% 
  group_by(site) %>% 
  mutate(
    baseline = min(discharge),
    # baseline = if_else2(row_number() == 1, baseline, NA_real_),
  ) %>% 
  ungroup() %>% 
  mutate(
    precip_vol = mean_us_precip * upstream_area / (60 * 60 * 24),
    snowmelt_vol = mean_us_snowmelt * upstream_area / (60 * 60 * 24),
    precip_vol_lag = lag_us_precip * upstream_area / (60 * 60 * 24),
    snowmelt_vol_lag = lag_us_snowmelt * upstream_area / (60 * 60 * 24),
    across(
      c(precip_vol, snowmelt_vol, precip_vol_lag, snowmelt_vol_lag), 
      \(x) ifelse(x < 0, 0, x)
    ),
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
  geom_point(aes(x = upstream_area, y = baseline)) +
  geom_rug(aes(x = upstream_area)) +
  xlab("Week") +
  ylab(expression("Discharge"~(m^3/s))) +
  labs(colour = "Site") +
  NULL

sbf_open_window(10, 6)
sbf_print(gp)


gp <- ggplot(discharge) +
  geom_line(aes(x = week, y = discharge, colour = site, group = site)) +
  xlab("Week") +
  ylab(expression("Discharge"~(m^3/s))) +
  labs(colour = "Site") +
  NULL

sbf_open_window(10, 6)
sbf_print(gp)

gp <- ggplot(discharge) +
  geom_point(aes(x = log(precip_vol), y = log(discharge), colour = site)) + 
  xlab(expression("Log Precipitation"~(m^3/s))) +
  ylab(expression("Log Discharge"~(m^3/s))) +
  NULL

sbf_open_window()
sbf_print(gp)

gp <- ggplot(discharge) +
  geom_point(aes(x = log(snowmelt_vol), y = log(discharge), colour = site)) + 
  xlab(expression("Log Snow Melt"~(m^3/s))) +
  ylab(expression("Log Discharge"~(m^3/s))) +
  NULL

sbf_open_window()
sbf_print(gp)

gp <- ggplot(discharge) +
  geom_point(aes(x = log(lag_us_precip), y = log(discharge), colour = site)) + 
  xlab(expression("Log US Precipitation Lag"~(m^3/s))) +
  ylab(expression("Log Discharge"~(m^3/s))) +
  NULL

sbf_open_window()
sbf_print(gp)

gp <- ggplot(discharge) +
  geom_point(aes(x = log(lag_us_snowmelt), y = log(discharge), colour = site)) + 
  xlab(expression("Log US Snow Melt Lag"~(m^3/s))) +
  ylab(expression("Log Discharge"~(m^3/s))) +
  NULL

sbf_open_window()
sbf_print(gp)

gp <- ggplot(discharge) +
  geom_point(aes(x = log(precip_vol_lag), y = log(discharge), colour = site)) + 
  xlab(expression("Log US Precipitation Lag Volume"~(m^3/s))) +
  ylab(expression("Log Discharge"~(m^3/s))) +
  NULL

sbf_open_window()
sbf_print(gp)

gp <- ggplot(discharge) +
  geom_point(aes(x = log(snowmelt_vol_lag), y = log(discharge), colour = site)) + 
  xlab(expression("Log Snow Melt Lag Volume"~(m^3/s))) +
  ylab(expression("Log Discharge"~(m^3/s))) +
  NULL

sbf_open_window()
sbf_print(gp)

gp <- ggplot(discharge) +
  geom_point(aes(x = log(mean_us_precip), y = log(discharge), colour = site)) + 
  xlab(expression("Mean US Precipitation"~(m^3/s))) +
  ylab(expression("Log Discharge"~(m^3/s))) +
  NULL

sbf_open_window()
sbf_print(gp)

gp <- ggplot(discharge) +
  geom_point(aes(x = log(mean_us_snowmelt), y = log(discharge), colour = site)) + 
  xlab(expression("Log Mean US Snow Melt"~(m^3/s))) +
  ylab(expression("Log Discharge"~(m^3/s))) +
  NULL

sbf_open_window()
sbf_print(gp)

gp <- ggplot(discharge) +
  geom_point(aes(x = log(sum_us_precip), y = log(discharge), colour = site)) + 
  xlab(expression("Log Sum US Precipitation"~(m^3/s))) +
  ylab(expression("Log Discharge"~(m^3/s))) +
  NULL

sbf_open_window()
sbf_print(gp)

gp <- ggplot(discharge) +
  geom_point(aes(x = log(sum_us_snowmelt), y = log(discharge), colour = site)) + 
  xlab(expression("Log Sum US Snow Melt"~(m^3/s))) +
  ylab(expression("Log Discharge"~(m^3/s))) +
  NULL

sbf_open_window()
sbf_print(gp)

gp <- ggplot(discharge) +
  geom_point(aes(x = week, y = precip_vol)) +
  geom_point(aes(x = week, y = precip_vol_lag), colour = "red") + 
  facet_grid(rows = vars(site)) +
  NULL

sbf_open_window()
sbf_print(gp)

gp <- ggplot(discharge) +
  geom_point(aes(x = week, y = snowmelt_vol)) +
  geom_point(aes(x = week, y = snowmelt_vol_lag), colour = "red") + 
  facet_grid(rows = vars(site)) +
  NULL

sbf_open_window()
sbf_print(gp)

gp <- ggplot(discharge) +
  geom_point(aes(x = week, y = log(discharge))) +
  geom_point(aes(x = week, y = log(mean_us_snowmelt)), colour = "blue") +
  geom_point(aes(x = week, y = log(mean_us_precip)), colour = "red") +
  facet_grid(rows = vars(site)) +
  NULL

sbf_open_window()
sbf_print(gp)

sbf_set_sub("discharge")
sbf_save_data(discharge, "data")
sbf_save_data(date_ref, "date_ref")
sbf_save_object(D)
sbf_save_object(W)
sbf_save_object(H)
sbf_save_object(flow_con_mat)
sbf_save_object(E)

if(FALSE) {
  sbf_compare_data_archive()
}