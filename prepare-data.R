source("header.R")

sbf_set_sub("query")
sbf_load_datas()

D <- sbf_load_object("downstream_hydrologic_distance", sub = "distance/temp")
dist_sites <- rownames(D)

# Sites
sites <- 
  water_temp_site %>% 
  tibble() %>% 
  select(site)

# Date ref for bounding dates
dates_sites_all <-
  tibble(
    date = seq(bounding_dates[1], bounding_dates[2], by = "day")
  ) %>% 
  mutate(
    annual = dtt_year(date),
    day = as.integer(date),
    day = day - min(day) + 1,
    week = day %/% 7 + 1,
  ) %>% 
  cross_join(sites)

first_date_week <- 
  dates_sites_all %>% 
  distinct(date, week) %>% 
  group_by(week) %>% 
  slice_head() %>% 
  ungroup()

# Keep all water temp except for "fail"
water_temp %<>%
  filter(flag != "F") %>% 
  full_join(dates_sites_all, join_by(date, site))

water_temp %<>% 
  # filter(week %in% date_ref$week) %>% 
  group_by(site, week) %>% 
  summarize(
    water_temp = mean(temp),
    annual = first(annual),
    .groups = "drop"
  ) %>% 
  arrange(site, week)

# Calculate vapour pressure
# constants:
ea_w <- 6.1121
eb_w <- 18.729
ec_w <- 257.87
ed_w <- 227.3
fa_w <- 1.00072
fb_w <- 3.2e-6
fc_w <- 5.9e-10

vapour_pressure <- 
  full_join(
    air_temp,
    dewpoint_temp,
    join_by(cell_id, site, date, time)
  ) %>% 
  full_join(
    surface_pressure,
    join_by(cell_id, site, date, time)
  ) %>% 
  mutate(
    surface_pressure = surface_pressure / 100, # in hectopascals for lw rad. eqn
    vapour_pressure = ea_w * exp((eb_w - (dewpoint_temp / ed_w)) * (dewpoint_temp / (dewpoint_temp + ec_w))) *
      (surface_pressure * (fc_w * dewpoint_temp^2 + fb_w) + fa_w)
  ) %>% 
  # Aggregate by week
  full_join(dates_sites_all, join_by(site, date)) %>% 
  group_by(site, week) %>% 
  summarize(
    vapour_pressure = mean(vapour_pressure),
    .groups = "drop"
  ) %>% 
  select(site, week, vapour_pressure) %>% 
  arrange(site, week)

# Aggregate air temp data by week
air_temp %<>%
  full_join(dates_sites_all, join_by(site, date)) %>%
  group_by(site, week) %>% 
  summarize(
    air_temp = mean(air_temp),
    .groups = "drop"
  ) %>% 
  arrange(site, week)

# Aggregate solar radiation data by week
# Get average daily solar rad (sum over each day then take mean)
solar_rad %<>%
  group_by(site, date) %>% 
  summarize(
    solar_rad = sum(solar_rad),
    .groups = "drop"
  ) %>% 
  full_join(dates_sites_all, join_by(site, date)) %>%
  group_by(site, week) %>% 
  summarize(
    solar_rad = mean(solar_rad),
    .groups = "drop"
  ) %>% 
  arrange(site, week)

# Aggregate discharge data by week, match to water temp sites
discharge %<>% 
  left_join(dates_sites_all, join_by(date, site)) %>%
  group_by(site, week) %>% 
  summarize(
    discharge = mean(discharge),
    .groups = "drop"
  )

elev <- 
  water_temp_site %>% 
  drop_units() %>% 
  tibble() %>% 
  select(site, elev)

# Add missing values for water temp by expanding to every site-week combo
water_temp %<>% 
  left_join(vapour_pressure, join_by(site, week)) %>% 
  left_join(air_temp, join_by(site, week)) %>% 
  left_join(solar_rad, join_by(site, week)) %>% 
  left_join(discharge, join_by(site, week)) %>% 
  left_join(elev, join_by(site)) %>% 
  left_join(first_date_week, join_by(week)) %>% 
  mutate(
    longwave_rad = (0.61 + 0.05 * sqrt(vapour_pressure)) * 5.670373e-8 * (air_temp + 273.16)^4 * 24 * 60 * 60 ,
    site_remains = if_else2(site %in% dist_sites, TRUE, FALSE)
  ) %>% 
  filter(site_remains) %>% 
  mutate(
    site = factor(site),
    week = factor(week)
  ) %>%
  ### **Must be arranged in this order for the model**
  arrange(week, site)

sbf_set_sub("prepare")
sbf_save_datas()

if (FALSE) {
  sbf_compare_data_archive()
}