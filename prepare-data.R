source("header.R")

sbf_set_sub("query")
sbf_load_datas()

D <- sbf_load_object("downstream_hydrologic_distance", sub = "distance/temp")
dist_sites <- rownames(D)

message("Decide what water temp flags to filter out")

water_temp %<>%
  filter(flag != "F") %>% 
  mutate(
    day = as.integer(date),
    day = day - min(day),
    week = day %/% 7 + 1,
    annual = dtt_year(date)
  )

date_ref <- 
  water_temp %>% 
  distinct(site, date, week, annual) %>% 
  add_count(week) %>% 
  filter(n > 21) %>% 
  distinct(date, week, annual) %>% 
  arrange(date)

date_ref_summ <- 
  date_ref %>% 
  group_by(week) %>% 
  summarize(annual = first(annual), .groups = "drop")

water_temp %<>% 
  filter(week %in% date_ref$week) %>% 
  group_by(site, week) %>% 
  summarize(
    water_temp = mean(temp),
    annual = first(annual),
    .groups = "drop"
  ) %>% 
  arrange(site, week)

# Aggregate weather data by week
weather_temp %<>% 
  filter(date %in% seq(min(date_ref$date), max(date_ref$date), by = "day")) %>%
  left_join(date_ref, join_by(date)) %>%
  group_by(site, week) %>% 
  summarize(
    across(
      c("air_temp", "evap_water", "evap_total", "snowmelt", "runoff_subsurface",
        "runoff_surface", "net_solar_rad", "precip", "soil_vol_1"),
      \(x) mean(x)
    ),
    .groups = "drop"
  ) %>% 
  arrange(site, week)

# Add missing values for water temp by expanding to every site-week combo
water_temp %<>% 
  expand(site, week = full_seq(week, 1)) %>% 
  left_join(water_temp %>% select(-annual), join_by(site, week)) %>% 
  left_join(date_ref_summ, join_by(week)) %>% 
  left_join(weather_temp, join_by(site, week)) %>% 
  mutate(
    site_remains = if_else2(site %in% dist_sites, TRUE, FALSE)
  ) %>% 
  filter(site_remains) %>% 
  mutate(
    site = factor(site),
    week = factor(week)
  ) %>%
  ### Must be arranged in this order for the model
  arrange(week, site)

sbf_set_sub("prepare")
sbf_save_datas()

if (FALSE) {
  sbf_compare_data_archive()
}