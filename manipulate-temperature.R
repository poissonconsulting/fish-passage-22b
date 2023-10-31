source("header.R")

sbf_load_objects("distance")
# Convert to km
D <- downstream_hydrologic_distance / 1000
W <- weight_matrix
H <- total_hydrologic_distance / 1000
flow_con_mat <- flow_connected
E <- euclidean_distance / 1000

dist_sites <- rownames(D)

sbf_load_datas("query")
sbf_load_datas("radiation")

# Weekly mean water temp ----
date_ref <-
  tibble(date = seq(min(water_temp$date), max(water_temp$date), by = "day")) %>% 
  mutate(
    annual = dtt_year(date),
    day = as.integer(date),
    day = day - min(day),
    week = as.integer(day %/% 7 + 1),
    doy = dtt_doy(date)
  ) %>% 
  group_by(week) %>% 
  arrange(annual, doy, .by_group = TRUE) %>% 
  summarize(
    annual = first(annual), 
    doy = first(doy), 
    .groups = "drop"
  )

# Check full seq of dates in discharge data
chk_equal(
  unique(discharge$date) %>% sort(), 
  seq(min(discharge$date), max(discharge$date), 1)
)

water_temp <- 
  water_temp %>% 
  ### TODO: Decide what flags to keep?
  filter(flag != "F") %>% 
  mutate(
    annual = dtt_year(date),
    day = as.integer(date),
    day = day - min(day),
    week = as.integer(day %/% 7 + 1),
    doy = dtt_doy(date)
  ) %>% 
  # add in discharge data
  left_join(discharge, join_by(site, date)) %>% 
  group_by(site, week) %>%
  summarize(
    mean_temp = mean(temp), 
    mean_level = mean(level),
    mean_discharge = mean(discharge),
    .groups = "drop"
  ) %>% 
  drop_na(mean_level)

chk_equal(
  unique(water_temp$week) %>% sort(), 
  seq(min(water_temp$week), max(water_temp$week), 1)
)

data <- 
  water_temp %>% 
  expand(site, week = full_seq(week, 1)) %>% 
  left_join(water_temp, join_by(site, week)) %>% 
  left_join(date_ref, join_by(week), relationship = "many-to-one") %>% 
  # Mean open sky shortwave radiation in weekly period
  # Calculated by minute during water temp recording period
  # Summed over each day
  # And mean by week
  left_join(radiation, join_by(site, week)) %>%
  mutate(
    site_remains = if_else2(site %in% dist_sites, TRUE, FALSE)
  ) %>% 
  filter(site_remains) %>% 
  select(site, week, doy, annual, mean_temp, mean_discharge, mean_level, shortwave) %>% 
  group_by(week) %>% 
  mutate(
    mean_volume = mean_discharge,
    drop_week = if_else2(any(is.na(mean_level)), TRUE, FALSE)
  ) %>% 
  filter(!drop_week) %>% 
  ungroup()
  

# ### Filter to dates with at least 3 sites with data
# keep_weeks <-
#   data %>%
#   filter(!is.na(mean_temp)) %>%
#   dplyr::count(week) %>%
#   filter(n >= 3) %>%
#   pull(week)
# 
# # check no gaps in weeks
# chk_equal(keep_weeks, min(keep_weeks):max(keep_weeks))

data %<>%
  # filter(week %in% keep_weeks) %>%
  mutate(
    week = factor(week),
    site = factor(site),
    H = seq(min(H), max(H) * 50, length.out = n()),
    E = seq(min(E), max(E) * 50, length.out = n())
  ) %>%
  ### Must be arranged in this order for the model
  arrange(week, site)

# chk_equal(levels(data$site), dist_sites)

gp <- ggplot(data) +
  geom_line(aes(x = doy, y = mean_temp, group = site, color = site)) +
  NULL

sbf_open_window(6, 6)
sbf_print(gp)

gp <- ggplot(data) +
  geom_line(aes(x = doy, y = shortwave, group = site, colour = site)) +
  NULL

sbf_open_window(6, 6)
sbf_print(gp)

sbf_set_sub("temperature")
sbf_save_data(data, "data")
sbf_save_object(D, "D")
sbf_save_object(W, "W")
sbf_save_object(H, "H")
sbf_save_object(flow_con_mat, "flow_con_mat")
sbf_save_object(E, "E")

if(FALSE) {
  sbf_compare_data_archive()
}
