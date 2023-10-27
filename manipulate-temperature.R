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

# # Daily mean water temp ----
# water_temp <-
#   water_temp %>% 
#   filter(flag == "P") %>% 
#   select(site, date, time, temp) %>% 
#   group_by(site, date) %>% 
#   summarize(mean_temp = mean(temp), .groups = "drop")
# 
# data <- water_temp %>% 
#   expand(site, date = full_seq(date, 1)) %>% 
#   left_join(water_temp, join_by(site, date)) %>% 
#   mutate(
#     site_remains = if_else2(site %in% dist_sites, TRUE, FALSE)
#   ) %>% 
#   filter(site_remains) %>% 
#   select(site, date, mean_temp)
# 
# ### Filter to dates with at least 3 sites with data
# keep_dates <- 
#   data %>% 
#   filter(!is.na(mean_temp)) %>% 
#   count(date) %>% 
#   filter(n >= 3) %>% 
#   pull(date)
# 
# data %<>% 
#   filter(date %in% keep_dates) %>% 
#   mutate(
#     doy = dtt_doy(date),
#     date = factor(date),
#     site = factor(site)
#   ) %>% 
#   ### Must be arranged in this order for the model
#   arrange(date, site)
# 
# chk_equal(levels(data$site), dist_sites)

# Weekly mean water temp ----
water_temp <-
  water_temp %>% 
  filter(flag == "P") %>% 
  select(site, date, time, temp) %>% 
  mutate(
    day = as.integer(dtt_date(date)),
    day = day - min(day),
    week = as.integer(day %/% 7 + 1)
  ) %>% 
  group_by(week) %>% 
  mutate(doy = min(dtt_doy(date))) %>% 
  group_by(site, week, doy) %>% 
  summarize(mean_temp = mean(temp), .groups = "drop")

week_doy <- 
  water_temp %>% 
  select(week, doy) %>% 
  distinct() %>% 
  arrange(week)

data <- 
  water_temp %>% 
  expand(site, week = full_seq(week, 1)) %>% 
  left_join(water_temp, join_by(site, week)) %>% 
  left_join(week_doy, join_by(week)) %>% 
  mutate(
    doy = if_else(is.na(doy.x), doy.y, doy.x),
    site_remains = if_else2(site %in% dist_sites, TRUE, FALSE)
  ) %>% 
  filter(site_remains) %>% 
  select(site, week, doy, mean_temp)

### Filter to dates with at least 3 sites with data
keep_weeks <- 
  data %>% 
  filter(!is.na(mean_temp)) %>% 
  dplyr::count(week) %>% 
  filter(n >= 3) %>% 
  pull(week)

# check no gaps
chk_equal(keep_weeks, min(keep_weeks):max(keep_weeks))

data %<>% 
  filter(week %in% keep_weeks) %>% 
  mutate(
    week = factor(week),
    site = factor(site),
    H = seq(min(H), max(H) * 50, length.out = n()),
    E = seq(min(E), max(E) * 50, length.out = n())
  ) %>%
  ### Must be arranged in this order for the model
  arrange(week, site)

chk_equal(levels(data$site), dist_sites)

gp <- ggplot(data) +
  geom_line(aes(x = doy, y = mean_temp, group = site, color = site)) +
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
