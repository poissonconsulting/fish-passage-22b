source("header.R")

sbf_load_objects("distance/temp")
# Convert to km
D <- downstream_hydrologic_distance / 1000
W <- weight_matrix
H <- total_hydrologic_distance / 1000
flow_con_mat <- flow_connected
E <- euclidean_distance / 1000

sbf_set_sub("prepare")
sbf_load_datas()

water_temp %<>% 
  group_by(site) %>%
  mutate(
    discharge = discharge / mean(discharge)
  ) %>% 
  ungroup() %>%
  mutate(
    water_temp = if_else2(water_temp < 0, 0, water_temp),
    annual = factor(dtt_year(date)),
    week_year = as.integer(week),
    H = seq(min(H), max(H) * 50, length.out = n()),
    E = seq(min(E), max(E) * 50, length.out = n()),
    nsite = nlevels(site)
  ) %>% 
  group_by(annual) %>%
  mutate(
    max_week_year = if_else2(row_number() == n(), max(week_year), NA),
    last_row = if_else2(row_number() == n(), TRUE, FALSE)
  ) %>% 
  ungroup() %>% 
  fill(max_week_year, .direction = "down") %>% 
  group_by(annual) %>% 
  mutate(
    max_week_year = if_else2(row_number() == n(), lag(max_week_year), max_week_year),
    max_week_year = replace_na(max_week_year, replace = 0),
    week_year = week_year - max_week_year,
    max_week_year = max(week_year)
  ) %>% 
  ungroup() %>% 
  ### Must be arranged in this order for the model!
  arrange(week, site) 

gp <- ggplot(water_temp) +
  geom_line(aes(x = week, y = water_temp, colour = site, group = site)) +
  xlab("Week") +
  ylab(expression("Water temperature"~(degree*C))) +
  NULL

sbf_open_window(16, 8)
sbf_print(gp)

sbf_set_sub("temperature-air2stream")
sbf_save_data(water_temp, "data")
sbf_save_object(D)
sbf_save_object(W)
sbf_save_object(H)
sbf_save_object(flow_con_mat)
sbf_save_object(E)

if(FALSE) {
  sbf_compare_data_archive()
}