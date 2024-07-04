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
  mutate(
    H = seq(min(H), max(H) * 50, length.out = n()),
    E = seq(min(E), max(E) * 50, length.out = n()),
    nsite = nlevels(site)
  ) %>% 
  ### Must be arranged in this order for the model
  arrange(week, site)

gp <- ggplot(water_temp) +
  geom_line(aes(x = week, y = water_temp, colour = site, group = site)) +
  xlab("Week") +
  ylab(expression("Water temperature"~(degree*C))) +
  NULL

sbf_open_window(16, 8)
sbf_print(gp)

sbf_set_sub("temperature")
sbf_save_data(water_temp, "data")
sbf_save_object(D)
sbf_save_object(W)
sbf_save_object(H)
sbf_save_object(flow_con_mat)
sbf_save_object(E)

if(FALSE) {
  sbf_compare_data_archive()
}