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
  ### Must be arranged in this order for the model!
  arrange(week, site)

gp <- ggplot(water_temp) +
  geom_line(aes(x = week, y = water_temp, colour = site, group = site)) +
  xlab("Week") +
  ylab(expression("Water temperature"~(degree*C))) +
  NULL

sbf_open_window(16, 8)
sbf_print(gp)


dat <- 
  water_temp %>% 
  filter(!is.na(water_temp)) %>% 
  mutate(
    # pred_water_temp = solar_rad / (discharge * 1e8)
    # pred_water_temp = solar_rad / (discharge * 24 * 60 * 60 * 41.8e3)
    # average daily solar rad in J/m^2
    # average air temperature in ˚C.
    # average volume of water??
    # 1 hectopascal = 1 millibar! no conversion necessary.
    eHeat = longwave_rad + solar_rad, # average weekly input ratiation!
    # probably need to account for the heat loss from the river too
    # and check scales of longwave rad and solar rad (should be roughly similar!?)
    eVolume = discharge * 41.8e3 * 1000, # this is volume / second??
    pred_water_temp = eHeat / eVolume,
    delta = log(eHeat / eVolume)
  )

dat %>% 
  select(site, week, longwave_rad, solar_rad, eHeat, discharge, eVolume, pred_water_temp, water_temp) %>% 
  group_by(site) %>% 
  summarize(
    across(
      c(longwave_rad, solar_rad, eHeat, discharge, eVolume, pred_water_temp, water_temp),
      mean
    ),
    .groups = "drop"
  ) %>% 
  arrange(eHeat)

## RSC looks decent with 5e8 multiplier on bottom.
## LTL decent with 1e8 multiplier on bottom
## Bunch look ok at 1e7

# gp <- ggplot(dat) +
#   geom_abline(aes(intercept = 0, slope = 1)) +
#   geom_point(aes(water_temp, y = pred_water_temp)) +
#   facet_wrap(~site) +
#   xlab("Obs Water Temp") +
#   ylab("Pred Water Temp") +
#   NULL
# 
# sbf_open_window()
# sbf_print(gp)

gp <- ggplot(dat) +
  geom_line(aes(x = date, y = water_temp)) +
  geom_line(aes(x = date, y = pred_water_temp), col = "blue") +
  # geom_line(aes(x = date, y = delta), col = "red") +
  facet_wrap(~site) +
  xlab("Date") +
  ylab("Water Temp") +
  NULL

sbf_open_window()
sbf_print(gp)

sbf_set_sub("temperature-mechanistic")
sbf_save_data(water_temp, "data")
sbf_save_object(D)
sbf_save_object(W)
sbf_save_object(H)
sbf_save_object(flow_con_mat)
sbf_save_object(E)

if(FALSE) {
  sbf_compare_data_archive()
}