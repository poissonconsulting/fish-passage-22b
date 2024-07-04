source("header.R")

sbf_set_sub("temperature")

sbf_load_datas()
sbf_load_objects()

data %<>% 
  mutate(
    eShortwave = net_solar_rad * 24,
    # e_atm = function of air temp and pressure?
    e_atm = 1,
    eLongwave = (0.97 * e_atm * 5.670367e-8 * (air_temp + 273.15)^4), #- (0.97 * 5.670367e-8 * water_temp^4),
    eHeat = eShortwave + eLongwave,
    eTemp = eHeat / (discharge * 41.8e3 * 1000)
  )

data %>% 
  filter(!is.na(water_temp)) %>% 
  summarize(
    mean_water_temp = mean(water_temp),
    mean_eTemp = mean(eTemp)
  ) %>% 
  mutate(
    diff = mean_water_temp / mean_eTemp
  )

data %>% 
  group_by(site) %>% 
  summarize(
    mean_discharge = mean(discharge),
    mean_air_temp = mean(air_temp),
    mean_nsr = mean(net_solar_rad),
    mean_water_temp = mean(water_temp, na.rm = TRUE),
    mean_e_temp = mean(eTemp)
  ) %>% 
  print(n = nrow(.))

gp <- ggplot(data) +
  geom_point(aes(x = water_temp, y = eTemp, colour = net_solar_rad)) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  facet_wrap(~site) +
  NULL

sbf_open_window()
sbf_print(gp)
