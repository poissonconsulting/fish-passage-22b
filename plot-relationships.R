source("header.R")

sbf_set_sub("prepare")
sbf_load_datas()

sbf_set_sub("plot-relationships")

data <- 
  water_temp %>% 
  mutate(dayte = dtt_dayte(date))

gp <- ggplot(data %>% filter(!is.na(water_temp))) +
  geom_point(aes(x = date, y = water_temp)) + 
  facet_wrap(~site) +
  xlab("") +
  ylab("") + 
  NULL

sbf_open_window()
sbf_print(gp)

gp <- ggplot(data %>% filter(!is.na(water_temp))) +
  geom_point(aes(x = solar_rad, y = air_temp, colour = as.numeric(week))) +
  xlab("Net Solar Radiation") +
  ylab("Air Temperature") +
  NULL

sbf_open_window()
sbf_print(gp)

gp <- ggplot(data %>% filter(!is.na(water_temp))) +
  geom_point(aes(x = solar_rad, y = discharge, colour = water_temp)) +
  xlab("Net Solar Radiation") +
  ylab("Discharge") +
  NULL

sbf_open_window()
sbf_print(gp)

gp <- ggplot(data) +
  geom_line(aes(x = dayte, y = water_temp, colour = site)) +
  xlab("Day") +
  ylab("Water Temp") +
  NULL

sbf_open_window()
sbf_print(gp)

gp <- ggplot(data %>% filter(!is.na(water_temp))) +
  geom_point(aes(x = dayte, y = solar_rad, colour = discharge)) +
  facet_wrap(~site) +
  NULL

sbf_open_window()
sbf_print(gp)

gp <- ggplot(data %>% filter(!is.na(water_temp))) +
  geom_line(aes(x = dayte, y = water_temp, colour = elev)) +
  facet_wrap(~site) +
  NULL

sbf_open_window()
sbf_print(gp)

gp <- ggplot(data %>% filter(!is.na(water_temp))) +
  geom_point(aes(x = solar_rad, y = air_temp, colour = water_temp)) +
  facet_wrap(~annual) +
  NULL

sbf_open_window()
sbf_print(gp)

gp <- ggplot(data %>% filter(!is.na(water_temp))) +
  geom_point(aes(x = elev, y = solar_rad, colour = air_temp)) +
  NULL

sbf_open_window()
sbf_print(gp)

gp <- ggplot(data %>% filter(!is.na(water_temp))) +
  geom_point(aes(x = air_temp, y = water_temp, colour = discharge)) +
  facet_wrap(~annual) +
  NULL

sbf_open_window()
sbf_print(gp)

gp <- ggplot(data %>% filter(!is.na(water_temp))) +
  geom_point(aes(x = dayte, y = air_temp, colour = water_temp)) +
  facet_wrap(~site) +
  NULL

sbf_open_window()
sbf_print(gp)

gp <- ggplot(data %>% filter(!is.na(water_temp))) +
  geom_point(aes(x = dayte, y = discharge, colour = water_temp)) +
  facet_wrap(~site) +
  NULL

sbf_open_window()
sbf_print(gp)

gp <- ggplot(data %>% filter(!is.na(water_temp))) +
  geom_point(aes(x = discharge, y = solar_rad, colour = water_temp)) +
  facet_wrap(~site) +
  NULL

sbf_open_window()
sbf_print(gp)

data %>% 
  group_by(site) %>% 
  summarize(
    elev = first(elev),
    discharge = mean(discharge, na.rm = TRUE),
    water_temp = mean(water_temp, na.rm = TRUE),
    air_temp = mean(air_temp, na.rm = TRUE),
    solar_rad = mean(solar_rad, na.rm = TRUE),
    longwave_rad = mean(longwave_rad, na.rm = TRUE)
  ) %>% 
  mutate(
    across(
      c(elev, discharge, water_temp, air_temp, solar_rad, longwave_rad),
      \(x) as.vector(scale(x))
    )
  ) %>% 
  arrange(water_temp) %>% 
  select(site, elev, air_temp, solar_rad, longwave_rad, discharge, water_temp) %>% 
  print(n = nrow(.)) 

data %<>% 
  pivot_longer(
    cols = c(
      solar_rad,
      longwave_rad,
      discharge,
      water_temp,
      elev
    ),
    names_to = "variable",
    values_to = "value"
  ) %>% 
  select(site, week, annual, variable, value, dayte) %>% 
  mutate(
    week = as.numeric(as.character(week))
  )

# gp <- ggplot(data) +
#   geom_point(aes(x = value, y = water_temp, colour = dayte, shape = factor(annual)), alpha = 0.4) +
#   facet_grid(cols = vars(variable), rows = vars(site), scales = "free_x") +
#   xlab("Value") +
#   ylab("Water Temp") +
#   NULL
# 
# sbf_open_window()
# sbf_print(gp)

data %<>% 
  group_by(variable, week) %>% 
  mutate(
    value = as.vector(scale(value))
  ) %>% 
  ungroup()

gp <- ggplot(data) +
  geom_hline(aes(yintercept = 0), linetype = "dotted") +
  geom_line(aes(x = week, y = value, group = variable, colour = variable)) +
  facet_wrap(~site) +
  xlab("Week") +
  ylab("Scaled Value of Variable") + 
  NULL

sbf_open_window(15, 8)
sbf_print(gp)

