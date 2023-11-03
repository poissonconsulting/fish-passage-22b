source("header.R")

sbf_set_sub("query")
sbf_load_datas()

weather_discharge %<>% 
  group_by(station_id, date) %>% 
  summarize(
    snowmelt = mean(snowmelt),
    runoff_surface = mean(runoff_surface),
    precip = mean(precip),
    soil_vol_1 = mean(soil_vol_1),
    .groups = "drop"
  )

discharge %<>% 
  left_join(us_weather_discharge_obs, by = c("station_id", "date")) %>% 
  left_join(weather_discharge, by = c("station_id", "date"))

# Area ----
gp <- ggplot(discharge) +
  geom_point(aes(x = upstream_area, y = discharge, colour = station_id)) +
  xlab(expression("Upstream Area"~(m^2))) +
  ylab(expression("Discharge"~(m^3/s))) +
  NULL

sbf_open_window()
sbf_print(gp)

# Groundwater ----
gp <- ggplot(discharge) +
  geom_point(aes(x = soil_vol_1, y = discharge, colour = station_id)) +
  xlab(expression("Soil Volume"~(m^3))) +
  ylab(expression("Discharge"~(m^3/s))) + 
  NULL

sbf_open_window()
sbf_print(gp)

# Surface runoff ----
gp <- ggplot(discharge) +
  geom_point(aes(x = runoff_surface, y = discharge, colour = station_id)) +
  xlab(expression("Surface Runoff"~(m))) +
  ylab(expression("Discharge"~(m^3/s))) + 
  NULL

sbf_open_window()
sbf_print(gp)

# Discharge through time
gp <- ggplot(discharge) +
  geom_line(aes(x = date, y = discharge, group = station_id, colour = station_id)) +
  xlab("Date") +
  ylab(expression("Discharge"~(m^3/s))) +
  NULL

sbf_open_window()
sbf_print(gp)

# Upstream weather variables through time ----
## Upstream Surface Runoff ----
gp <- ggplot(discharge) +
  geom_line(aes(x = date, y = mean_us_runoff_surface, group = station_id, colour = station_id)) +
  xlab("Date") +
  ylab("Upstream Surface Runoff (m)") +
  NULL

sbf_open_window()
sbf_print(gp)

## Upstream Precipitation ----
gp <- ggplot(discharge) +
  geom_line(aes(x = date, y = mean_us_precip, group = station_id, colour = station_id)) +
  xlab("Date") +
  ylab("Upstream Precipitation (m)") +
  NULL

sbf_open_window()
sbf_print(gp)

## Upstream Snowmelt ----
gp <- ggplot(discharge) +
  geom_line(aes(x = date, y = mean_us_snowmelt, group = station_id, colour = station_id)) +
  xlab("Date") +
  ylab("Upstream Snow Melt (m)") +
  NULL

sbf_open_window()
sbf_print(gp)

## Relationship with discharge
# Upstream Snowmelt ----
gp <- ggplot(discharge) +
  geom_point(aes(x = mean_us_snowmelt, y = discharge, colour = station_id)) +
  xlab("Upstream Snow Melt (m)") +
  ylab(expression("Discharge"~(m^3/s))) + 
  NULL

sbf_open_window()
sbf_print(gp)

# Upstream Surface Runoff ----
gp <- ggplot(discharge) +
  geom_point(aes(x = mean_us_runoff_surface, y = discharge, colour = station_id)) +
  xlab("Upstream Surface Runoff (m)") +
  ylab(expression("Discharge"~(m^3/s))) + 
  NULL

sbf_open_window()
sbf_print(gp)

# Upstream Precipitation ----
gp <- ggplot(discharge) +
  geom_point(aes(x = mean_us_precip, y = discharge, colour = station_id)) +
  xlab("Upstream Precipitation (m)") +
  ylab(expression("Discharge"~(m^3/s))) + 
  NULL

sbf_open_window()
sbf_print(gp)


# Other relationships ----
# Surface Runoff vs Precipitation ----
gp <- ggplot(discharge) +
  geom_point(aes(x = mean_us_precip, y = mean_us_runoff_surface, colour = station_id)) +
  ylab("Upstream Surface Runoff (m)") +
  xlab("Upstream Precipitation (m)") +
  NULL

sbf_open_window()
sbf_print(gp)
