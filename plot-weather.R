source("header.R")

sbf_set_sub("query")
sbf_load_datas()

us_weather_discharge_obs %<>% 
  group_by(station_id, date) %>% 
  summarize(
    us_evap_water = mean(us_evap_water),
    us_snowmelt = mean(us_snowmelt),
    us_runoff_surface = mean(us_runoff_surface),
    us_precip = mean(us_precip),
    upstream_area = first(upstream_area),
    .groups = "drop"
  )

weather_discharge %<>% 
  group_by(station_id, date) %>% 
  summarize(
    evap_water = mean(evap_water),
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
  geom_line(aes(x = date, y = us_runoff_surface, group = station_id, colour = station_id)) +
  xlab("Date") +
  ylab("Upstream Surface Runoff (m)") +
  NULL

sbf_open_window()
sbf_print(gp)

## Upstream Precipitation ----
gp <- ggplot(discharge) +
  geom_line(aes(x = date, y = us_precip, group = station_id, colour = station_id)) +
  xlab("Date") +
  ylab("Upstream Precipitation (m)") +
  NULL

sbf_open_window()
sbf_print(gp)

## Upstream Evaporation ----
gp <- ggplot(discharge) +
  geom_line(aes(x = date, y = us_evap_water, group = station_id, colour = station_id)) +
  xlab("Date") +
  ylab("Upstream Evaporation (m)") +
  NULL

sbf_open_window()
sbf_print(gp)

## Upstream Snowmelt ----
gp <- ggplot(discharge) +
  geom_line(aes(x = date, y = us_snowmelt, group = station_id, colour = station_id)) +
  xlab("Date") +
  ylab("Upstream Snow Melt (m)") +
  NULL

sbf_open_window()
sbf_print(gp)

## Relationship with discharge
# Upstream Evaporation ----
gp <- ggplot(discharge) +
  geom_point(aes(x = us_evap_water, y = discharge, colour = station_id)) +
  xlab("Upstream Evaporation (m)") +
  ylab(expression("Discharge"~(m^3/s))) + 
  NULL

sbf_open_window()
sbf_print(gp)

# Upstream Snowmelt ----
gp <- ggplot(discharge) +
  geom_point(aes(x = us_snowmelt, y = discharge, colour = station_id)) +
  xlab("Upstream Snow Melt (m)") +
  ylab(expression("Discharge"~(m^3/s))) + 
  NULL

sbf_open_window()
sbf_print(gp)

# Upstream Surface Runoff ----
gp <- ggplot(discharge) +
  geom_point(aes(x = us_runoff_surface, y = discharge, colour = station_id)) +
  xlab("Upstream Surface Runoff (m)") +
  ylab(expression("Discharge"~(m^3/s))) + 
  NULL

sbf_open_window()
sbf_print(gp)

# Upstream Precipitation ----
gp <- ggplot(discharge) +
  geom_point(aes(x = us_precip, y = discharge, colour = station_id)) +
  xlab("Upstream Precipitation (m)") +
  ylab(expression("Discharge"~(m^3/s))) + 
  NULL

sbf_open_window()
sbf_print(gp)


# Other relationships ----
# Surface Runoff vs Precipitation ----
gp <- ggplot(discharge) +
  geom_point(aes(x = us_precip, y = us_runoff_surface, colour = station_id)) +
  ylab("Upstream Surface Runoff (m)") +
  xlab("Upstream Precipitation (m)") +
  NULL

sbf_open_window()
sbf_print(gp)
