source("header.R")

sbf_set_sub("read", "weather")
sbf_load_datas()

weather %<>% 
  rename(
    air_temp = t2m,
    evap_water = evaow,
    snowmelt = smlt,
    runoff_subsurface = ssro,
    net_solar_rad = ssr,
    runoff_surface = sro,
    evap_total = e,
    precip = tp,
    soil_vol_1 = swvl1
  ) %>% 
  mutate(
    # Some rows have nonsensical (slightly) negative values: make these 0
    across(
      c(snowmelt, runoff_surface, net_solar_rad, precip), 
      \(x) if_else(x < 0, 0, x)
    ),
    date_time = as_datetime(time * 60 * 60, origin = origin),
    date_time = dtt_adjust_tz(date_time, tz = tz_analysis),
    date = dtt_date(date_time),
    time = dtt_time(date_time),
    air_temp = air_temp - 273.15,
    air_temp = set_units(air_temp, "degree_Celsius"),
    evap_water = set_units(evap_water, "m"),
    snowmelt = set_units(snowmelt, "m"),
    runoff_subsurface = set_units(runoff_subsurface, "m"),
    net_solar_rad = set_units(net_solar_rad, "J/m^2"),
    runoff_surface = set_units(runoff_surface, "m"),
    evap_total = set_units(evap_total, "m"),
    precip = set_units(precip, "m"),
    soil_vol_1 = set_units(soil_vol_1, "m^3")
  )

# Check no months of data are missing
chk_identical(
  weather %>% 
    group_by(latitude, longitude) %>% 
    filter(cur_group_id() == 1) %>% 
    ungroup() %>% 
    arrange(date_time) %>% 
    pull(date_time),
  seq(min(weather$date_time), max(weather$date_time), by = "hour")
)

# Check correct range of data
chk_equal(
  weather %>% min(date), 
  bounding_dates[1]
)

chk_equal(
  weather %>% max(date), 
  bounding_dates[2]
)

sbf_set_sub("clean", "weather")
sbf_save_datas()

if(FALSE) {
  sbf_compare_data_archive()
}