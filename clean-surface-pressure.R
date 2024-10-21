source("header.R")

sbf_set_sub("read", "surface-pressure")
sbf_load_datas()

surface_pressure %<>% 
  mutate(
    surface_pressure = sp,
    surface_pressure = set_units(surface_pressure, "Pa"),
    date_time = dtt_date_time(valid_time, tz = "UTC"),
    date_time = dtt_adjust_tz(date_time, tz = tz_analysis),
    date = dtt_date(date_time),
    time = dtt_time(date_time),
    across(c(latitude, longitude), as.numeric)
  )

# Check no months of data are missing
chk_identical(
  surface_pressure %>% 
    group_by(latitude, longitude) %>% 
    filter(cur_group_id() == 1) %>% 
    ungroup() %>% 
    arrange(date_time) %>% 
    pull(date_time),
  seq(min(surface_pressure$date_time), max(surface_pressure$date_time), by = "hour")
)

# Check correct range of data
chk_equal(
  surface_pressure %>% pull(date) %>% min(),
  bounding_dates[1] - 1 # off due to time zone conversion
)

chk_equal(
  surface_pressure %>% pull(date) %>% max(),
  bounding_dates[2]
)

sbf_set_sub("clean", "surface-pressure", rm = TRUE)
sbf_save_datas()

if (FALSE) {
  sbf_compare_data_archive()
}
