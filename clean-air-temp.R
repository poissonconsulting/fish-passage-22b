source("header.R")

sbf_set_sub("read", "air-temp")
sbf_load_datas()

air_temp %<>% 
  mutate(
    air_temp = t2m - 273.15,
    air_temp = set_units(air_temp, "degree_Celsius"),
    date_time = as_datetime(time * 60 * 60, origin = origin),
    date_time = dtt_adjust_tz(date_time, tz = tz_analysis),
    date = dtt_date(date_time),
    time = dtt_time(date_time)
  )

# Check no months of data are missing
chk_identical(
  air_temp %>% 
    group_by(latitude, longitude) %>% 
    filter(cur_group_id() == 1) %>% 
    ungroup() %>% 
    arrange(date_time) %>% 
    pull(date_time),
  seq(min(air_temp$date_time), max(air_temp$date_time), by = "hour")
)

# Check correct range of data
chk_equal(
  air_temp %>% pull(date) %>% min(),
  bounding_dates[1] - 1 # off due to time zone conversion
)

chk_equal(
  air_temp %>% pull(date) %>% max(),
  bounding_dates[2]
)

sbf_set_sub("clean", "air-temp")
sbf_save_datas()

if(FALSE) {
  sbf_compare_data_archive()
}
