source("header.R")

sbf_set_sub("read", "dewpoint-temp")
sbf_load_datas()

dewpoint_temp %<>% 
  mutate(
    dewpoint_temp = d2m - 273.15,
    dewpoint_temp = set_units(dewpoint_temp, "kelvin"),
    date_time = dtt_date_time(valid_time, tz = "UTC"),
    date_time = dtt_adjust_tz(date_time, tz = tz_analysis),
    date = dtt_date(date_time),
    time = dtt_time(date_time),
    across(c(latitude, longitude), as.numeric)
  )

# Check no months of data are missing
chk_identical(
  dewpoint_temp %>% 
    group_by(latitude, longitude) %>% 
    filter(cur_group_id() == 1) %>% 
    ungroup() %>% 
    arrange(date_time) %>% 
    pull(date_time),
  seq(min(dewpoint_temp$date_time), max(dewpoint_temp$date_time), by = "hour")
)

# Check correct range of data
chk_equal(
  dewpoint_temp %>% pull(date) %>% min(),
  bounding_dates[1] - 1 # off due to time zone conversion
)

chk_equal(
  dewpoint_temp %>% pull(date) %>% max(),
  bounding_dates[2]
)

sbf_set_sub("clean", "dewpoint-temp", rm = TRUE)
sbf_save_datas()

if (FALSE) {
  sbf_compare_data_archive()
}
