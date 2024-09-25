source("header.R")

sbf_set_sub("read", "solar-rad")
sbf_load_datas()

solar_rad %<>% 
  mutate(
    solar_rad = set_units(ssr, "J/m^2"),
    date_time = dtt_date_time(valid_time, tz = "UTC"),
    date_time = dtt_adjust_tz(date_time, tz = tz_analysis),
    date = dtt_date(date_time),
    time = dtt_time(date_time),
    across(c(latitude, longitude), as.numeric)
  )

# Check no months of data are missing
chk_identical(
  solar_rad %>% 
    group_by(latitude, longitude) %>% 
    filter(cur_group_id() == 1) %>% 
    ungroup() %>% 
    arrange(date_time) %>% 
    pull(date_time),
  seq(min(solar_rad$date_time), max(solar_rad$date_time), by = "hour")
)

# Check correct range of data
chk_equal(
  solar_rad %>% pull(date) %>% min(),
  bounding_dates[1] - 1 # off due to time zone conversion
)

chk_equal(
  solar_rad %>% pull(date) %>% max(),
  bounding_dates[2]
)

sbf_set_sub("clean", "solar-rad", rm = TRUE)
sbf_save_datas()

if (FALSE) {
  sbf_compare_data_archive()
}
