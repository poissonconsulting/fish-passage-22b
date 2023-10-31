source("header.R")

sbf_set_sub("read", "air-temp")
sbf_load_datas()
sbf_load_objects()

air_temp_origin <- str_extract(air_temp_origin, "(?<=hours since ).*")

air_temp <- 
  air_temp %>% 
  mutate(
    temp = t2m - 273.15,
    date_time = as_datetime(time * 60 * 60, origin = air_temp_origin),
    date_time = dtt_adjust_tz(date_time, tz = tz_analysis),
    date = dtt_date(date_time),
  )

rm(air_temp_origin)

sbf_set_sub("clean", "air-temp")
sbf_save_datas()

if(FALSE) {
  sbf_compare_data_archive()
}