source("header.R")

sbf_set_sub("clean", "water-temp")
sbf_load_datas()

water_temp %<>%
  rename(site_description = site) %>%
  left_join(
    select(water_temp_site, site, site_description), 
    join_by(site_description)
  ) %>%
  left_join(water_temp_problem_periods, join_by(site, date_time)) %>% 
  mutate(problem = replace_na(problem, FALSE)) %>% 
  left_join(water_temp_visit, join_by(site, date_time)) %>% 
  mutate(data_downloading = replace_na(data_downloading, FALSE)) %>%
  # Also flag data as "fail" if there was a problem with the logger
  # or during times the data was being downloaded
  mutate(
    new_flag = case_when(
      flag == "F" ~ "F",
      flag == "B" ~ "B",
      flag == "P" ~ "P",
      problem ~ "I",
      data_downloading ~ "D",
      .default = NA_character_
    ),
    date = dtt_date(date_time),
    time = dtt_time(date_time)
  ) %>% 
  select(site, date, time, temp, flag = new_flag, comment) %>% 
  drop_na(temp) %>% 
  # Filter out duplicate site/date_time observations
  group_by(site, date, time) %>%
  filter(row_number() == 1) %>%
  ungroup()

rm(water_temp_notes, water_temp_meta_data, water_temp_problem_periods,
   water_temp_visit)

sbf_set_sub("tidy", "water-temp")
sbf_save_datas()

if(FALSE) {
  sbf_compare_data_archive()
}