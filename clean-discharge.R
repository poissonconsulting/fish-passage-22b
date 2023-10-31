source("header.R")

sbf_set_sub("read", "discharge")
sbf_load_datas()

discharge %<>% 
  group_by(file) %>% 
  group_split() %>% 
  map(
    .f = \(x) {
      meta <- 
        x %>% 
        select(-file) %>% 
        rename(title = `# Title:                 GRDC STATION DATA FILE`) %>% 
        mutate(
          header = str_detect(title, "^#")
        )
      
      data <- meta %>% 
        filter(!header) %>% 
        filter(row_number() != 1) %>% 
        separate_wider_delim(
          title, 
          delim = ";",
          names_sep = "_"
        ) %>% 
        rename(
          date = title_1,
          time = title_2,
          value = title_3
        ) %>% 
        mutate(
          value = str_squish(value),
          value = as.numeric(value),
          value = if_else2(value == -999, NA_real_, value),
          date = dtt_date(date)
        ) %>% 
        select(date, time, value)
      
      meta %<>% 
        filter(header) %>% 
        mutate(
          title = str_extract(title, "(?<=# ).*")
        ) %>% 
        separate_wider_delim(
          title, 
          delim = ":", 
          names_sep ="_",
          too_few = "align_start"
        ) %>% 
        mutate(
          across(everything(), \(x) str_squish(x)),
          title_1 = to_snake_case(title_1)
        ) %>% 
        filter(
          title_1 %in% c(
            "grdc_no", "river", "station", "latitude_dd", "longitude_dd", 
            "catchment_area_km", "altitude_m_asl", "data_set_content",
            "unit_of_measure"
          )
        ) %>% 
        select(var = title_1, value = title_2) %>% 
        pivot_wider(
          names_from = var,
          values_from = value
        ) %>% 
        rename(
          station_id = grdc_no,
          latitude = latitude_dd,
          longitude = longitude_dd,
        )
      
      data %<>% 
        cross_join(meta) %>% 
        mutate(
          across(
            .cols = c(latitude, longitude, catchment_area_km, altitude_m_asl),
            \(x) {
              y <- as.numeric(x) %>% 
                if_else2(. == -999, NA_real_, .)
              y
            }
          )
        )
      
      data
    }
  ) %>% 
  list_rbind()

chk_all_identical(discharge$unit_of_measure) # all m^3/s
chk_all_identical(discharge$data_set_content) # all mean daily discharge
chk_all_identical(discharge$time) # all "--:--" (null)

discharge %<>% 
  filter(date %in% seq(bounding_dates[1], bounding_dates[2], 1)) %>% 
  select(
    date, value, station_id, river, station, latitude, longitude, 
    catchment_area_km, altitude_m_asl
  )

sbf_set_sub("clean", "discharge")
sbf_save_datas()

if (FALSE) {
  sbf_compare_data_archive()
}