source("header.R")

message("compares estimated discharge to true discharge")

path <- file.path(dir, "Data/Discharge/GRDC text")
files <- list.files(path, full.names = TRUE) %>% 
  as_list()

discharge <- 
  map(
    .x = files, 
    .f = \(x) {
      read_csv(x, id = "file")
    }
  ) %>% 
  list_rbind()

rm(path, files)

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

# Filter to sites in the nechako watershed
discharge %<>%
  filter(longitude < -122.92 & longitude > -127.53 & 
           latitude > 53.32 & latitude < 54.89)

discharge_site <- 
  discharge %>% 
  mutate(
    altitude = set_units(altitude_m_asl, "m")
  ) %>% 
  select(
    station_id, river, station, latitude, longitude, altitude
  ) %>% 
  distinct()

discharge %<>% 
  group_by(station_id) %>% 
  mutate(missing = if_else2(any(is.na(value)), TRUE, FALSE)) %>% 
  ungroup() %>% 
  filter(!missing) %>% 
  select(station_id, date, mean_discharge = value) %>% 
  mutate(
    mean_discharge = set_units(mean_discharge, "m^3/s")
  )

# Are there any sites < 100 m apart?
water_temp_site <- sbf_load_data("water_temp_site", sub = "query")
discharge_est <- sbf_load_data("discharge", sub = "clean/discharge")

discharge_site %<>% 
  rename(Latitude = latitude, Longitude = longitude) %>% 
  ps_longlat_to_sfc() %>% 
  st_transform(crs = st_crs(water_temp_site))

max_dist <- 500
  
sites_within_max_dist <- 
  st_join(
    discharge_site,
    water_temp_site,
    join = st_is_within_distance,
    dist = max_dist
  ) %>% 
  filter(!is.na(site)) %>% 
  select(station_id, site)

# Comparison sites
mapview(water_temp_site %>% filter(site %in% sites_within_max_dist$site) , col.regions = "red") + 
  mapview(discharge_site %>% filter(station_id %in% sites_within_max_dist$station_id))

comparing_discharge <- 
  discharge_est %>% 
  right_join(sites_within_max_dist, join_by(site)) %>% 
  left_join(discharge, join_by(station_id, date))

gp <- ggplot(comparing_discharge) +
  geom_abline(aes(intercept = 0, slope = 1)) + 
  geom_point(aes(x = mean_discharge, y = discharge)) +
  xlab("Observed Discharge") +
  ylab("Predicted Discharge") +
  NULL

sbf_open_window()
sbf_print(gp)
