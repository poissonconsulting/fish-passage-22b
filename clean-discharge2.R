source("header.R")

sbf_set_sub("read", "discharge2")
sbf_load_datas()

runoff %<>% 
  mutate(date = as_date(time, origin = origin)) %>% 
  select(
    runoff = RUNOFF,
    longitude = lon,
    latitude = lat,
    date
  )

baseflow %<>% 
  mutate(date = as_date(time, origin = origin)) %>% 
  select(
    baseflow = BASEFLOW,
    longitude = lon,
    latitude = lat,
    date
  )

discharge <-
  full_join(runoff, baseflow, join_by(longitude, latitude, date)) %>% 
  mutate(
    grid_length = sqrt(3e7), # 1/16 degree grid area = ~30 km^2 at latitudes covered = 3e7 m^2
    # this makes big assumptions about the shape of the river! 
      # straight through the grid with a channel width of 1m?
    discharge = ((runoff + baseflow) / 1000), # in metres
    discharge = discharge * grid_length
    # but is this over the whole area or a constant rate within?
  ) %>% 
  select(latitude, longitude, date, discharge)

rm(runoff, baseflow)

sbf_set_sub("clean", "discharge2", rm = TRUE)
sbf_save_datas()
