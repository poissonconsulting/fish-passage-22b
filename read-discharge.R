source("header.R")

baseflow_path <- file.path(dir, "Data/Discharge/pcic/baseflow.nc")

ncin <- nc_open(baseflow_path)
origin <- ncin$dim$time$units # Days since 1945-01-01 00:00:00
nc_close(ncin)
origin <- str_extract(origin, "(?<=days since ).*")

baseflow <- tidync(baseflow_path) %>%
  hyper_tibble()

baseflow %<>% # units: mm
  mutate(origin = origin)

runoff_path <- file.path(dir, "Data/Discharge/pcic/runoff.nc")

runoff <- tidync(runoff_path) %>%
  hyper_tibble()

ncin <- nc_open(runoff_path)
origin <- ncin$dim$time$units # Days since 1945-01-01 00:00:00
nc_close(ncin)
origin <- str_extract(origin, "(?<=days since ).*")

runoff %<>% # units: mm
  mutate(origin = origin)

sbf_set_sub("read", "discharge", rm = TRUE)
sbf_save_datas()
