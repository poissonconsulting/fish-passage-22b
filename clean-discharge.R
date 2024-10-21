source("header.R")

sbf_set_sub("read", "discharge")
sbf_load_datas()

# Calculate discharge
# From Simon's logic
# https://github.com/smnorris/bcfishpass/blob/e87ec79deb8027cf9f196be78c1d63e1527f1238/model/02_habitat_linear/discharge/sql/discharge03_wsd.sql

water_temp_site <- sbf_load_data("water_temp_site", sub = "tidy/water-temp")

runoff %<>%
  mutate(
    date = dtt_date_time(time, tz = "UTC"),
    date = dtt_adjust_tz(date, tz = tz_analysis),
    date = dtt_date(date),
    across(c(lat, lon), as.numeric)
  ) %>%
  select(
    runoff = RUNOFF,
    longitude = lon,
    latitude = lat,
    date
  )

baseflow %<>%
  mutate(
    date = dtt_date_time(time, tz = "UTC"),
    date = dtt_adjust_tz(date, tz = tz_analysis),
    date = dtt_date(date),
    across(c(lat, lon), as.numeric)
  ) %>%
  select(
    baseflow = BASEFLOW,
    longitude = lon,
    latitude = lat,
    date
  )

discharge <-
  full_join(runoff, baseflow, join_by(longitude, latitude, date)) %>%
  mutate(
    discharge = runoff + baseflow
  ) %>%
  rename(Latitude = latitude, Longitude = longitude) %>%
  ps_longlat_to_sfc() %>%
  st_transform(crs = st_crs(water_temp_site))

chk_true(st_crs(water_temp_site) == st_crs(discharge))

# Calculate weighted average of discharge for all area upstream of site.
# Get watersheds
water_temp_site %<>%
  filter(site != "SLS") %>% # Was removed from stream network
  ps_sfc_to_longlat() %>%
  rename(lat = Latitude, lon = Longitude) %>%
  fwa_add_blk_to_lon_lat() %>%
  as_tibble() %>%
  select(-geometry) %>%
  rowwise() %>%
  group_split() %>%
  map(
    .f = function(x) fwa_add_watershed_to_blk(x)
  ) %>%
  list_rbind() %>%
  ps_activate_sfc() %>%
  st_transform(crs = st_crs(discharge)) %>%
  mutate(
    geometry = st_make_valid(geometry),
    area = st_area(geometry)
  )

chk_true(st_crs(water_temp_site) == st_crs(discharge))

# Get all discharge values in the upstream watershed for each site!
upstream_discharge <-
  water_temp_site %>%
  group_by(site) %>%
  group_split() %>%
  map(
    .f = function(x) st_intersection(discharge, x)
  ) %>%
  list_rbind()
  
discharge <-
  upstream_discharge %>% 
  group_by(site) %>% 
  mutate(
    us_grid_area = set_units(n() * 30000, "m^2"),
    discharge = set_units(discharge * 30000, "m^3"),
  ) %>% 
  group_by(site, area, us_grid_area, date) %>% 
  summarize(us_discharge = sum(discharge), .groups = "drop") %>% 
  mutate(
    weighted_discharge = us_discharge * (area / us_grid_area),
    discharge = weighted_discharge / set_units(24 * 60 * 60, "s"),
    dayte = dtt_dayte(date),
    year = factor(dtt_year(date))
  )

gp <- ggplot(discharge) +
  geom_line(aes(x = dayte, y = discharge, group = year, colour = year)) +
  scale_colour_disc_poisson() +
  facet_wrap(~site) +
  xlab("Date") +
  ylab("Discharge") +
  NULL

sbf_open_window(8)
sbf_print(gp)

rm(runoff, baseflow, upstream_discharge, water_temp_site)

sbf_set_sub("clean", "discharge", rm = TRUE)
sbf_save_datas()

if (FALSE) {
  sbf_compare_data_archive()
}
