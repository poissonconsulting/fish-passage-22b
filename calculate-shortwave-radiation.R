source("header.R")

sites <- sbf_load_data("water_temp_site", sub = "query")
distances <- sbf_load_object("flow_connected", sub = "distance")
water_temp <- sbf_load_data("water_temp", sub = "query")

# dem_path <- "output/objects/ssn/elev.tif"
# rast <- raster::raster(dem_path)
# raster::crs(rast) <- crs
# 
# slope_aspect <- raster::terrain(
#   rast, 
#   opt = c("slope", "aspect"), 
#   unit = "degrees",
#   neighbors = 4
# )

sites %<>% 
  filter(site %in% rownames(distances)) %>% 
  ps_sfc_to_longlat() %>%
  rename_all(snakecase::to_snake_case) %>% 
  select(site, longitude, latitude, elev)

# slopes <- raster::rasterToPoints(slope_aspect$slope, spatial = FALSE)
# 
# slopes2 <- 
#   slopes %>% 
#   as_tibble() %>% 
#   ps_coords_to_sfc(coords = c("x", "y"), crs = 4326)
# 
# aspects <- raster::rasterToPoints(slope_aspect$aspect, spatial = FALSE)
# 
# sites %>% 
#   rename(Latitude = latitude, Longitude = longitude) %>% 
#   ps_longlat_to_sfc()
# 
# slopes2 <- 
#   raster::extract(
#     slope_aspect$slope, 
#     SpatialPoints(
#       cbind(sites$longitude[1], sites$latitude[1]), 
#       proj4string = raster::crs(rast)
#     )
#   )
# 
# aspects2 <- 
#   raster::extract(
#     slope_aspect$aspect, 
#     SpatialPoints(
#       cbind(sites$longitude, sites$latitude), 
#       proj4string = raster::crs(rast)
#     )
#   )
# 
# sps <- SpatialPoints(
#   cbind(sites$longitude, sites$latitude), 
#   proj4string = raster::crs(rast)
# )
# 
# plot(sps)
# 
# x <- -124.7431
# y <- 53.32150
# tibble(Latitude = y, Longitude = x) %>% 
#   ps_longlat_to_sfc() %>%
#   mapview()
# 
# 
# sp1 <- SpatialPoints(cbind(y, x), proj4string = raster::crs(rast))
# # plot(slope_aspect$slope)
# # points(sp1, col = "black", cex = 12)
# raster::extract(slope_aspect$slope, sp1, buffer = 1000)
# 
# ## Try terra
# rast3 <- terra::rast(dem_path)
# slope3 <- terra::terrain(rast3, v = "slope", unit = "degrees", neighbors = 8)
# plot(slope3)
# aspect3 <- terra::terrain(rast3, v = "aspect", unit = "degrees", neighbors = 8)
# plot(aspect3)
# 
# ll_mat <- as.matrix(sites[, c("longitude", "latitude")])
# ll_df <- data.frame(lon = sites$longitude, lat = sites$latitude)
# points <- vect(ll_mat, crs = terra::crs(rast3))
# terra::extract(slope3, ll_df)
# terra::extract(aspect3, ll_df)
# 
# slope_aspect3 <- terra::terrain(rast3, v = c("slope", "aspect"), unit = "degrees", neighbors = 8)
# 
# slope_aspect_points <- terra::as.points(slope_aspect3, na.rm = TRUE)
# 

# water_temp %>% 
#   left_join(sites, by = "site") %>% 
#   drop_units() %>% 
#   expand(nesting(site, longitude, latitude, elev), date) %>% 
#   rowwise() %>% 
#   group_split() %>% 
#   map(
#     .f = \(x) {
#       browser()
#       shortwave <- solrad::Solar(
#         DOY = dtt_doy(x$date), 
#         Lat = x$latitude, 
#         Lon = x$longitude, 
#         SLon = x$longitude, ##? 
#         DS = 0,
#         Elevation = x$elev,
#         Slope = 0,
#         Aspect = 0
#       )
#       x$s_extr <- shortwave$Sextr # Extraterrestrial solar radiation
#       x$s_open <- shortwave$Sopen 
#       x$s_diropen <- shortwave$sdiropen # Open sky direct solar radiation
#       x$s_difopen <- shortwave$sdifopen # Diffues shortwave radiation
#     }
#   ) %>% 
#   bind_rows()
date_ref <-
  tibble(date = seq(min(water_temp$date), max(water_temp$date), by = "day")) %>% 
  mutate(
    annual = dtt_year(date),
    day = as.integer(date),
    day = day - min(day),
    week = as.integer(day %/% 7 + 1),
    doy = dtt_doy(date)
  ) %>% 
  group_by(week) %>% 
  arrange(annual, doy, .by_group = TRUE) %>% 
  summarize(
    date = first(date),
    annual = first(annual), 
    doy = first(doy), 
    .groups = "drop"
  ) %>% 
  rename(date_start = date) %>% 
  mutate(
    date_end = date_start + 6
  ) %>% 
  relocate(date_end, .after = date_start)

dates <- 
  water_temp %>% 
  arrange(date) %>% 
  filter(row_number() == 1 | row_number() == n()) %>% 
  select(date) %>% 
  mutate(
    date = if_else2(row_number() == n(), date + 1, date),
    date_time = dtt_date_time(date, dtt_time("00:00:00"))
  ) %>% 
  expand(date_time = seq(min(date_time), max(date_time), by = "min")) %>% 
  filter(row_number() != n()) %>% 
  left_join(date_ref, join_by(between(date_time, date_start, date_end))) %>% 
  select(date_time, week)

weekly_rad <- map(
  .x = sites %>% 
    rowwise() %>% 
    group_split(),
  .f = \(x) {
    y <- 
      dates %>%
      mutate(
        shortwave = solrad::OpenRadiation(
          DOY = solrad::DayOfYear(date_time),
          Lat = x$latitude,
          Lon = x$longitude,
          SLon = (-8 * 15),
          DS = 0,
          Elevation = drop_units(x$elev)
        ),
        date = dtt_date(date_time)
      ) %>% 
      # group_by(week, date) %>% 
      # summarize(
      #   daily_shortwave = sum(shortwave, na.rm = TRUE),
      #   .groups = "drop"
      # ) %>% 
      group_by(week) %>% 
      summarize(
        shortwave = mean(shortwave, na.rm = TRUE),
        .groups = "drop"
      ) %>% 
      mutate(
        site = x$site,
        elev = x$elev,
        longitude = x$longitude,
        latitude = x$latitude
      )
    y
  }
)
  
radiation <- bind_rows(weekly_rad)

sbf_set_sub("radiation")
sbf_save_data(radiation)
