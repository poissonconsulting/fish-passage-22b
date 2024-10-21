source("header.R")

water_temp_site <- sbf_load_data("water_temp_site", sub = "query")

# Load in channel width data from fissr explore
channel_width <- read_csv("~/Poisson/Data/fissr-explore/2022-01-27/fiss_density_pts_channel_width.csv")

channel_width %<>% 
  # CRS is BC Albers for the current analysis too
  ps_coords_to_sfc(coords = c("bcalbers_x", "bcalbers_y"), crs = crs) %>% 
  select(
    cw_id = fiss_density_distinct_id,
    channel_width, 
    cw_modelled_21, 
    cw_modelled_21b,# go with this one?
    geometry
  )

# Only a few overlap - going to need to use model?
mapview(channel_width) + mapview(water_temp_site, col.regions = "red")

water_temp_site %<>% 
  st_transform(crs = crs)

st_join(
  water_temp_site,
  channel_width,
  join = st_is_within_distance,
) %>% 
  print(n = nrow(.))