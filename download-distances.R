source("header.R")

sbf_set_sub("tidy")
sites_long_lat <- sbf_load_data("sites_long_lat")

points <- 
  sites_long_lat %>% 
  group_by(site) %>% 
  group_split() %>% 
  map(
    .f = \(x) fwa_index_point(x$lon, x$lat, tolerance = 100) %>% # Low tolerance
      mutate(site = x$site)
  ) %>% 
  do.call(rbind, .) %>% 
  filter(!site == "SLS") # SLS site located on a spillway, not in stream network.

# Long/Lat from Wikipedia
# outlet <- tibble(
#   lat = 53.91722,
#   lon = -122.7147
# )
# 
# outlet_point <- fwa_index_point(x = outlet$lon, y = outlet$lat)
# outlet_point

# Need stream network connecting all (?) sites, with segments splitting at confluences
# Need lengths of each segment, and need to know which segments are upstream of others

# Get this programatically
stream_network_names <- as.list(c(
  "NECR",
  "CHES",
  "LCHL",
  "LNRS",
  "UNRS",
  "CHIL",
  "FRAN",
  "MIDR",
  "LTRE",
  "UTRE",
  "STUR",
  "STUL"
))
  
stream_network_list <- map(
  .x = stream_network_names,
  .f = \(x) fwa_query_collection(
    collection_id = "whse_basemapping.fwa_stream_networks_sp",
    filter = list(watershed_group_code_50k = x)
  )
)

# Filter to lines in `stream_network` that connect the points
filter_wscode_ltree <- 
  points %>%
  filter(str_detect(wscode_ltree, "100\\.567134")) %>%
  distinct(wscode_ltree) %>% 
  mutate(
    order = str_split(wscode_ltree, pattern = "\\.")
  ) %>% 
  unnest_wider(
    col = order,
    names_sep = "_"
  ) %>% 
  mutate(
    c_1 = order_1,
    c_1_2 = str_c(order_1, order_2, sep = "."),
    c_1_2_3 = str_c(order_1, order_2, order_3, sep = "."),
    c_1_2_3_4 = str_c(order_1, order_2, order_3, order_4, sep = "."),
    c_1_2_3_4_5 = str_c(order_1, order_2, order_3, order_4, order_5, sep = "."),
    c_1_2_3_4_5_6 = str_c(order_1, order_2, order_3, order_4, order_5, order_6, sep = ".")
  ) %>% 
  select(starts_with("c")) %>% 
  pivot_longer(
    cols = starts_with("c"),
    values_to = "wscode_ltree",
    names_to = NULL
  ) %>% 
  drop_na() %>% 
  distinct() %>% 
  arrange()

stream_network <- st_sf(do.call(rbind, stream_network_list)) %>% 
  filter(wscode_ltree %in% filter_wscode_ltree$wscode_ltree)

# Get finer detail
watershed_keys <- as.list(unique(stream_network$watershed_key))

stream_network_detail_list <- map(
  .x = watershed_keys,
  .f = \(x) fwa_query_collection(
    collection_id = "whse_basemapping.fwa_stream_networks_sp",
    filter = list(watershed_key = x)
  )
)

stream_network_detail <- st_sf(do.call(rbind, stream_network_detail_list))

# Union the linestrings in a given wscode_ltree.
# network_unioned <- 
#   stream_network_detail %>%
#   group_by(wscode_ltree) %>%
#   group_split(.keep = TRUE) %>% 
#   map(
#   .x = .,
#   .f = \(x) {
#     val <- st_union(x)
#     if (inherits(val, "sfc_MULTILINESTRING")) {
#       val <- st_line_merge(val)
#     }
#     val <- st_cast(val, to = "LINESTRING")
#     val <- st_as_sf(val)
#     val$wscode_ltree <- x$wscode_ltree[1]
#     val$downstream_route_measure <- min(x$downstream_route_measure)
#     val
#   }
# ) %>%
#   do.call(rbind, .)
# 
# network_unioned <- 
#   network_unioned %>%
#   group_by(wscode_ltree, downstream_route_measure) %>%
#   mutate(id = 1:n()) %>% 
#   ### Remove the long side channel on the nechako 
#   filter(!(wscode_ltree == "100.567134" & id == 2)) %>% 
#   ungroup() %>% 
#   select(-id)

### Remove the side channel of the Nechako
# stream_network_detail <- 
#   stream_network_detail %>% 
#   filter(!(wscode_ltree == "100.567134" & stream_order_parent == 7))

network_unioned <-
  stream_network_detail %>%
  mutate(
    stream_order = as.integer(stream_order),
    stream_order_max = as.integer(stream_order_max),
    stream_order_max_m1 = stream_order_max - 1
  ) %>%
  # And filter to the the max stream order or 1 minus the max stream order
  filter(stream_order %in% c(stream_order_max, stream_order_max_m1)) %>%
  # And remove "OP" feature sources that don't have a stream order
  filter(!(feature_source == "OP" & is.na(stream_order_max)))

ggplot() +
  geom_sf(data = network_unioned, aes(color = wscode_ltree)) +
  geom_sf(data = points, aes(color = "red"))

wscode_ltree_network <- 
  network_unioned %>% 
  as_tibble() %>% 
  select(wscode_ltree) %>%
  mutate(origins = 1:n())

network_intersection_points <-
  st_intersection(network_unioned) %>% 
  filter(n.overlaps > 1) %>% 
  unnest(origins) %>% 
  left_join(
    wscode_ltree_network,
    by = "origins"
  ) %>% 
  filter(wscode_ltree.x != wscode_ltree.y) %>%
  distinct(
    wscode_ltree.x, wscode_ltree.y, blue_line_key, downstream_route_measure,
    .keep_all = TRUE
  ) %>% 
  # Make wscode_ltree.x the shorter code, and wscode_ltree.y the longer one
  mutate(
    x = if_else(
      nchar(wscode_ltree.x) > nchar(wscode_ltree.y),
      wscode_ltree.y,
      wscode_ltree.x
    ),
    y = if_else(
      nchar(wscode_ltree.x) > nchar(wscode_ltree.y),
      wscode_ltree.x,
      wscode_ltree.y
    )
  ) %>%
  # keep the point on the mainstem
  group_by(x, y) %>%
  arrange(downstream_route_measure, .by_group = TRUE) %>%
  mutate(group_number = cur_group_id()) %>% 
  slice_tail() %>% 
  ungroup() %>% 
  arrange(localcode_ltree) %>%
  mutate(id = 1:n())

ggplot() +
  geom_sf(data = network_unioned, aes(color = wscode_ltree)) +
  geom_sf(data = network_intersection_points) +
  NULL

# Rn the network still has the secondary paths; planning on using river metres for distances so it shouldn't matter.

# Remove the secondary lines to get the segments as single lines
network_unioned <-
  stream_network_detail %>%
  # Get rid of secondary flow paths
  filter(!(edge_type %in% c(1100, 1150, 1300, 1350))) %>%
  mutate(
    stream_order = as.integer(stream_order),
    stream_order_max = as.integer(stream_order_max),
    stream_order_max_m1 = stream_order_max - 1
  ) %>%
  # And filter to the the max stream order or 1 minus the max stream order
  filter(stream_order %in% c(stream_order_max, stream_order_max_m1)) %>%
  # And remove "OP" feature sources that don't have a stream order
  filter(!(feature_source == "OP" & is.na(stream_order_max)))

# Add back the secondary flow path that connects the Nechako to the Chilako 
# (first intersection point)
nechako_chilako_intersection <-
  stream_network_detail %>% 
  filter(wscode_ltree == "100.567134") %>% 
  mutate(distance = st_distance(., network_intersection_points[1, ])) %>% 
  drop_units() %>% 
  filter(distance < 400) %>% 
  filter(edge_type != 1250) %>%
  arrange(downstream_route_measure) %>%
  slice_head(n = 2) %>% 
  mutate(
    stream_order_max = as.integer(stream_order_max),
    wscode_ltree = "100.567134.069486",
    localcode_ltree = NA_character_
  )

network_unioned %<>% 
  bind_rows(
    nechako_chilako_intersection
  ) %>% 
  arrange(localcode_ltree, downstream_route_measure)

wscode_ltree_network <- 
  network_unioned %>% 
  as_tibble() %>% 
  select(wscode_ltree) %>%
  mutate(origins = 1:n())

network_intersection_points <-
  st_intersection(network_unioned) %>% 
  # filter(n.overlaps > 1) %>% 
  unnest(origins) %>% 
  left_join(
    wscode_ltree_network,
    by = "origins"
  ) %>% 
  relocate(upstream_route_measure, .after = downstream_route_measure) %>% 
  filter(wscode_ltree.x != wscode_ltree.y) %>%
  distinct(
    wscode_ltree.x, wscode_ltree.y, blue_line_key, downstream_route_measure,
    .keep_all = TRUE
  ) %>% 
  # Make wscode_ltree.x the shorter code, and wscode_ltree.y the longer one
  mutate(
    x = if_else(
      nchar(wscode_ltree.x) > nchar(wscode_ltree.y),
      wscode_ltree.y,
      wscode_ltree.x
    ),
    y = if_else(
      nchar(wscode_ltree.x) > nchar(wscode_ltree.y),
      wscode_ltree.x,
      wscode_ltree.y
    )
  ) %>%
  # keep the point on the mainstem
  group_by(x, y) %>%
  arrange(downstream_route_measure, .by_group = TRUE) %>%
  mutate(group_number = cur_group_id()) %>% 
  slice_tail() %>% 
  ungroup() %>% 
  arrange(localcode_ltree) %>%
  mutate(id = 1:n())

ggplot() +
  geom_sf(data = network_unioned, aes(colour = wscode_ltree)) +
  geom_sf(data = network_intersection_points, size = 0.3) +
  geom_sf(data = points, colour = "red", size = 0.3) +
  NULL

segments_mainstems <- 
  network_intersection_points %>% 
  arrange(x) %>% 
  group_by(x) %>% 
  group_split() %>% 
  map(
    .f = \(x) {
      y <- network_unioned %>% 
        filter(wscode_ltree == x$x[1]) %>%
        arrange(downstream_route_measure) %>% 
        mutate(
          split_point = if_else2(
            downstream_route_measure %in% x$downstream_route_measure,
            TRUE,
            FALSE
          ),
          segments = cumsum(split_point) + 1,
          segment_id = paste0(x$x[1], ".0000S", segments)
      ) 
      y
    }
  ) %>% 
  do.call(rbind, .)


all_segments <- 
  segments_mainstems %>% 
  bind_rows(
    network_unioned %>% 
      filter(!(wscode_ltree %in% unique(segments_mainstems$wscode_ltree))) %>% 
      group_by(wscode_ltree) %>%
      mutate(
        segments = 1,
        segment_id = paste0(wscode_ltree, ".0000S", segments)
      )
  )

segments <- 
  all_segments %>% 
  group_by(segment_id) %>% 
  arrange(downstream_route_measure, .by_group = TRUE) %>%
  group_split() %>% 
  map(
    .x = ., 
    .f = \(x) {
      min_rm <- min(x$downstream_route_measure)
      max_rm <- max(x$upstream_route_measure)
      seg_id <- x$segment_id[1]
      wscode_ltree <- x$wscode_ltree[1]
      localcode_ltree <- tail(x$localcode_ltree, 1)
      x <- st_zm(x)
      x <- st_union(x)
      if (st_is(x, "MULTILINESTRING")) {
        x <- st_line_merge(x)
      }
      x <- st_cast(x, "LINESTRING")
      x <- st_as_sf(x)
      x$downstream_route_measure <- min_rm
      x$upstream_route_measure <- max_rm
      x$segment_id <- seg_id
      x$wscode_ltree <- wscode_ltree
      x$localcode_ltree <- localcode_ltree
      x
    }
  ) %>% 
  do.call(rbind, .) %>% 
  arrange(segment_id)

ggplot() +
  geom_sf(data = segments, aes(colour = fct_shuffle(segment_id))) +
  geom_sf(data = network_intersection_points, size = 0.3) +
  geom_sf(data = points, colour = "red", size = 0.3) +
  NULL

# Copy Simon's logic from 
# https://github.com/smnorris/fwapg/blob/main/sql/functions/FWA_Upstream.sql
fwa_upstream <- function(a, b) {
  ws_a <- a$wscode_ltree
  ws_b <- b$wscode_ltree
  lc_a <- a$localcode_ltree
  lc_b <- b$localcode_ltree
  
  bol_1 <- ws_a == lc_a & str_detect(ws_b, ws_a) & str_detect(lc_b, lc_a)
  bol_2 <- ws_a != lc_a & str_detect(ws_b, ws_a)
  bol_3 <- ws_b > lc_a & !(str_detect(ws_b, lc_a)) & str_detect(ws_b, ws_a) & lc_b > lc_a
  bol_4 <- ws_b == ws_a & lc_b >= lc_a

  upstream <- bol_1 | (bol_2 & (bol_3 | bol_4))
  upstream
}

# Likewise for downstream function
# https://github.com/smnorris/fwapg/blob/main/sql/functions/FWA_Downstream.sql
fwa_downstream <- function(a, b) {
  ws_a <- a$wscode_ltree
  ws_b <- b$wscode_ltree
  lc_a <- a$localcode_ltree
  lc_b <- b$localcode_ltree
  
  bol_1 <- str_detect(ws_a, ws_b)
  # Local code of a is bigger than local code of b at given level.
  nlevels_b <- length(str_split(lc_b, "\\.")[[1]])
  pattern <- str_split(lc_a, "\\.")[[1]][1:nlevels_b]
  pattern <- pattern[!is.na(pattern)] %>% 
    paste0(collapse = ".")
  bol_2 <- pattern > lc_b
  bol_3 <- ws_b == lc_b & ws_a != ws_b
  
  downstream <- bol_1 & (bol_2 | bol_3)
  downstream
}

# Split up network by the sites
nsites <- nrow(points)
# Determine if sites are flow connected
connectivity <- 
  expand_grid(base = points$site, compare = points$site) %>% 
  rowwise() %>% 
  group_split() %>%
  map(
    .f = \(x) {
      x$upstream <- fwa_upstream(points[points$site == x$base, ], points[points$site == x$compare, ])
      x$downstream <- fwa_downstream(points[points$site == x$base, ], points[points$site == x$compare, ])
      x
    }
  ) %>% 
  list_rbind()

upstream_segments <- 
  connectivity %>% 
  group_by(base) %>% 
  filter(upstream) %>% 
  do(segments = as.vector(t(.$compare))) %>% 
  ungroup() %>% 
  rename(segment_id = base)

connectivity %<>% 
  left_join(upstream_segments, by = c("base" = "segment_id")) %>%
  rename(segments_base = segments) %>% 
  left_join(upstream_segments, by = c("compare" = "segment_id")) %>%
  rename(segments_compare = segments) %>% 
  rowwise() %>% 
  mutate(
    intersecting_segments = length(intersect(segments_base, segments_compare)),
    flow_connected = if_else(intersecting_segments != 0, TRUE, FALSE),
    flow_unconnected = if_else(intersecting_segments == 0, TRUE, FALSE)
  ) %>% 
  ungroup() %>% 
  select(base, compare, upstream, downstream, flow_connected, flow_unconnected)

# Flow connected = upstream or downstream
flow_connected <- matrix(NA, nrow = nsites, ncol = nsites)
rownames(flow_connected) <- points$site
colnames(flow_connected) <- points$site

for (i in 1:nsites) {
  for (j in 1:nsites) {
    flow_connected[i, j] <- connectivity$flow_connected[connectivity$base == points$site[i] & connectivity$compare == points$site[j]]
  }
}

# Downstream
downstream <- matrix(NA, nrow = nsites, ncol = nsites)
rownames(downstream) <- points$site
colnames(downstream) <- points$site

for (i in 1:nsites) {
  for (j in 1:nsites) {
    downstream[i, j] <- connectivity$downstream[connectivity$base == points$site[i] & connectivity$compare == points$site[j]]
  }
}

# id <- 1
# ggplot() +
#   geom_sf(data = network_unioned, aes(color = wscode_ltree)) +
#   geom_sf(data = points %>% 
#             left_join(
#               tibble(
#                 site = rownames(flow_unconnected),
#                 flow_unconnected = flow_unconnected[, id]
#               ), 
#               join_by(site)
#             ),
#           aes(shape = flow_unconnected)
#           ) +
#   geom_sf(data = points %>% filter(site == rownames(flow_unconnected)[id]), aes(color = "red")) +
#   NULL

## Run segments code
# Get all segments downstream of a site, not including that site

### Connectivity - points/segments
connectivity_ps <- 
  expand_grid(base = points$site, compare = segments$segment_id) %>% 
  rowwise() %>% 
  group_split() %>%
  map(
    .f = \(x) {
      x$upstream <- fwa_upstream(points[points$site == x$base, ], segments[segments$segment_id == x$compare, ])
      x$downstream <- fwa_downstream(points[points$site == x$base, ], segments[segments$segment_id == x$compare, ])
      x
    }
  ) %>% 
  list_rbind() %>% 
  left_join(
    segments %>% 
      select(segment_id, upstream_route_measure, downstream_route_measure),
    by = c("compare" = "segment_id")
  )


ggplot() +
  geom_sf(data = segments, aes(colour = fct_shuffle(segment_id))) +
  geom_sf(data = network_intersection_points, size = 0.3) +
  geom_sf(data = points, colour = "red", size = 0.3) +
  NULL


i <- 7
connectivity_ps %>% 
  filter(base == rownames(flow_unconnected)[i]) %>% 
  filter(upstream) %>% 
  ps_activate_sfc(sfc_name = "x") %>% 
  ggplot() +
  geom_sf(data = segments, aes(color = segment_id)) +
  geom_sf(data = network_intersection_points, size = 0.3) +
  geom_sf() +
  geom_sf(data = points %>% filter(site == rownames(flow_unconnected)[i]), aes(colour = "red"), size = 0.1) + 
  NULL

# upstream_segments <- 
#   connectivity %>% 
#   group_by(base) %>% 
#   filter(upstream) %>% 
#   do(segments = as.vector(t(.$compare))) %>% 
#   ungroup() %>% 
#   rename(segment_id = base)
# 
# connectivity %<>% 
#   left_join(upstream_segments, by = c("base" = "segment_id")) %>%
#   rename(segments_base = segments) %>% 
#   left_join(upstream_segments, by = c("compare" = "segment_id")) %>%
#   rename(segments_compare = segments) %>% 
#   rowwise() %>% 
#   mutate(
#     intersecting_segments = length(intersect(segments_base, segments_compare)),
#     flow_connected = if_else(intersecting_segments != 0, TRUE, FALSE),
#     flow_unconnected = if_else(intersecting_segments == 0, TRUE, FALSE)
#   ) %>% 
#   ungroup() %>% 
#   select(base, compare, upstream, downstream, flow_connected, flow_unconnected)


downstream_hydrologic_distance <- matrix(NA, nsites, nsites)
diag(downstream_hydrologic_distance) <- 0
rownames(downstream_hydrologic_distance) <- points$site
colnames(downstream_hydrologic_distance) <- points$site

### TODO: Make some tests for this to test the logic.

for (base in 1:nsites) {
  for (compare in 1:nsites) {
    # If i is downstream of j
    if (downstream[compare, base]) {
      # If they have the same wscode_ltree, subtract rm upstream from rm downstream
      # E.g. base = NMI (12), compare = NCC (11)
      if (points$wscode_ltree[compare] == points$wscode_ltree[base]) {
        downstream_hydrologic_distance[compare, base] <- 
          points$downstream_route_measure[compare] - points$downstream_route_measure[base]
      } else {
        # If they don't have the same wscode_ltree, 
        # Add downstream rm of base
        # Add upstream rm of all segments downstream of compare and upstream of base 
        # Subtract downstream rm of compare
        compare_segment <- rownames(flow_connected)[compare]
        base_segment <- rownames(flow_connected)[base]
        
        downstream_of_compare <- 
          connectivity_ps %>% 
          filter(base == compare_segment) %>%
          filter(downstream) %>% 
          select(segment_id = compare, upstream_route_measure) 
        
        upstream_of_base <- 
          connectivity_ps %>% 
          filter(base == base_segment) %>% 
          filter(upstream) %>% 
          select(segment_id = compare, upstream_route_measure)
        
        intermediate_segments <- 
          inner_join(
            downstream_of_compare,
            upstream_of_base,
            join_by(segment_id, upstream_route_measure)
          )
        
        downstream_hydrologic_distance[compare, base] <- 
          points$downstream_route_measure[compare] + 
          sum(intermediate_segments$upstream_route_measure) -
          points$downstream_route_measure[base]
      } 
    } else if (flow_connected[compare, base]) {
      downstream_hydrologic_distance[compare, base] <- 0
    } else {
      # Distance to common confluence (network intersection point) 
      # E.g. CSC (2) and CNR (1)
      compare_segment <- rownames(flow_connected)[compare]
      base_segment <- rownames(flow_connected)[base]
      
      downstream_of_base <- 
        connectivity_ps %>% 
        filter(base == base_segment) %>% 
        filter(downstream) %>% 
        select(segment_id = compare, downstream_route_measure, upstream_route_measure)
      
      downstream_of_compare <- 
        connectivity_ps %>% 
        filter(base == compare_segment) %>% 
        filter(downstream) %>% 
        select(segment_id = compare, downstream_route_measure, upstream_route_measure)
      
      downstream_dist_to_cc <- 
        downstream_of_compare %>% 
        anti_join(downstream_of_base, join_by(segment_id, upstream_route_measure)) %>% 
        mutate(
          segment_length = upstream_route_measure - downstream_route_measure
        )
      
      downstream_hydrologic_distance[compare, base] <- 
        points$downstream_route_measure[compare] + 
        sum(downstream_dist_to_cc$segment_length)
    }
  }
}

total_hydrologic_distance <- 
  downstream_hydrologic_distance + t(downstream_hydrologic_distance)


# n_segments <- 
#   segments %>%
#   distinct(segment_id) %>% 
#   nrow()
# 
# segment_ids <-
#   segments %>% 
#   as_tibble() %>% 
#   distinct(segment_id) %>% 
#   arrange(segment_id) %>% 
#   distinct(segment_id)
# 
# connectivity <- 
#   expand_grid(base = segment_ids$segment_id, compare = segment_ids$segment_id) %>% 
#   # mutate(row_id = 1:n()) %>% 
#   # group_by(row_id) %>% 
#   rowwise() %>% 
#   group_split() %>%
#   map(
#     .x = .,
#     .f = \(x) {
#       x$upstream <- fwa_upstream(segments[segments$segment_id == x$base, ], segments[segments$segment_id == x$compare, ])
#       x$downstream <- fwa_downstream(segments[segments$segment_id == x$base, ], segments[segments$segment_id == x$compare, ])
#       x
#     }
#   ) %>% 
#   list_rbind()
# 
# upstream_segments <- 
#   connectivity %>% 
#     group_by(base) %>% 
#     filter(upstream) %>% 
#     do(segments = as.vector(t(.$compare))) %>% 
#     ungroup() %>% 
#     rename(segment_id = base)
# 
# connectivity %<>% 
#   left_join(upstream_segments, by = c("base" = "segment_id")) %>%
#   rename(segments_base = segments) %>% 
#   left_join(upstream_segments, by = c("compare" = "segment_id")) %>%
#   rename(segments_compare = segments) %>% 
#   rowwise() %>% 
#   mutate(
#     intersecting_segments = length(intersect(segments_base, segments_compare)),
#     flow_connected = if_else(intersecting_segments != 0, TRUE, FALSE),
#     flow_unconnected = if_else(intersecting_segments == 0, TRUE, FALSE)
#   ) %>% 
#   ungroup() %>% 
#   select(base, compare, upstream, downstream, flow_connected, flow_unconnected)
# 
# chosen_seg_id <- segment_ids$segment_id[12]
# segments %>% 
#   left_join(
#     connectivity %>% 
#       filter(base == chosen_seg_id),
#     by = c("segment_id" = "compare")
#   ) %>% 
#   ggplot() +
#   geom_sf(aes(colour = flow_unconnected)) +
#   geom_sf(data = segments %>% filter(segment_id == chosen_seg_id))
# 
#   
# # This matrix is symmetric.
# flow_unconnected <- matrix(NA, nrow = n_segments, ncol = n_segments)
# rownames(flow_unconnected) <- segment_ids$segment_id
# colnames(flow_unconnected) <- segment_ids$segment_id
# 
# for (i in 1:n_segments) {
#   for (j in 1:n_segments) {
#     flow_unconnected[i, j] <- connectivity$flow_unconnected[connectivity$base == segment_ids$segment_id[i] & connectivity$compare == segment_ids$segment_id[j]]
#   }
# }
# 
# 
# ggplot() +
#   # geom_sf(data = network_intersection_points) +
#   geom_sf_label(data = network_intersection_points, aes(label = id)) +
#   geom_sf(data = segments, aes(colour = segment_id))
# 
# segments


# Maybe this could look like:
  # ~ Segment, ~ Upstream Segment(s), ~ Downstream Segment(s), ~ Segment length (km)

## Also need a measure of discharge/upstream watershed area for each segment for weighting matrix

# Downstream distance matrix
  # 0 along diagonal
  # total distance if row is downstream of column
  # 0 if column is upstream of row
  # distance downstream to common confluence if 2 sites are flow unconnected.
downstream_distance <- 
  matrix(
    c(0, 3, 0, 0,
      7, 0, 0, 0,
      12, 8, 0, 0,
      18, 14, 6, 0),
    byrow = TRUE, 
    nrow = 4, 
    ncol = 4
  )

# Total hydrologic distance (for TD model)
total_distance <- downstream_distance + t(downstream_distance)

# For weight matrix calculation
flow_unconnected <- matrix(NA, nrow = dim(total_distance)[1], ncol = dim(total_distance)[2])

for (i in 1:nrow(mat)) {
  for (j in 1:ncol(mat)) {
    flow_unconnected[i, j] <- !((mat[i, j] > 0) & (mat[j, i] > 0))
  }
}


# Cumulative watershed area
sites <- tibble(
  site_id = c("S1", "S2", "S3", "S4"),
  seg_id = c("R1", "R2", "R3", "R5")
)

segments <- tibble(
  seg_id = c("R1", "R2", "R3", "R4", "R5"),
  outlet = c(FALSE, FALSE, FALSE, FALSE, TRUE),
  cumulative_watershed_area = c(50, 35, 115, 20, 160),
  direct_upstream_segments = list(
    c("R1", "R2"),
    c("R1", "R2"),
    c("R3", "R4"),
    c("R3", "R4"),
    NULL
  ),
  total_downstream_segments = list(
    c("R1", "R3", "R5"),
    c("R2", "R3", "R5"),
    c("R3", "R5"),
    c("R4", "R5"),
    c("R5")
  )
) %>% 
  mutate(
    upstream_watershed_area = map(
        .x = .$direct_upstream_segments,
        .f = \(x) 
          sum(.$cumulative_watershed_area[which(.$seg_id %in% x)])
      ) %>% 
      unlist(),
    segment_pi = if_else(
      outlet,
      1,
      cumulative_watershed_area / upstream_watershed_area
    )
  ) %>% 
  mutate(
    segment_afv = map(
      .x = .$total_downstream_segments,
      .f = \(x) 
      prod(.$segment_pi[which(.$seg_id %in% x)])
    ) %>% 
    unlist()
  )

sites <- 
  sites %>% 
  left_join(select(segments, seg_id, segment_afv), join_by(seg_id)) %>% 
  rename(site_afv = segment_afv)
## This will need sorting by from "upstream" to "downstream" for matrix calculation below
# Order of sites at flow-unconnected locations doesn't matter (see how this expands though)

nsites <- nrow(sites)

weight_matrix <- matrix(NA, nsites, nsites)

for (i in 1:nsites) {
  for (j in 1:nsites) {
    if (i <= j) 
      weight_matrix[i, j] <- sqrt(sites$site_afv[i] / sites$site_afv[j])
      weight_matrix[j, i] <- weight_matrix[i, j]
  }
}

weight_matrix <- weight_matrix * flow_unconnected # Element-wise multiplication
