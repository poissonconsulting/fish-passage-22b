source("header.R")

sbf_set_sub("tidy")
sites_long_lat <- sbf_load_data("sites_long_lat")

points <- 
  sites_long_lat %>% 
  group_by(site) %>% 
  group_split() %>% 
  map(
    .f = \(x) fwa_index_point(x$Longitude, x$Latitude, tolerance = 250) %>% # Low tolerance
      mutate(site = x$site)
  ) %>% 
  do.call(rbind, .) %>% 
  filter(!site == "SLS") # SLS site located on a spillway, not in stream network.

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

network_intersection_points_all <-
  st_intersection(network_unioned) %>% 
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
  arrange(localcode_ltree) %>%
  group_by(geometry) %>% 
  arrange(downstream_route_measure, .by_group = TRUE) %>% 
  mutate(id = cur_group_id()) %>% 
  ungroup()

network_intersection_points <- 
  network_intersection_points_all %>%
  # keep the point on the mainstem
  group_by(x, y) %>%
  slice_tail() %>% 
  ungroup()

# Get stream outlet 
outlet_point <- 
  network_unioned %>% 
  slice_min(localcode_ltree) %>% 
  slice_min(downstream_route_measure) %>% 
  st_cast("POINT") %>%
  slice(1)

# Add outlet to the intersection points
network_intersection_points_all %<>% 
  bind_rows(
    outlet_point %>% 
      rename(x = wscode_ltree) %>% 
      mutate(id = max(network_intersection_points_all$id) + 1)
  )

ggplot() +
  geom_sf(data = network_unioned, aes(colour = wscode_ltree)) +
  geom_sf(data = network_intersection_points_all, size = 0.3) +
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

upper_tribs <- network_unioned %>% 
  filter(!(wscode_ltree %in% unique(segments_mainstems$wscode_ltree))) %>% 
  group_by(wscode_ltree) %>%
  mutate(
    segments = 1,
    segment_id = paste0(wscode_ltree, ".0000S", segments)
  )

all_segments <- 
  bind_rows(
    segments_mainstems,
    upper_tribs
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

segments <- 
  segments %>% 
  st_join(
    network_intersection_points_all %>% 
      select(confluence_id = id, point_drm = downstream_route_measure),
    join = st_is_within_distance,
    dist = 10
  ) %>% 
  group_by(segment_id) %>% 
  arrange(segment_id, point_drm, .by_group = TRUE) %>%
  slice_head() %>% 
  select(-point_drm)

ggplot() +
  geom_sf(data = segments, aes(colour = fct_shuffle(segment_id))) +
  geom_sf(data = network_intersection_points, size = 0.3) +
  geom_sf(data = points, colour = "red", size = 0.3) +
  NULL

# Copy Simon's logic from 
# https://github.com/smnorris/fwapg/blob/main/sql/functions/FWA_Upstream.sql
# Is b upstream of a?
fwa_upstream <- function(a, b) {
  ws_a <- a$wscode_ltree
  ws_b <- b$wscode_ltree
  lc_a <- a$localcode_ltree
  lc_b <- b$localcode_ltree
  rm_a <- a$downstream_route_measure
  rm_b <- b$downstream_route_measure
  
  bol_1 <- ws_a == lc_a & str_detect(ws_b, ws_a) & str_detect(lc_b, lc_a)
  bol_2 <- ws_a != lc_a & str_detect(ws_b, ws_a)
  bol_3 <- ws_b > lc_a & !(str_detect(ws_b, lc_a)) & str_detect(ws_b, ws_a) & lc_b > lc_a
  bol_4 <- ws_b == ws_a & lc_b >= lc_a
  bol_5 <- ws_a == ws_b & lc_a == lc_b & rm_b > rm_a

  upstream <- bol_1 | (bol_2 & (bol_3 | bol_4)) | bol_5
  upstream
}

# Likewise for downstream function
# https://github.com/smnorris/fwapg/blob/main/sql/functions/FWA_Downstream.sql
# Is b downstream of a?
fwa_downstream <- function(a, b) {
  ws_a <- a$wscode_ltree
  ws_b <- b$wscode_ltree
  lc_a <- a$localcode_ltree
  lc_b <- b$localcode_ltree
  rm_a <- a$downstream_route_measure
  rm_b <- b$downstream_route_measure
  
  bol_1 <- str_detect(ws_a, ws_b)
  # Local code of a is bigger than local code of b at given level.
  nlevels_b <- length(str_split(lc_b, "\\.")[[1]])
  pattern <- str_split(lc_a, "\\.")[[1]][1:nlevels_b]
  pattern <- pattern[!is.na(pattern)] %>% 
    paste0(collapse = ".")
  bol_2 <- pattern > lc_b
  bol_3 <- ws_b == lc_b & ws_a != ws_b
  bol_4 <-  ws_a == ws_b & lc_a == lc_b & rm_b < rm_a
  
  downstream <- (bol_1 & (bol_2 | bol_3)) | bol_4
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
    flow_connected[j, i] <- connectivity$flow_connected[connectivity$base == points$site[i] & connectivity$compare == points$site[j]]
  }
}

# Downstream
downstream <- matrix(NA, nrow = nsites, ncol = nsites)
rownames(downstream) <- points$site
colnames(downstream) <- points$site

for (i in 1:nsites) {
  for (j in 1:nsites) {
    downstream[j, i] <- connectivity$downstream[connectivity$base == points$site[i] & connectivity$compare == points$site[j]]
  }
}

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

downstream_hydrologic_distance <- matrix(NA, nsites, nsites)
diag(downstream_hydrologic_distance) <- 0
rownames(downstream_hydrologic_distance) <- points$site
colnames(downstream_hydrologic_distance) <- points$site


### Add segment_id to points
points %<>% 
  rowwise() %>% 
  group_split() %>% 
  map(
    .f = \(x) {
      wscode <- x$wscode_ltree
      ds_rm <- x$downstream_route_measure
      x$segment_id <- 
        segments %>% 
        as_tibble() %>% 
        filter(wscode_ltree == wscode) %>%
        filter(
          ds_rm >= downstream_route_measure &
            ds_rm <= upstream_route_measure
        ) %>% 
        pull(segment_id)
      x
    }
  ) %>% 
  do.call(rbind, .)

for (base in 1:nsites) {
  for (compare in 1:nsites) {
    # If i is downstream of j
    if (downstream[compare, base]) {
      # If they have the same wscode_ltree, subtract rm upstream from rm downstream
      # E.g. base = NMI (12), compare = NCC (11)
      if (points$wscode_ltree[compare] == points$wscode_ltree[base]) {
        downstream_hydrologic_distance[compare, base] <- 
          points$downstream_route_measure[base] - points$downstream_route_measure[compare]
      } else {
        # If they don't have the same wscode_ltree, 
        # Add downstream rm of base
        # Add upstream rm of all segments downstream of compare and upstream of base 
        # Subtract downstream rm of compare
        compare_segment <- rownames(flow_connected)[compare]
        base_segment <- rownames(flow_connected)[base]
        
        downstream_of_base <- 
          connectivity_ps %>% 
          filter(base == base_segment) %>% 
          filter(downstream) %>% 
          select(segment_id = compare, upstream_route_measure, downstream_route_measure) %>% 
          mutate(segment_length = upstream_route_measure - downstream_route_measure) %>%
          summarize(length = sum(segment_length)) %>% 
          pull(length)
        
        downstream_of_compare <- 
          connectivity_ps %>% 
          filter(base == compare_segment) %>%
          filter(downstream) %>% 
          select(segment_id = compare, upstream_route_measure, downstream_route_measure) %>% 
          mutate(segment_length = upstream_route_measure - downstream_route_measure) %>%
          summarize(length = sum(segment_length)) %>% 
          pull(length)
        
        dist_upstream_of_confluence <- 
          points %>% 
          filter(site == compare_segment) %>% 
          left_join(
            segments %>% 
              as_tibble() %>% 
              select(segment_id, seg_drm = downstream_route_measure), 
            join_by(segment_id)
          ) %>% 
          mutate(
            dist_upstream_of_confluence = downstream_route_measure - seg_drm
          ) %>% 
          pull(dist_upstream_of_confluence)
        
        downstream_hydrologic_distance[compare, base] <- 
          points$downstream_route_measure[base] + 
          downstream_of_base -
          downstream_of_compare -
          dist_upstream_of_confluence
      } 
    } else if (flow_connected[compare, base]) {
      downstream_hydrologic_distance[compare, base] <- 0
    } else {
      # Distance to common confluence (network intersection point) 
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
        downstream_of_base %>% 
        anti_join(downstream_of_compare, join_by(segment_id, upstream_route_measure)) %>% 
        mutate(
          segment_length = upstream_route_measure - downstream_route_measure
        )
      
      dist_upstream_of_confluence <- 
        points %>% 
        filter(site == base_segment) %>% 
        left_join(
          segments %>% 
            as_tibble() %>% 
            select(segment_id, seg_drm = downstream_route_measure), 
          join_by(segment_id)
        ) %>% 
        mutate(
          dist_upstream_of_confluence = downstream_route_measure - seg_drm
        ) %>% 
        select(downstream_route_measure, seg_drm, dist_upstream_of_confluence) %>%
        pull(dist_upstream_of_confluence)

      downstream_hydrologic_distance[compare, base] <- 
        dist_upstream_of_confluence + 
        sum(downstream_dist_to_cc$segment_length)
    }
  }
}
  
total_hydrologic_distance <- 
  downstream_hydrologic_distance + t(downstream_hydrologic_distance)

# Get cumulative watershed area from a point near the bottom of each segment
# Not the intersection point itself, because that would include upstream areas of both tribs
# The point 10% upstream from the confluence seems to work best to get the correct areas
# Checked by plotting
# Segment 11 (100.567134.179319.0000S2) had some strange behaviour with this method
# So taking its 7th point.

segments %<>% 
  rowwise() %>% 
  group_split() %>% 
  map(
    .f = \(x) {
      seg_point <- st_cast(x, to = "POINT") %>% 
        slice(if_else2(nrow(.) >= 10, round(0.1 * nrow(.)), 1)) %>%
        ps_sfc_to_longlat() %>% 
        suppressWarnings()
      if (x$segment_id == "100.567134.179319.0000S2") {
        seg_point <- st_cast(x, to = "POINT") %>% 
          slice(7) %>%
          ps_sfc_to_longlat() %>% 
          suppressWarnings()
      }
      area <- hydroshed(seg_point$Longitude, seg_point$Latitude) %>% 
        st_area()
      x$area <- area
      x
    }
  ) %>% 
  do.call(rbind, .)

nsegments <- nrow(segments)
downstream_segments <- matrix(NA, nrow = nsegments, ncol = nsegments)
colnames(downstream_segments) <- segments$segment_id
rownames(downstream_segments) <- segments$segment_id

for (i in 1:nsegments) {
  for (j in 1:nsegments) {
    downstream_segments[j, i] <- fwa_downstream(segments[i, ], segments[j, ])
  }
}

segment_pi <- 
  segments %>% 
  group_by(confluence_id) %>% 
  mutate(confluence_area = sum(area)) %>% 
  ungroup() %>% 
  mutate(
    segment_pi = drop_units(area / confluence_area)
  )

segment_afv <-
  segment_pi %>% 
  rowwise() %>% 
  group_split() %>%
  map(
    .f = \(x) {
      ds <- tibble(
        segment_id = segment_pi$segment_id,
        downstream = downstream_segments[, x$segment_id]
      ) %>% 
        filter(downstream) %>% 
        pull(segment_id)
      
      ds_pi <- 
        segment_pi %>% 
        filter(segment_id %in% ds) %>% 
        pull(segment_pi)
      
      x$segment_afv <- x$segment_pi * prod(ds_pi)
      x
    }
  ) %>% 
  do.call(rbind, .) %>% 
  as_tibble() %>% 
  select(segment_id, segment_afv)

points %<>% 
  left_join(segment_afv, join_by(segment_id))
  
weight_matrix <- matrix(NA, nrow = nsites, ncol = nsites)
rownames(weight_matrix) <- points$site
colnames(weight_matrix) <- points$site

for (i in 1:nsites) {
  for (j in 1:nsites) {
    i_upstream <- fwa_upstream(points[j, ], points[i, ])
    j_upstream <- fwa_upstream(points[i, ], points[j, ])
    if (i_upstream & j_upstream) {
      weight_matrix[i, j] <- 1
    } else if (i_upstream & !j_upstream) {
      weight_matrix[i, j] <- sqrt(points$segment_afv[i] / points$segment_afv[j])
    } else if (j_upstream & !i_upstream) {
      weight_matrix[i, j] <- sqrt(points$segment_afv[j] / points$segment_afv[i])
    } else {
      weight_matrix[i, j] <- 0
    }
  }
}

chk_true(all(diag(weight_matrix) == 1))
chk_true(isSymmetric(weight_matrix))
chk_true(all((weight_matrix == 0) == (flow_connected == 0)))


sbf_set_sub("distance")
# Save distance matrices
sbf_save_object(downstream_hydrologic_distance, "downstream_hydrologic_distance")
sbf_save_object(total_hydrologic_distance, "total_hydrologic_distance")
# Save weight matrix
sbf_save_object(weight_matrix, "weight_matrix")

### Compare distance matrix to that from openstars
dist <- readRDS("~/Analyses/fish-passage-22/nc.ssn/distance/obs/dist.net157.RData")
sbf_save_object(dist, "openstars_distance")

downstream_hydrologic_distance %>% view()
dist %>% view()
