source("header.R")

sbf_set_sub("temperature")

analysis <- sbf_load_object("analysis")
data <- data_set(analysis)

# GSDD without uncertainty ----
# using interpolated estimated water temperature for each day in week
gsdd_pred <-
  predict(analysis, new_data = data, term = "eTemp") %>%
  select(site, date, temperature = estimate) %>%
  arrange(site, date) %>%
  group_by(site) %>%
  group_split() %>%
  map(
    .f = function(y) {
      site <- y$site[1]
      y_zoo <- zoo::zoo(y$temperature, y$date)
      y_filled <- zoo::na.approx(y_zoo, xout = seq(start(y_zoo), end(y_zoo), by = "day"), na.rm = FALSE)
      y <- tibble(
          site = site,
          date = seq(start(y_zoo), end(y_zoo), by = "day"),
          temperature = as.numeric(y_filled)
        )
      y_gsdd <- y %>%
        gsdd::gsdd() %>%
        mutate(site = site)
    }
  ) %>%
  bind_rows()

# GSDD with uncertainty ----
# using constant temperature for each day in the week
full_dates_data <-
  xnew_data(
    data,
    site,
    xnew_seq(date, length_out = as.integer(bounding_dates[2] - bounding_dates[1]) + 3)
  ) %>% 
  select(site, date) %>% 
  left_join(data, join_by(site, date)) %>% 
  mutate(
    first_day_of_week = if_else2(!is.na(week), TRUE, FALSE),
    annual = factor(dtt_year(date))
  ) %>% 
  group_by(site) %>% 
  fill(
    week, water_temp, air_temp, discharge_site, discharge, elev, 
    site_remains, H, E, nsite, 
    .direction = "down"
  ) %>% 
  ungroup()

x <- mcmc_derive_data(
  analysis,
  new_data = full_dates_data,
  term = "eTemp"
)

gsdd <-
  x %>% 
  group_by(site, annual) %>% 
  summarize(
    .fun = gsdd::gsdd_vctr
  ) %>% 
  coef()

gp <- ggplot(gsdd) +
  geom_pointrange(aes(x = annual, y = estimate, ymin = lower, ymax = upper), size = 0.3) +
  facet_wrap(~site) +
  xlab("Year") +
  ylab("GSDD") +
  guides(x = guide_axis(angle = 45)) +
  NULL

sbf_open_window(5)
sbf_print(gp)

sbf_save_plot(
  x_name = "gsdd-annual-site",
  report = TRUE,
  caption = "GSDD by year and site (with 95% CIs)"
)

stream_network <- sbf_load_data("stream_network", sub = "distance/temp") %>% 
  st_combine()
points <- sbf_load_data("points", sub = "distance/temp") %>% 
  select(site)

gsdd_spatial <-
  gsdd %>% 
  left_join(points, join_by(site)) %>% 
  ps_activate_sfc()
  
gp <- ggplot() +
  geom_sf(data = stream_network) +
  geom_sf(data = gsdd_spatial, aes(colour = estimate), size = 2) +
  facet_grid(rows = vars(annual)) +
  labs(colour = "GSDD") +
  NULL
  
sbf_open_window(4, 6)
sbf_print(gp)

sbf_save_plot(
  x_name = "gsdd-map",
  report = TRUE,
  caption = "Median GSDD estimate by year and site. The black lines are the stream network."
)
