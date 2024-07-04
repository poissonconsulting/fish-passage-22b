source("header.R")

options(mb.parallel = FALSE)

sbf_set_sub("temperature", "glm")

analysis <- sbf_load_object("analysis")
data <- data_set(analysis)

glance <- glance(analysis)
coef <- coef(analysis, include_constant = FALSE, simplify = TRUE) %>%
  mutate(across(estimate:svalue, \(x) signif(x, 3))) %>% 
  filter(!(str_detect(term, "y_mis")))

glance %>% print()
div <- partition_div(analysis$stanfit)
print(paste("There were", nrow(div[[1]]), "divergent transitions"))
coef %>% print(n = nrow(.)) 

tidy <- tidy(analysis) %>% 
  mutate(ess = esr * niters(analysis) * nchains(analysis))

sbf_save_table(glance, caption = "Model convergence")
sbf_save_table(coef, caption = "Model coefficients")

# Predict tail-up semivariogram
tu <- predict(analysis, "H", term = "eTU") %>% 
  mutate(type = "Tail-up")

td <- predict(analysis, "H", term = "eTD") %>% 
  mutate(type = "Tail-down")

ed <- predict(analysis, "H", term = "eED") %>% 
  mutate(type = "Euclidean")

covariance <- bind_rows(tu, td, ed) %>% 
  mutate(type = factor(type)) %>% 
  filter(H < 10000)

gp <- ggplot(covariance) +
  geom_line(aes(x = H, y = estimate)) +
  geom_line(aes(x = H, y = lower), linetype = "dotted") +
  geom_line(aes(x = H, y = upper), linetype = "dotted") +
  facet_grid(rows = vars(type)) +
  xlab("Distance (km)") +
  ylab("Covariance") +
  NULL

sbf_open_window(4, 5)
sbf_print(gp)

# Predict stream temp
# Solar radiation
solar_rad <- predict(analysis, xnew_data(data, net_solar_rad), term = "eTemp")

gp <- ggplot(solar_rad) +
  geom_line(aes(x = net_solar_rad, y = estimate)) +
  geom_line(aes(x = net_solar_rad, y = lower), linetype = "dotted") +
  geom_line(aes(x = net_solar_rad, y = upper), linetype = "dotted") +
  geom_point(
    data = data %>% filter(!is.na(water_temp)),
    aes(
      x = net_solar_rad,
      y = water_temp,
      colour = site
    ),
    alpha = 0.3
  ) +
  xlab("Net Solar Radiation") +
  ylab(expression(paste("Temperature (", degree, "C)"))) +
  NULL

sbf_open_window(4, 5)
sbf_print(gp)

# Elev
elev <- predict(analysis, xnew_data(data, elev), term = "eTemp")

gp <- ggplot(elev) +
  geom_line(aes(x = elev, y = estimate)) +
  geom_line(aes(x = elev, y = lower), linetype = "dotted") +
  geom_line(aes(x = elev, y = upper), linetype = "dotted") +
  geom_point(
    data = data %>% filter(!is.na(water_temp)),
    aes(
      x = elev,
      y = water_temp,
      colour = site
    ),
    alpha = 0.3
  ) +
  xlab("Elevation (m)") +
  ylab(expression(paste("Temperature (", degree, "C)"))) +
  NULL

sbf_open_window(4, 5)
sbf_print(gp)

# Discharge
discharge <- predict(analysis, xnew_data(data, discharge), term = "eTemp")

gp <- ggplot(discharge) +
  geom_line(aes(x = discharge, y = estimate)) +
  geom_line(aes(x = discharge, y = lower), linetype = "dotted") +
  geom_line(aes(x = discharge, y = upper), linetype = "dotted") +
  geom_point(
    data = data %>% filter(!is.na(water_temp)),
    aes(
      x = discharge,
      y = water_temp,
      colour = site
    ),
    alpha = 0.3
  ) +
  xlab(expression(paste("Discharge"~(m^3/s)))) +
  ylab(expression(paste("Temperature (", degree, "C)"))) +
  NULL

sbf_open_window(4, 5)
sbf_print(gp)

# Air temp
air_temp <- predict(analysis, xnew_data(data, air_temp), term = "eTemp")

gp <- ggplot(air_temp) +
  geom_line(aes(x = air_temp, y = estimate)) +
  geom_line(aes(x = air_temp, y = lower), linetype = "dotted") +
  geom_line(aes(x = air_temp, y = upper), linetype = "dotted") +
  geom_point(
    data = data %>% filter(!is.na(water_temp)),
    aes(
      x = air_temp,
      y = water_temp,
      colour = site
    ),
    alpha = 0.3
  ) +
  xlab(expression(paste("Air Temperature (", degree, "C)"))) +
  ylab(expression(paste("Water Temperature (", degree, "C)"))) +
  NULL

sbf_open_window(4, 5)
sbf_print(gp)

# Plot predicted vs observed
preds_vs_obs <-
  data %>% 
  rename(estimate = water_temp) %>% 
  mutate(type = "obs") %>% 
  bind_rows(
    predict(analysis, new_data = data, term = "eTemp") %>% 
      mutate(type = "pred - naive")
  ) %>% 
  # bind_rows(
  #   predict(analysis, new_data = data) %>%
  #     mutate(type = "pred - temporal corr")
  # ) %>%
  bind_rows(
    predict(analysis, new_data = data, term = "prediction_temporal_corr") %>%
    mutate(type = "pred - temporal corr")
  ) %>%
  # ** Don't show preds for first date (temporal correlation starts after this)
  filter(date != min(date))

gp <- ggplot(preds_vs_obs) +
  geom_pointrange(
    aes(x = date, y = estimate, ymin = lower, ymax = upper, colour = type), 
    alpha = 0.7
  ) +
  scale_colour_disc_poisson() +
  xlab("Date (weekly observations)") +
  facet_wrap(~site) +
  ylab(expression(paste("Temperature (", degree, "C)"))) +
  NULL

sbf_open_window(10, 6)
sbf_print(gp)

sbf_save_plot(x_name = "obs_vs_preds")
