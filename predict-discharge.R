source("header.R")

options(mb.parallel = FALSE)

sbf_set_sub("discharge")
# sbf_set_sub("discharge-simple")

analysis <- sbf_load_object("analysis")
data <- data_set(analysis)

glance <- glance(analysis)
coef <- coef(analysis, include_constant = FALSE, simplify = TRUE) %>%
  mutate(across(estimate:svalue, \(x) signif(x, 3))) %>% 
  filter(!(str_detect(term, "y_mis")))

glance %>% print()
coef %>% print(n = nrow(.)) 

tidy <- tidy(analysis) %>% 
  mutate(ess = esr * niters(analysis) * nchains(analysis))

# chk_true(nrow(tidy %>% filter(ess < 100)) == 0)

sbf_save_table(glance, caption = "Model convergence")
sbf_save_table(coef, caption = "Model coefficients")

# Precipitation ----
precip <- predict(analysis, "sum_us_precip")

gp <- ggplot(precip) +
  geom_line(aes(x = (sum_us_precip), y = (estimate))) +
  geom_line(aes(x = (sum_us_precip), y = (lower)), linetype = "dotted") +
  geom_line(aes(x = (sum_us_precip), y = (upper)), linetype = "dotted") +
  geom_point(data = data, aes(x = (sum_us_precip), y = (discharge)), alpha = 0.3) +
  xlab(expression("Upstream Precipitation"~(m^3/s))) +
  ylab(expression("Discharge"~(m^3/s))) +
  NULL

sbf_open_window(3, 3)
sbf_print(gp)

sbf_save_plot(x_name = "precip")

# Snow melt ----
snowmelt <- predict(analysis, "sum_us_snowmelt")

gp <- ggplot(snowmelt) +
  geom_line(aes(x = (sum_us_snowmelt), y = (estimate))) +
  geom_line(aes(x = (sum_us_snowmelt), y = (lower)), linetype = "dotted") +
  geom_line(aes(x = (sum_us_snowmelt), y = (upper)), linetype = "dotted") +
  geom_point(data = data, aes(x = (sum_us_snowmelt), y = (discharge)), alpha = 0.3) +
  xlab(expression("Upstream Snow Melt"~(m^3/s))) +
  ylab(expression("Discharge"~(m^3/s))) +
  NULL

sbf_open_window(3, 3)
sbf_print(gp)

sbf_save_plot(x_name = "snowmelt")

# # Upstream Area ----
# area <- predict(analysis, "upstream_area")
# 
# gp <- ggplot(area) +
#   geom_line(aes(x = upstream_area, y = estimate)) + 
#   geom_line(aes(x = upstream_area, y = lower), linetype = "dotted") +
#   geom_line(aes(x = upstream_area, y = upper), linetype = "dotted") +
#   geom_point(data = data, aes(x = upstream_area, y = discharge)) + 
#   xlab(expression("Upstream Area"~(m^2))) +
#   ylab(expression("Discharge"~(m^3/s))) +
#   NULL
# 
# sbf_open_window(3, 3)
# sbf_print(gp)

# # Baseline ----
# baseline <- predict(analysis, "upstream_area", term = "eBaseline")
# 
# gp <- ggplot(baseline) +
#   geom_line(aes(x = upstream_area, y = exp(estimate))) +
#   geom_line(aes(x = upstream_area, y = exp(lower)), linetype = "dotted") +
#   geom_line(aes(x = upstream_area, y = exp(upper)), linetype = "dotted") +
#   geom_point(data = data, aes(x = upstream_area, y = baseline)) +
#   xlab(expression("Upstream Area"~(m^2))) +
#   ylab(expression("Baseline Flow"~(m^3/s))) +
#   NULL
# 
# sbf_open_window(3, 3)
# sbf_print(gp)
# 
# sbf_save_plot(x_name = "baseline")

# Compare predictions to real values
date_ref <- sbf_load_data("date_ref", sub = "discharge") %>% 
  arrange(date) %>% 
  group_by(week) %>% 
  slice_head() %>% 
  ungroup() %>% 
  select(date, week)

date_ref_dates <- date_ref$date
sbf_save_object(date_ref_dates, "weekly-breaks", sub = "clean")

preds_vs_obs <-
  predict(analysis, new_data = data) %>% 
  arrange(week, site) %>% 
  mutate(type = "prediction") %>% 
  bind_rows(
    data %>%
      mutate(type = "observation") %>% 
      rename(estimate = discharge)
  ) %>% 
  mutate(week = as.numeric(as.character(week))) %>% 
  left_join(date_ref, join_by(week))

gp <- ggplot(preds_vs_obs) +
  geom_pointrange(aes(x = date, y = estimate, ymin = lower, ymax = upper, colour = type)) +
  scale_colour_disc_poisson() +
  xlab("Date (weekly observations)") +
  facet_grid(rows = vars(site)) +
  # facet_wrap(~site) +
  ylab(expression("Discharge"~(m^3/s))) +
  NULL

sbf_open_window(6, 6)
sbf_print(gp)

sbf_save_plot(x_name = "obs_vs_preds")

# data$x <- data$precip_vol_lag
# mod <- lm(data$discharge ~ data$x)
# data$prediction <- predict(mod)
# plot(data$discharge ~ data$x, type = "p")
# lines(data$prediction ~ data$x)

# # Prediction sites
# preds <- 
#   data %>% 
#   filter(is.na(discharge)) %>% 
#   predict(analysis, new_data = ., term = "eDischargePreds")
# 
# 
# gp <- ggplot(preds) + 
#   geom_line(aes(x = week, y = estimate, group = site)) +
#   geom_line(aes(x = week, y = lower, group = site), linetype = "dotted") +
#   geom_line(aes(x = week, y = upper, group = site), linetype = "dotted") +
#   facet_wrap(~site) +
#   xlab("Week") +
#   ylab(expression("Discharge"~(m^3/s))) +
#   NULL
# 
# sbf_open_window()
# sbf_print(gp)
