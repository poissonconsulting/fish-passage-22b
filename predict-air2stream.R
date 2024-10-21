source("header.R")

options(mb.parallel = FALSE)

sbf_set_sub("temperature-air2stream")

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

sbf_save_table(glance, caption = "Model convergence")
sbf_save_table(coef, caption = "Model coefficients")

# Predict tail-down semivariogram
td <- predict(analysis, "H", term = "eTD") %>% 
  mutate(type = "Tail-down")

gp <- ggplot(td) +
  geom_line(aes(x = H, y = estimate)) +
  geom_line(aes(x = H, y = lower), linetype = "dotted") +
  geom_line(aes(x = H, y = upper), linetype = "dotted") +
  xlab("Distance (km)") +
  ylab("Covariance") +
  NULL

sbf_open_window(3, 2)
sbf_print(gp)

sbf_save_plot(
  x_name = "covariance-distance",
  report = TRUE,
  caption = "Tail-down covariance by hydrologic distance (with 95% CIs)"
)

# Plot predicted vs observed
# preds_vs_obs <-
#   data %>% 
#   rename(estimate = water_temp) %>% 
#   mutate(type = "obs") %>% 
#   bind_rows(
#     predict(analysis, new_data = data, term = "eTemp") %>% 
#       mutate(type = "pred")
#   )

water_temp <- predict(analysis, new_data = data, term = "eTemp")

gp <- ggplot(water_temp, aes(x = date)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = pois_cols("grey")) +
  geom_line(aes(y = estimate), linewidth = 0.2) +
  geom_point(data = data, aes(y = water_temp), colour = pois_cols("red"), alpha = 0.3, size = 0.5) +
  facet_wrap(~site, nrow = 8) + 
  xlab("Date") +
  ylab(expression(paste("Water Temperature (", degree, "C)"))) +
  NULL

sbf_open_window(6, 9)
sbf_print(gp)

sbf_save_plot(
  x_name = "water-temp",
  report = TRUE,
  caption = "Predicted water temperature by date (with 95% CIs). The points are the observed data."
)

