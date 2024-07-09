source("header.R")

options(mb.parallel = FALSE)

sbf_set_sub("temperature", "logistic-no-phi-re-alpha-beta-gamma")

analysis <- sbf_load_object("analysis")
data <- data_set(analysis)

glance <- glance(analysis)
coef <- coef(analysis, include_constant = FALSE, simplify = TRUE) %>%
  mutate(across(estimate:svalue, \(x) signif(x, 3))) %>% 
  filter(!(str_detect(term, "y_mis")))

derived_coef <- coef(analysis, param_type = "derived", include_constant = FALSE, simplify = TRUE) %>% 
  filter(str_detect(term, "bRhoLogisticPars"))

coef %<>% bind_rows(derived_coef) %>% arrange(term)

glance %>% print()
div <- partition_div(analysis$stanfit)
print(paste("There were", nrow(div[[1]]), "divergent transitions"))
coef %>% print(n = nrow(.)) 

sbf_save_table(glance, caption = "Model convergence")
sbf_save_table(coef, caption = "Model coefficients")

message("updated new expression")
analysis$model <- update_model(
  analysis$model,
  new_expr = "
    for (i in 1:nObs) {
      eAlpha[i] <- bAlphaIntercept + bLogisticPars[site[i], 1]
      eBeta[i] <- bBetaIntercept + bLogisticPars[site[i], 2]
      eGamma[i] <- bGammaIntercept + bLogisticPars[site[i], 3]
      eTemp[i] <- eAlpha[i] / (1 + exp(eGamma[i] * (eBeta[i] - air_temp[i])))
      # Predict over range of H values (total hydrologic distance)
      eTU[i] <- sigma_tu^2 * exp(-3 * H[i] / alpha_tu)
      eTD[i] <- sigma_td^2 * exp(-3 * H[i] / alpha_td) # Flow-unconnected just sums distance to common confluence
      eED[i] <- sigma_ed^2 * exp(-3 * E[i] / alpha_ed)
      log_lik[i] <- eLogLik[i]
    }",
  new_expr_vec = TRUE
)

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

sbf_save_plot(
  x_name = "covariance"
)

# Predict stream temp
# Air temp
air_temp <- predict(analysis, xnew_data(data, site, air_temp), term = "eTemp")

gp <- ggplot(air_temp) +
  geom_line(aes(x = air_temp, y = estimate)) +
  geom_line(aes(x = air_temp, y = lower), linetype = "dotted") +
  geom_line(aes(x = air_temp, y = upper), linetype = "dotted") +
  geom_point(
    data = data %>% filter(!is.na(water_temp)),
    aes(
      x = air_temp,
      y = water_temp
    ),
    alpha = 0.3
  ) +
  facet_wrap(~site) +
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
      mutate(type = "pred")
  ) %>% 
  # bind_rows(
  #   predict(analysis, new_data = data) %>%
  #     mutate(type = "pred - temporal corr")
  # ) %>%
  # bind_rows(
  #   predict(analysis, new_data = data, term = "prediction_temporal_corr") %>%
  #     mutate(type = "pred - temporal corr")
  # ) %>%
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

