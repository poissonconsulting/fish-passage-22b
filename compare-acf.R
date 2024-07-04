source("header.R")

sbf_set_sub("temperature", "logistic-fixed-phi-re-alpha-beta-gamma")
analysis <- sbf_load_object("analysis")
data <- data_set(analysis)

analysis$model <- update_model(
  analysis$model,
  new_expr = "
    for (i in 1:nObs) {
      eAlpha[i] <- bAlphaIntercept + bLogisticPars[site[i], 1]
      eBeta[i] <- bBetaIntercept + bLogisticPars[site[i], 2]
      eGamma[i] <- bGammaIntercept + bLogisticPars[site[i], 3]
      eTemp[i] <- eAlpha[i] / (1 + exp(eGamma[i] * (eBeta[i] - air_temp[i])))
      prediction[i] <- eTemp[i]
      # first_week[i] <- if_else(as.numeric(week[i]) == 1, 0, 1)
      # if (i <= nsite) eEpsilon[i] <- 0 else eEpsilon[i] <- eTemp[i] - eTemp[i - nsite]
      # prediction[i] <- eTemp[i] + (0.5 * eEpsilon[i]) * first_week[i]
      # fit[i] <- eTemp[i]
      residual[i] <- res_norm(water_temp[i], prediction[i], sigma_tu + sigma_td + sigma_ed)
    }
  "
)

data$residual <- residuals(analysis)$estimate

sites <- levels(data$site)
site_acf <- rep(NA, length(sites))

par(mfrow = c(5, 5))
for (i in 1:nlevels(data$site)) {
  x <- data %>% filter(site == sites[i]) %>% pull(residual)
  site_acf[i] <- acf(x, na.action = na.pass)[[1]][2]
}

site_acf <- tibble(
  site = sites,
  phi = site_acf
)

site_acf %>% print(n = nrow(.))

site_acf %>% pull(phi) %>% mean()
