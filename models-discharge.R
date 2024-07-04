description <- c(
  "`bY`" = "Intercept for `eY`",
  "`bX`" = "Effect of `X` on `bY`",
  "`sY`" = "SD of residual variation in `Y`",
  "`eY[i]`" = "Expected value of `y[i]`",
  "`Y[i]`" = "The `i`^th^ Y value"
)

description <- tibble(
  Parameter = names(description),
  Description = description
)

description %<>% arrange(Parameter)

sbf_save_table(description, caption = "Parameter descriptions.")

model <- model(
  code = read_file("discharge.stan"),
  modify_data = function(data) {
    data$i_y_obs <- which(!is.na(data$discharge))
    data$i_y_mis <- which(is.na(data$discharge))
    data$N_y_obs <- length(data$i_y_obs)
    data$N_y_mis <- length(data$i_y_mis)
    data$y_obs <- data$discharge[!is.na(data$discharge)]
    data$I <- diag(1, nrow = data$nsite, ncol = data$nsite)
    data$D <- D
    data$W <- W
    data$H <- H
    data$flow_con_mat <- flow_con_mat
    data$E <- E
    data$i_baseline <- which(!is.na(data$baseline))
    data$baseline <- data$baseline[data$i_baseline]
    data$nsite_baseline <- length(data$i_baseline)
    data$upstream_area <- data$upstream_area[data$i_baseline]
    data
  },
  new_expr = "
    for (i in 1:nObs) {
      # eBaseline[i] = bInterceptBaseline + bAreaBaseline * log(upstream_area[i])
      prediction[i] = exp(bIntercept + bPrecip * sum_us_precip[i] + bSnowmelt * sum_us_snowmelt[i])
      eDischargePreds[i] = y_mis[i]
      # Predict over range of H values (total hydrologic distance)
      eTU[i] <- sigma_tu^2 * exp(-3 * H[i] / alpha_tu)
      eTD[i] <- sigma_td^2 * exp(-3 * H[i] / alpha_td) # Flow-unconnected just sums distance to common confluence
      eED[i] <- sigma_ed^2 * exp(-3 * E[i] / alpha_ed)
    }
  ",
  new_expr_vec = TRUE,
  select_data = list(
    site = factor(),
    week = factor(),
    discharge = c(0, 1000, NA),
    baseline = c(0, 500),
    # `precip_vol*` = c(0, 100000),
    # `snowmelt_vol*` = c(0, 50000),
    # `precip_vol_lag` = c(0, 1000000),
    # `snowmelt_vol_lag` = c(0, 1000000),
    # lag_us_precip = c(0, 1),
    # lag_us_snowmelt = c(0, 1),
    # `mean_us_precip` = c(0, 1),
    # `mean_us_snowmelt` = c(0, 1),
    `sum_us_precip*` = c(0, 500),
    `sum_us_snowmelt*` = c(0, 1),
    `upstream_area` = c(1e6, 1e11)
    # `air_temp*` = c(-50, 50),
    # `evap_water*` = c(1),
    # `evap_total*` = c(1),
    # `snowmelt*` = c(1),
    # `runoff_subsurface*` = c(1),
    # `runoff_surface*` = c(1),
    # `net_solar_rad*` = c(1),
    # `precip*` = c(1)
    # `soil_vol_1*` = c(0, 1)
    # `upstream_area*` = c(1),
    # `us_evap_water` = c(1),
    # `us_snowmelt` = c(1),
    # `us_runoff_surface` = c(1),
    # `us_precip` = c(1),
    # H = c(0, 50 * 5000),
    # E = c(0, 50 * 500)
  )
)

sbf_save_block(template(model), "template", caption = "Model description.")
