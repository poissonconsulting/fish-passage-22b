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
  code = read_file("temperature-logistic.stan"),
  modify_data = function(data) {
    data$i_y_obs <- which(!is.na(data$water_temp))
    data$i_y_mis <- which(is.na(data$water_temp))
    data$N_y_obs <- length(data$i_y_obs)
    data$N_y_mis <- length(data$i_y_mis)
    data$y_obs <- data$water_temp[!is.na(data$water_temp)]
    
    data$I <- diag(1, nrow = data$nsite, ncol = data$nsite)
    data$D <- D
    data$W <- W
    data$H <- H
    data$flow_con_mat <- flow_con_mat
    data$E <- E
    data
  },
  new_expr = "
    for (i in 1:nObs) {
      eTemp[i] <- bMu + (bAlpha - bMu) / (1 + exp(bGamma * (bBeta - air_temp[i])))
      first_week[i] <- if_else(as.numeric(week[i]) == 1, 0, 1)
      if (i <= nsite) eEpsilon[i] <- 0 else eEpsilon[i] <- eTemp[i] - eTemp[i - nsite]
      prediction[i] <- eTemp[i] + (phi[site[i]] * eEpsilon[i]) * first_week[i]
      prediction_temporal_corr[i] <- eTempCorr[i]
      # Predict over range of H values (total hydrologic distance)
      eTU[i] <- sigma_tu^2 * exp(-3 * H[i] / alpha_tu)
      eTD[i] <- sigma_td^2 * exp(-3 * H[i] / alpha_td) # Flow-unconnected just sums distance to common confluence
      eED[i] <- sigma_ed^2 * exp(-3 * E[i] / alpha_ed)
      log_lik[i] <- eLogLik[i]
    }",
  new_expr_vec = FALSE,
  # new_expr_vec = TRUE,
  derived = c("eTempCorr", "y", "eLogLik"),
  select_data = list(
    site = factor(),
    week = factor(),
    water_temp = c(-2, 30, NA),
    `discharge/` = c(0, 1e10), # divides by sd
    `net_solar_rad/` = c(1e4, 1e9), # divide by sd
    `elev/` = c(500, 1000), # divide by sd
    air_temp = c(-30, 30), 
    H = c(0, 50 * 5000),
    E = c(0, 50 * 500)
  )
)

sbf_save_block(template(model), "template", caption = "Model description.")
