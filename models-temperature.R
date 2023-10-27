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
  code = read_file("temperature.stan"),
  modify_data = function(data) {
    data$i_y_obs <- which(!is.na(data$mean_temp))
    data$i_y_mis <- which(is.na(data$mean_temp))
    data$N_y_obs <- length(data$i_y_obs)
    data$N_y_mis <- length(data$i_y_mis)
    data$y_obs <- data$mean_temp[!is.na(data$mean_temp)]
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
      prediction[i] <- exp(bIntercept + bDoy * doy[i] + bDoy2 * doy[i]^2)
      # Predict over range of H values (total hydrologic distance)
      eTU[i] <- sigma_tu^2 * exp(-3 * H[i] / alpha_tu)
      eTD[i] <- sigma_td^2 * exp(-3 * H[i] / alpha_td) # Flow-unconnected just sums distance to common confluence
      eED[i] <- sigma_ed^2 * exp(-3 * E[i] / alpha_ed)
    }",
  new_expr_vec = TRUE,
  select_data = list(
    mean_temp = c(0, 30, NA),
    site = factor(),
    week = factor(),
    `doy*` = c(1L, 366L),
    H = c(0, 50 * 5000),
    E = c(0, 50 * 500)
  )
)

sbf_save_block(template(model), "template", caption = "Model description.")
