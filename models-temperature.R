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
    
    data$dis_station_id <- as.integer(discharge$station_id)
    data$dis_week <- as.integer(discharge$week)
    data$dis_discharge <- discharge$discharge
    data$dis_air_temp <- discharge$air_temp
    data$dis_evap_water <- discharge$evap_water
    data$dis_evap_total <- discharge$evap_total
    data$dis_snowmelt <- discharge$snowmelt
    data$dis_runoff_subsurface <- discharge$runoff_subsurface
    data$dis_runoff_surface <- discharge$runoff_surface
    data$dis_net_solar_rad <- discharge$net_solar_rad
    data$dis_precip <- discharge$precip
    data$dis_soil_vol_1 <- discharge$soil_vol_1
    
    data$nstation_id <- nlevels(discharge$station_id)
    data$nweek_dis <- nlevels(discharge$week)
    
    chk_equal(data$nweek, data$nweek_dis)
    
    data$dis_I <- diag(1, nrow = data$nsite, ncol = data$nsite)
    data$dis_D <- dis_D
    data$dis_W <- dis_W
    data$dis_H <- dis_H
    data$dis_flow_con_mat <- dis_flow_con_mat
    data$dis_E <- dis_E

    data
  },
  new_expr = "
    for (i in 1:nObs) {
      prediction[i] <- bIntercept + bShortwave * shortwave[i] + bDischarge * mean_discharge[i];
      # Predict over range of H values (total hydrologic distance)
      eTU[i] <- sigma_tu^2 * exp(-3 * H[i] / alpha_tu)
      eTD[i] <- sigma_td^2 * exp(-3 * H[i] / alpha_td) # Flow-unconnected just sums distance to common confluence
      eED[i] <- sigma_ed^2 * exp(-3 * E[i] / alpha_ed)
    }",
  new_expr_vec = TRUE,
  ### TODO: Is there a way to pass 2 dataframes to a model?
  select_data = list(
    site = factor(),
    week = factor(),
    water_temp = c(-2, 30, NA),
    air_temp = c(-50, 50),
    evap_water = c(1),
    evap_total = c(1),
    snowmelt = c(1),
    runoff_subsurface = c(1),
    runoff_surface = c(1),
    net_solar_rad = c(1),
    precip = c(1),
    soil_vol_1 = c(1),
    H = c(0, 50 * 5000),
    E = c(0, 50 * 500)
  )
)

sbf_save_block(template(model), "template", caption = "Model description.")
