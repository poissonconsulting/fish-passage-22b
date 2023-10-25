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
    data$I <- diag(0, nrow = data$nsite, ncol = data$nsite)
    data$D <- D
    data$W <- W
    data$H <- H
    data$flow_con_mat <- flow_con_mat
    data$E <- E
    data$alpha_max <- 4 * max(data$H) 
    # data$npredictors <- 3L
    # data$X <- array(NA, dim = c(data$ndate, data$nsite, data$npredictors))
    # for (t in 1:data$ndate) {
    #   # Intercept
    #   data$X[t, , 1] <- rep(1.0, data$nsite)
    #   # bDoy
    #   data$X[t, , 2] <- data$doy[((t - 1) * data$nsite + 1):(t * data$nsite)]
    #   # bDoy2
    #   data$X[t, , 3] <- data$X[t, , 2]^2
    # }
    data
  },
  new_expr = "
    for(i in 1:nObs) {
      
    }",
  new_expr_vec = TRUE,
  select_data = list(
    mean_temp = c(0, 30, NA),
    site = factor(),
    date = factor(),
    `doy*` = c(1L, 366L)
  )
)

sbf_save_block(template(model), "template", caption = "Model description.")
