description <- c(
  "`eAlpha`" = "Maximum stream temperature",
  "`bAlphaIntercept`" = "Intercept for `eAlpha`",
  "`bLogisticPars[, 1]`" = "Effect of `site` on `bAlphaIntercept`",
  "`eGamma`" = "Measure of the steepest slope of the function",
  "`bGammaIntercept`" = "Intercept for `eGamma`",
  "`bLogisticPars[, 3]`" = "Effect of `site` on `bGammaIntercept`",
  "`eBeta`" = "Air temperature at the inflection point of the curve",
  "`bBetaIntercept`" = "Intercept for `eBeta`",
  "`bLogisticPars[, 2]`" = "Effect of `site` on `bBetaIntercept`",
  "`eTemp[i]`" = "Expected value of `water_temp[i]`",
  "`y[i]`" = "The `i`^th^ water temperature value",
  "`air_temp[i]`" = "The `i`^th^ air temperature value",
  "`site[i]`" = "The `i`^th^ site",
  "`Y[t]`" = "Vector of water temperature values for all sites in the `t`^th^ week",
  "`mu[t]`" = "Vector of `eTemp` values for all sites in the `t`^th^ week",
  "`sigma_nug`" = "Standard deviation of the nugget effect",
  "`I`" = "The identity matrix",
  "`var_nug`" = "Variance of the nugget effect",
  "`var_td`" = "Variance of the tail-down covariance",
  "`sigma_td`" = "Standard deviation of the exponential tail-down covariance model",
  "`alpha_td`" = "The variance of spatially independent points",
  "`bRhoLogisticPars[1,2]`" = "Correlation coefficient between the alpha and beta parameters",
  "`bRhoLogisticPars[1,3]`" = "Correlation coefficient between the alpha and gamma parameters",
  "`bRhoLogisticPars[2,3]`" = "Correlation coefficient between the beta and gamma parameters",
  "`C_td`" = "Covariance matrix of the tail-down exponential model",
  "`H`" = "Total hydrologic distance matrix",
  "`D`" = "Downstream hydrologic distance matrix",
  "`flow_con_mat`" = "Site connectivity matrix",
  "`y_obs`" = "Vector of observed water temperature values",
  "`y_mis`" = "Vector of missing water temperature values",
  "`N_y_obs`" = "Number of observed water temperature values",
  "`N_y_mis`" = "Number of missing water temperature values",
  "`i_y_obs`" = "Indexes of observed water temperature values",
  "`i_y_mis`" = "Indexes of missing water temperature values",
  "`sLogisticPars[1]`" = "Standard deviation of the random effect of site on `eAlpha`",
  "`sLogisticPars[2]`" = "Standard deviation of the random effect of site on `eBeta`",
  "`sLogisticPars[3]`" = "Standard deviation of the random effect of site on `eGamma`",
  "`nsite`" = "Number of sites",
  "`nweek`" = "Number of weeks",
  "`bLRhoLogisticPars`" = "Cholesky factor of the correlation matrix for the logistic curve parameters, for efficient sampling",
  "`eZLogisticPars`" = "Matrix with standard normal priors, for efficient sampling"
)

description <- tibble(
  Parameter = names(description),
  Description = description
)

description %<>% arrange(Parameter)

sbf_save_table(description, caption = "Parameter descriptions.")

model <- model(
  code = read_file("ssn-air2stream.stan"),
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
      if (week[i] == 1) {
        eTemp[i] = bInitialTemp
      } else {
        # eTempDiff[i] = (1/(discharge[i]^a4)) * (a1 + a2 * air_temp[i] - a3 * eTemp[i - nsite] + discharge[i] * (a5 + a6 * cos(2 * 3.141592654 * ((week_year[i] / max_week_year[i]) - a7)) - a8 * eTemp[i - nsite]));
        eTempDiff[i] = (1/(discharge[i]^a4)) * (a1 + a2 * air_temp[i] - a3 * eTemp[i - nsite]);
        eTemp[i] = eTemp[i - nsite] + eTempDiff[i];
        if (eTemp[i] < 0) {
          eTemp[i] = 0.0;
        }
      }
      # eTD[i] <- sigma_td^2 * exp(-3 * H[i] / alpha_td) # Flow-unconnected just sums distance to common confluence
      fit[i] <- eTemp[i]
    }",
  new_expr_vec = FALSE,
  # gen_inits = function(data) {
  #   inits <- list()
  #   # nh converging priors
  #   inits$a1 ~ runif(1, 0, 0.6)
  #   inits$a2 ~ runif(1, 0, 0.2)
  #   inits$a3 ~ runif(1, 0, 0.4)
  #   inits$a4 ~ runif(1, 0.5, 0.7)
  #   inits$a5 ~ runif(1, 2, 6)
  #   inits$a6 ~ runif(1, 2, 6)
  #   inits$a7 ~ runif(1, 0.3, 0.7)
  #   inits$a8 ~ runif(1, 0, 0.2)
  #   # msc student
  #   # inits$a1 ~ runif(1, 2, 2.5)
  #   # inits$a2 ~ runif(1, 0.34, 0.5)
  #   # inits$a3 ~ runif(1, -2, 0)
  #   # inits$a4 ~ runif(1, 2, 4)
  #   # inits$a5 ~ runif(1, 2, 4)
  #   # inits$a6 ~ runif(1, 2, 4)
  #   # inits$a7 ~ runif(1, 0.5, 0.7)
  #   # inits$a8 ~ runif(1, 0.3, 0.8)
  #   inits
  # },
  select_data = list(
    site = factor(),
    week = factor(),
    week_year = c(1L, 53L),
    max_week_year = c(51L, 53L),
    water_temp = c(0, 30, NA),
    discharge = c(0, 100),
    air_temp = c(-50, 40),
    H = c(0, 50 * 5000),
    E = c(0, 50 * 500)
  )
)

sbf_save_block(template(model), "template", caption = "Model description.")
