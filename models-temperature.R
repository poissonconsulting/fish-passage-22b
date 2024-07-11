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
    data
  },
  new_expr = "
    for (i in 1:nObs) {
      eAlpha[i] <- bAlphaIntercept + bLogisticPars[site[i], 1]
      eBeta[i] <- bBetaIntercept + bLogisticPars[site[i], 2]
      eGamma[i] <- bGammaIntercept + bLogisticPars[site[i], 3]
      eTemp[i] <- eAlpha[i] / (1 + exp(eGamma[i] * (eBeta[i] - air_temp[i])))
      # Predict over range of H values (total hydrologic distance)
      eTD[i] <- sigma_td^2 * exp(-3 * H[i] / alpha_td) # Flow-unconnected just sums distance to common confluence
      fit[i] <- eTemp[i]
    }",
  new_expr_vec = TRUE,
  derived = c(
    "y", 
    "bLogisticPars",
    "bRhoLogisticPars"
  ),
  select_data = list(
    site = factor(),
    week = factor(),
    water_temp = c(-2, 30, NA),
    air_temp = c(-30, 30), 
    H = c(0, 50 * 5000),
    E = c(0, 50 * 500)
  )
)

sbf_save_block(template(model), "template", caption = "Model description.")
