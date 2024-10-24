description <- c(
  "`eTemp[i]`" = "Expected value of `water_temp[i]`",
  "`eTempDiff[i]`" = "Expected difference in average water temperature from the previous week",
  "`y[i]`" = "The `i`^th^ water temperature value (˚C)",
  "`air_temp[i]`" = "The `i`^th^ air temperature value (˚C)",
  "`site[i]`" = "The `i`^th^ site",
  "`week[i]`" = "The `i`^th^ week",
  "`Y[t]`" = "Vector of water temperature values for all sites in the `t`^th^ week",
  "`mu[t]`" = "Vector of `eTemp` values for all sites in the `t`^th^ week",
  "`sigma_nug`" = "Standard deviation of the nugget effect",
  "`I`" = "The identity matrix",
  "`var_nug`" = "Variance of the nugget effect",
  "`var_td`" = "Variance of the exponential tail-down covariance model",
  "`sigma_td`" = "Standard deviation of the exponential tail-down covariance model",
  "`alpha_td`" = "The variance of spatially independent points",
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
  "`nsite`" = "Number of sites",
  "`nweek`" = "Number of weeks",
  "`m1`" = "Mean of the site-wise random effect for the `a1` parameter",
  "`m2`" = "Mean of the site-wise random effect for the `a2` parameter",
  "`m3`" = "Mean of the site-wise random effect for the `a3` parameter",
  "`m4`" = "Mean of the site-wise random effect for the `a4` parameter",
  "`s1`" = "Standard deviation of the site-wise random effect for the `a1` parameter",
  "`s2`" = "Standard deviation of the site-wise random effect for the `a2` parameter",
  "`s3`" = "Standard deviation of the site-wise random effect for the `a3` parameter",
  "`s4`" = "Standard deviation of the site-wise random effect for the `a4` parameter",
  "`a1[i]`" = "Intercept-type parameter of the air2stream model for the `i`^th^ site",
  "`a2[i]`" = "Effect of `air_temp[i]` on `eTempDiff[i]` for the `i`^th^ site",
  "`a3[i]`" = "Effect of the previous week's expected water temperature (`eTemp[i - nsite]`) on `eTempDiff[i]`, for the `i`^th^ site",
  "`a4[i]`" = "Effect of `discharge[i]` on `eTempDiff[i]` for the `i`^th^ site",
  "`bInitialTemp`" = "Expected average water temperature for the week starting 01-01-2019 for all sites",
  "`discharge[i]`" = "Dimensionless discharge for the `i`^th^ observation (discharge for that observation divided by the mean discharge across all observations for that site)"
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
        eTempDiff[i] = (1/(discharge[i]^a4[site[i]])) * (a1[site[i]] + a2[site[i]] * air_temp[i] - a3[site[i]] * eTemp[i - nsite]);
        eTemp[i] = eTemp[i - nsite] + eTempDiff[i];
        if (eTemp[i] < 0) {
          eTemp[i] = 0.0;
        }
      }
      eTD[i] <- sigma_td^2 * exp(-3 * H[i] / alpha_td) # Flow-unconnected just sums distance to common confluence
      fit[i] <- eTemp[i]
    }",
  new_expr_vec = FALSE,
  gen_inits = function(data) {
    inits <- list()
    inits$a1 <- runif(data$nsite, 0.4, 0.6)
    inits$a2 <- runif(data$nsite, 0.1, 0.3)
    inits$a3 <- runif(data$nsite, 1.5, 2.5)
    inits$a4 <- runif(data$nsite, -0.1, 0.1)
    inits
  },
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
  ),
  random_effects = list(
    a1 = "site",
    a2 = "site",
    a3 = "site",
    a4 = "site"
  )
)

sbf_save_block(template(model), "template", caption = "Model description.")
