source("header.R")

is.units <- function(x) inherits(x, 'units')
strip_units <- function(x) modify_if(x, is.units, as.vector)
library(SSNbayes)

water_temp <- sbf_load_data("water_temp", "clean")
air_temp <- sbf_load_data("air_temp", "clean")
water_temp_site <- sbf_load_data("water_temp_site", "clean")

water_temp %<>% 
  filter(flag == "P") %>% 
  mutate(date = dtt_date(date_time)) %>% 
  group_by(site, date) %>% 
  summarize(water_temp = mean(temp), .groups = "keep")
  
air_temp %<>% 
  select(site, date, mean_temp) %>% 
  arrange(date, site) %>% 
  rename(air_temp = mean_temp) %>% 
  left_join(water_temp, by = c("site", "date")) %>% 
  filter(!is.na(water_temp), !is.na(air_temp)) %>% 
  left_join(water_temp_site %>% select(site, elev), by = "site")

data <- air_temp %>%
  mutate(
    pid = 1:nrow(.),
    locID = rep(c(1, 2), nrow(.)/2)
  ) %>% 
  strip_units() %>% 
  as.data.frame()

# A formula as in lm()
formula <- water_temp ~ air_temp + elev

# A long data frame containing the locations, dates, covariates and the response variable. It has to have the locID and date. No missing values are allowed in the covariates. The order in this data.fame MUST be: spatial locations (1 to S) at time t=1, then locations (1 to S) at t=2 and so on.
data <- data
# Path with the name of the SpatialStreamNetwork object
path <- file.path("nc.ssn")

library(SSN)
n <- importSSN(path)
createDistMat(n, o.write = TRUE)
getStreamDistMat(n, Name = "obs")

# A list specifying the temporal structure (ar = Autorregressive; var = Vector autorregression) and coumn in the data with the time variable.  
time_method <- list("ar", "date") ### TODO: decide on ar or var
# A list defining if use or not of an SSN object and the spatial correlation structure. The second element is the spatial covariance structure. A 3rd element is a list with the lon and lat for Euclidean distance models.
space_method <- list("use_ssn", c("Exponential.taildown")) # e.g. list("use_ssn", c("Exponential.taildown"))

# Stan params
iter <- 3000
warmup <- 1500
chains <- 3
refresh <- max(iter/100, 1) # sampler refreshing rate
# The network id (optional). Used when the SSN object cotains multiple networks. 
net <- 157
# Variable to compute the additive function. Used to compute the spatial weights.
addfunccol <- NULL
# Logic parameter denoting if the loglik will be computed by the model.
loglik <- FALSE
# (optional) A seed for reproducibility
seed <- 10

# function (formula = formula, data = data, path = path, time_method = time_method, 
#           space_method = space_method, iter = 3000, warmup = 1500, 
#           chains = 3, refresh = max(iter/100, 1), net = 1, addfunccol = addfunccol, 
#           loglik = FALSE, seed = seed) 
# {
  if (missing(time_method)) {
    stop("Need to define the method (ar or var) and the column associated with time")
  }
  if (length(time_method) == 1) {
    stop("Need to specify the column in the the data with the time variable")
  }
  time_points <- time_method[[2]]
  if ("locID" %in% names(data) == FALSE) 
    stop("There is no column locID on the data. Please, set a column called locID with the observation locations")
  if (missing(seed)) 
    seed <- sample(1:1e+06, 1, replace = TRUE)
  if (!missing(space_method)) {
    print("using SSN object")
    if (space_method[[1]] == "use_ssn") {
      ssn_object <- TRUE
      if (length(space_method) > 1) {
        if (space_method[[2]] %in% c(
          "Exponential.tailup", "LinearSill.tailup", "Spherical.tailup", 
          "Exponential.taildown", "LinearSill.taildown", "Spherical.taildown", 
          "Exponential.Euclid") == FALSE) {
          stop("Need to specify one or more of the following covariance matrices: Exponential.tailup, LinearSill.tailup , Spherical.tailup ,\n\t\tExponential.taildown, LinearSill.taildown, Spherical.taildown or Exponential.Euclid")
        }
        CorModels <- space_method[[2]]
      }
      if (length(space_method) == 1) {
        CorModels <- "Exponential.tailup"
        print("using an Exponential.tailup model")
      }
    }
    if (space_method[[1]] == "no_ssn") {
      print("no SSN object defined")
      ssn_object <- FALSE
      if (space_method[[2]] %in% c("Exponential.Euclid") == 
          FALSE) {
        stop("Need to specify Exponential.Euclid")
      }
      if (length(space_method) < 3) {
        stop("Please, specify the columns in the data frame with the longitude and latitude (c('lon', 'lat'))")
      }
      data$lon <- data[, names(data) == space_method[[3]][1]]
      data$lat <- data[, names(data) == space_method[[3]][2]]
      CorModels <- space_method[[2]]
    }
  }
  if (missing(space_method)) {
    space_method <- "no_ssn"
    ssn_object <- FALSE
    CorModels <- "Exponential.Euclid"
  }
  cor_tu <- case_when(CorModels == "Exponential.tailup" ~ 
                        1, CorModels == "LinearSill.tailup" ~ 2, CorModels == 
                        "Spherical.tailup" ~ 3, TRUE ~ 5)
  cor_tu <- sort(cor_tu)[1]
  cor_td <- case_when(CorModels == "Exponential.taildown" ~ 
                        1, CorModels == "LinearSill.taildown" ~ 2, CorModels == 
                        "Spherical.taildown" ~ 3, TRUE ~ 5)
  cor_td <- sort(cor_td)[1]
  cor_ed <- case_when(CorModels == "Exponential.Euclid" ~ 
                        1, TRUE ~ 5)
  cor_ed <- sort(cor_ed)[1]
  cor_re <- case_when(CorModels == "RE1" ~ 1, TRUE ~ 5)
  cor_re <- sort(cor_re)[1]
  data_com <- "data {\n    int<lower=1> N;\n    int<lower=1> K;\n    int<lower=1> T;\n    matrix[N,K] X[T] ; // real X[N,K,T]; //\n\n    int<lower = 0> N_y_obs; // number observed values\n    int<lower = 0> N_y_mis; // number missing values\n\n    int<lower = 1> i_y_obs[N_y_obs] ;  //[N_y_obs,T]\n    int<lower = 1> i_y_mis[N_y_mis] ;  // N_y_mis,T]\n\n    vector[N_y_obs] y_obs;    //matrix[N_y_obs,1] y_obs[T];\n\n    matrix[N, N] W ; // spatial weights\n    matrix[N, N] h ; // total hydrological dist\n    matrix[N, N] I ; // diag matrix\n\n    matrix[N, N] D ; // downstream hydrological dist matrix\n    matrix[N, N] flow_con_mat; // flow conected matrix\n\n    matrix[N, N] e ; // Euclidean dist mat\n    real<lower=1> alpha_max ;\n\n  }"
  param_com <- "\n  parameters {\n    vector[K] beta;\n    real<lower=0> sigma_nug;\n\n    vector[N_y_mis] y_mis;//declaring the missing y\n  "
  param_phi_ar <- "\n    real <lower=-1, upper = 1> phi; // NB\n  "
  param_phi_var <- "\n    vector<lower=-1, upper = 1> [N] phi  ; // vector of autoregresion pars\n  "
  param_tu <- "\n    real<lower=0> sigma_tu;\n    real<lower=0> alpha_tu;\n"
  param_td <- "\n    real<lower=0> sigma_td; // sd of tail-down\n    real<lower=0> alpha_td; // range of the tail-down model\n"
  param_ed <- "\n    real<lower=0> sigma_ed;\n    real<lower=0> alpha_ed; // range of the Euclidean dist model\n"
  param_re <- "\n    real<lower=0> sigma_RE1;\n"
  tparam_com <- "\n  transformed parameters {\n    vector[N * T] y;\n    vector[N] Y[T];\n\n    vector[N] epsilon[T]; // error term\n    vector[N] mu[T]; // mean\n\n   real<lower=0> var_nug; // nugget\n\n   matrix[N, N] C_tu; //tail-up cov\n   matrix[N, N] C1; //tail-up cov\n   matrix[N, N] Ind; //tail-up indicator\n\n   matrix[N, N] C_td; //tail-down cov\n   matrix[N, N] Ind2; //tail-down indicator\n   matrix[2,1] iji;\n\n   matrix[N, N] C_ed ;// Euclidean cov\n\n   matrix[N, N] C_re ;// random effect cov\n   matrix[N, N] RE1; // random effect 1\n\n   "
  tparam_tu <- "\n   // tail up exponential\n   real<lower=0> var_tu; // parsil tail-down\n  "
  tparam_td <- "\n    real<lower=0> var_td; // parsil tail-down\n "
  tparam_ed <- "\n    real<lower=0> var_ed; //  Euclidean dist var\n"
  tparam_re <- "\n    real<lower=0> var_RE1; // Random effect 1\n"
  tparam_com2 <- "\n    y[i_y_obs] = y_obs;\n    y[i_y_mis] = y_mis;\n    for (t in 1:T){\n      Y[t] = y[((t - 1) * N + 1):(t * N)];\n    }\n\n    var_nug = sigma_nug ^ 2; // variance nugget\n        mu[1] = X[1] * beta;\n    epsilon[1] = Y[1] - mu[1];\n\n"
  tparam_com_ar <- "\n        for (t in 2:T){\n        mu[t] = X[t] * beta;\n        epsilon[t] = Y[t] - mu[t];\n        mu[t] = mu[t] + phi * epsilon[t-1]; //\n    }\n\n  "
  tparam_com_var <- "\n        for (t in 2:T){\n        mu[t] = X[t] * beta;\n        epsilon[t] = Y[t] - mu[t];\n        mu[t] = mu[t] + phi .* epsilon[t-1]; // element wise mult two vectors\n    }\n  "
  tparam_tu2_exp <- "\n    // tail up exponential\n    var_tu = sigma_tu ^ 2; // variance tail-up\n      C1 = var_tu * exp(- 3 * h / alpha_tu); // tail up exponential model\n    C_tu = C1 .* W; // Hadamard (element-wise) product\n"
  tparam_tu2_lin <- "\n     //Tail-up linear-with-sill model\n     var_tu = sigma_tu ^ 2; // variance tail-up\n      for (i in 1:N) {\n        for (j in 1:N) {\n        Ind[i,j] = (h[i,j] / alpha_tu) <= 1 ? 1 : 0; // indicator\n        }\n      }\n      C1 = var_tu * (1 - (h / alpha_tu)) .* Ind ; //Tail-up linear-with-sill model\n    C_tu = C1 .* W; // Hadamard (element-wise) product\n"
  tparam_tu2_sph <- "\n      // Tail-up spherical model\n      var_tu = sigma_tu ^ 2; // variance tail-up\n      for (i in 1:N) {// Tail-up spherical model\n        for (j in 1:N) {\n          Ind[i,j] = (h[i,j] / alpha_tu) <= 1 ? 1 : 0; // indicator\n        }\n      }\n    C1 = var_tu * (1 - (1.5 * h / alpha_tu) + (h .* h .* h / (2 * alpha_tu ^ 3))) .* Ind ; // Tail-up spherical model\n    C_tu = C1 .* W; // Hadamard (element-wise) product\n"
  tparam_td2_exp <- "\n    var_td= sigma_td ^ 2; // variance tail-down\n     \tfor (i in 1:N) {// Tail-down exponential model\n          for (j in 1:N) {\n            if(flow_con_mat[i,j] == 1){ // if points are flow connected\n               C_td[i,j] = var_td * exp(- 3 * h[i,j] / alpha_td);\n            }\n            else{// if points are flow unconnected\n              C_td[i,j] = var_td * exp(- 3 * (D[i,j] + D[j,i]) / alpha_td);\n            }\n          }\n        }\n\n"
  tparam_td2_lin <- "\n    var_td= sigma_td ^ 2; // variance tail-down\n        for (i in 1:N) {// Tail-down linear-with-sill model\n          for (j in 1:N) {\n            if(flow_con_mat[i,j] == 1){ // if points are flow connected\n              Ind2[i,j] = (h[i,j] / alpha_td) <= 1 ? 1 : 0; // indicator\n              C_td[i,j] = var_td * (1 - (h[i,j] / alpha_td)) .* Ind2[i,j] ; //Tail-up linear-with-sill model\n            }\n            else{// if points are flow unconnected\n              iji[1,1] = D[i,j];\n              iji [2,1] = D[j,i];\n              Ind2[i,j] = (max(iji) / alpha_td) <= 1 ? 1 : 0; // indicator\n              C_td[i,j] = var_td * (1 - (max(iji) / alpha_td)) * Ind2[i,j] ;\n            }\n          }\n        }\n\n  "
  tparam_td2_sph <- "\n    var_td= sigma_td ^ 2; // variance tail-down\n        for (i in 1:N) {// tail-down spherical model\n          for (j in 1:N) {\n            if(flow_con_mat[i,j] == 1){ // if points are flow connected\n              Ind2[i,j] = (h[i,j] / alpha_td) <= 1 ? 1 : 0; // indicator\n              C_td[i,j] = var_td * (1 - (1.5 * h[i,j] / alpha_td) + ( (h[i,j] ^ 3) / (2 * alpha_td ^ 3))) * Ind2[i,j];\n            }\n            else{// if points are flow unconnected\n              iji[1,1] = D[i,j];\n              iji [2,1] = D[j,i];\n              Ind2[i,j] = (max(iji) / alpha_td) <= 1 ? 1 : 0; // indicator\n              C_td[i,j] = var_td * (1 - (1.5 * min(iji) / alpha_td) + ( max(iji)/(2 * alpha_td)   )) * (1 - (max(iji) / alpha_td) ) ^ 2 * Ind2[i,j];\n            }\n          }\n        }\n\n"
  tparam_ed2 <- "\n\t  //Euclidean distance models start\n    var_ed = sigma_ed ^ 2; // var Euclidean dist\n      C_ed = var_ed * exp(- 3 * e / alpha_ed); // exponential model\n    //Euclidean distance models end\n"
  tparam_re2 <- "\n    // random effect\n    var_RE1 = sigma_RE1 ^ 2;\n      C_re = var_RE1 * RE1;\n\n"
  model_com <- "\n  model {\n    for (t in 1:T){\n      target += multi_normal_cholesky_lpdf(Y[t] | mu[t], cholesky_decompose(C_tu + C_td + C_re + C_ed + var_nug * I + 1e-6) );\n    }\n\n    sigma_nug ~ uniform(0,50); // cauchy(0,1) prior nugget effect\n    phi ~ uniform(-1, 1); // or can use phi ~ normal(0.5,0.3); //NB informative\n\n"
  model_tu <- "\n    sigma_tu ~ uniform(0,100);  // or cauchy(0,2) prior sd  tail-up model\n    alpha_tu ~ uniform(0, alpha_max);\n"
  model_td <- "\n    sigma_td ~ uniform(0,100); // sd tail-down\n    alpha_td ~ uniform(0, alpha_max);\n"
  model_ed <- "\n    sigma_ed ~ uniform(0,100); // sd Euclidean dist\n    alpha_ed ~ uniform(0, alpha_max); // Euclidean dist range\n"
  model_re <- "\n    sigma_RE1 ~ uniform(0,5);\n"
  gen_quant <- "\n  generated quantities {\n   vector[T] log_lik;\n     for (t in 1:T){\n       log_lik[t] = multi_normal_cholesky_lpdf(Y[t]|mu[t],\n        cholesky_decompose(C_tu + C_td + C_re + C_ed + var_nug * I + 1e-6) );\n      }\n  }\n  "
  ssn_ar <- paste(data_com, param_com, if (cor_tu %in% 1:3) 
    param_tu, if (cor_td %in% 1:3) 
      param_td, if (cor_ed %in% 1:3) 
        param_ed, if (cor_re %in% 1:3) 
          param_re, if (time_method[[1]] == "ar") 
            param_phi_ar, if (time_method[[1]] == "var") 
              param_phi_var, "}", tparam_com, if (cor_tu %in% 1:3) 
                tparam_tu, if (cor_td %in% 1:3) 
                  tparam_td, if (cor_ed %in% 1:3) 
                    tparam_ed, if (cor_re %in% 1:3) 
                      tparam_re, tparam_com2, if (time_method[[1]] == "ar") 
                        tparam_com_ar, if (time_method[[1]] == "var") 
                          tparam_com_var, case_when(cor_tu == 1 ~ tparam_tu2_exp, 
                                                    cor_tu == 2 ~ tparam_tu2_lin, cor_tu == 3 ~ tparam_tu2_sph, 
                                                    cor_tu >= 4 | cor_tu <= 0 ~ "C_tu = rep_matrix(0, N, N);"), 
    case_when(cor_td == 1 ~ tparam_td2_exp, cor_td == 2 ~ 
                tparam_td2_lin, cor_td == 3 ~ tparam_td2_sph, cor_td >= 
                4 | cor_td <= 0 ~ "C_td = rep_matrix(0, N, N);"), 
    case_when(cor_ed == 1 ~ tparam_ed2, cor_ed >= 2 | cor_ed <= 
                0 ~ "C_ed = rep_matrix(0, N, N);"), case_when(cor_re == 
                                                                1 ~ tparam_re2, cor_re >= 2 | cor_re <= 0 ~ "C_re = rep_matrix(0, N, N);"), 
    "}", model_com, if (cor_tu %in% 1:3) 
      model_tu, if (cor_td %in% 1:3) 
        model_td, if (cor_ed %in% 1:3) 
          model_ed, if (cor_re %in% 1:3) 
            model_re, "}", if (loglik == TRUE) 
              gen_quant)
  `%notin%` <- Negate(`%in%`)
  pars <- c(
    case_when(
      cor_tu %in% 1:3 ~ c("var_tu", "alpha_tu"), 
      cor_tu %notin% 1:3 ~ ""
    ), 
    case_when(
      cor_td %in% 1:3 ~ c("var_td", "alpha_td"),
      cor_td %notin% 1:3 ~ ""
    ), 
    case_when(
      cor_ed %in% 1:3 ~ c("var_ed", "alpha_ed"), 
      cor_ed %notin% 1:3 ~ ""
    ), 
    case_when(
      cor_re %in% 1:3 ~ c("var_re", "alpha_re"), 
      cor_re %notin% 1:3 ~ ""
    ), 
    if (loglik == TRUE) "log_lik", 
            "var_nug", "beta", "phi", "y"
  )
  pars <- pars[pars != ""]
  old <- options()
  on.exit(options(old))
  options(na.action = "na.pass")
  out_list <- mylm(formula = formula, data = data)
  response <- out_list$y
  design_matrix <- out_list$X
  obs_data <- data
  ndays <- length(unique(obs_data[, names(obs_data) %in% time_points]))
  N <- nrow(obs_data)/ndays
  nobs <- nrow(obs_data)/ndays
  obs_data$date_num <- as.numeric(factor(obs_data[, names(obs_data) %in% time_points]))
  resp_var_name <- gsub("[^[:alnum:]]", " ", formula[2])
  obs_data$y <- obs_data[, names(obs_data) %in% resp_var_name]
  X <- design_matrix
  Xarray <- aperm(array(c(X), dim = c(N, ndays, ncol(X))), 
                  c(2, 1, 3))
  y_obs <- response[!is.na(response)]
  i_y_obs <- obs_data[!is.na(obs_data$y), ]$pid
  i_y_mis <- obs_data[is.na(obs_data$y), ]$pid
  if (ssn_object == TRUE) {
    mat_all <- dist_weight_mat(path = path, net = net) #, addfunccol = addfunccol
  }
  if (ssn_object == FALSE) {
    first_date <- unique(obs_data[, names(obs_data) %in% 
                                    time_points])[1]
    di <- dist(obs_data[obs_data$date == first_date, c("lon", 
                                                       "lat")], method = "euclidean", diag = FALSE, upper = FALSE) %>% 
      as.matrix()
    mat_all <- list(e = di, D = di, H = di, w.matrix = di, 
                    flow.con.mat = di)
  }
  data_list <- list(N = N, T = ndays, K = ncol(X), y_obs = y_obs, 
                    N_y_obs = length(i_y_obs), N_y_mis = length(i_y_mis), 
                    i_y_obs = i_y_obs, i_y_mis = i_y_mis, X = Xarray, mat_all = mat_all, 
                    alpha_max = 4 * max(mat_all$H))
  data_list$e = data_list$mat_all$e
  data_list$h = data_list$mat_all$H
  data_list$W = data_list$mat_all$w.matrix
  data_list$flow_con_mat = data_list$mat_all$flow.con.mat
  data_list$D = data_list$mat_all$D
  data_list$I = diag(1, nrow(data_list$W), nrow(data_list$W))
  ini <- function() {
    list(var_nug = 0.1)
  }
  fit <- rstan::stan(model_code = ssn_ar, model_name = "ssn_ar", 
                     data = data_list, pars = pars, iter = iter, warmup = warmup, 
                     init = ini, chains = chains, verbose = FALSE, seed = seed, 
                     refresh = refresh)
  attributes(fit)$formula <- formula
  class(fit) <- "ssnbayes"
  fit
# }

