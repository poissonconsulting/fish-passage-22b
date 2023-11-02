data {
  int nsite;
  int nweek;

  int <lower=0> N_y_obs; // number observed values
  int <lower=0> N_y_mis; // number missing values
  int <lower=1> i_y_obs [N_y_obs] ;  // [N_y_obs,T]
  int <lower=1> i_y_mis [N_y_mis] ;  // [N_y_mis,T]
  vector [N_y_obs] y_obs;  // matrix[N_y_obs,1] y_obs[T];
  
  // real air_temp [nsite * nweek];
  // real evap_water [nsite * nweek];
  // real evap_total [nsite * nweek];
  // real snowmelt [nsite * nweek];
  // real runoff_subsurface [nsite * nweek];
  // real runoff_surface [nsite * nweek];
  // real net_solar_rad [nsite * nweek];
  // real precip [nsite * nweek];
  // real soil_vol_1 [nsite * nweek];
  
  // real us_evap_water[nsite * nweek];
  // real us_snowmelt[nsite * nweek];
  // real us_precip[nsite * nweek];
  // real us_runoff_surface[nsite * nweek];
  // real upstream_area[nsite * nweek];
  
  real precip [nsite * nweek];

  matrix [nsite, nsite] W;
  matrix [nsite, nsite] D;
  matrix [nsite, nsite] I;
  matrix [nsite, nsite] H;
  matrix [nsite, nsite] flow_con_mat;
  matrix [nsite, nsite] E;
}

parameters {
  vector[N_y_mis] y_mis; // declaring the missing y
  
  real bInterceptDischarge;
  real bPrecip;
  
  real<lower=0> sigma_nug; // sd of nugget effect
  // real<lower=0> sigma_td; // sd of tail-down
  // real<lower=0> alpha_td; // range of the tail-down model
  real<lower=0> sigma_tu; // sd of tail-up
  real<lower=0> alpha_tu; // range of tail-up model
  // real<lower=0> sigma_ed; // sd of euclidean distance model
  // real<lower=0> alpha_ed; // range of euclidean distance model
  real <lower=-1, upper=1> phi; // autoregression pars (btwn -1 and 1 to ensure stationarity)
}

transformed parameters {
  vector[nsite * nweek] y; // long vector of y
  vector[nsite] Y[nweek]; // array of y
  vector[nsite] epsilon[nweek]; // error term
  matrix[nsite, nsite] C_tu; // tail-up cov
  matrix[nsite, nsite] C1; // tail-up cov
  // matrix[nsite, nsite] C_td; // tail-down cov
  // matrix[nsite, nsite] C_ed ; // euclidean cov
  real <lower=0> var_nug; // nugget
  // real <lower=0> var_td; // partial sill tail-down
  real <lower=0> var_tu; // partial sill tail-up
  // real <lower=0> var_ed; // euclidean dist var
  
  // vector[nsite * nweek] eRunoff;
  vector[nsite * nweek] eDischarge;
  vector[nsite] mu [nweek];
  
  y[i_y_obs] = y_obs;
  y[i_y_mis] = y_mis;
  
  // Place observations into matrices
  for (t in 1:nweek){
    Y[t] = y[((t - 1) * nsite + 1):(t * nsite)];
  }

  var_nug = sigma_nug^2; // variance nugget
  // var_td = sigma_td^2; // variance tail-down
  var_tu = sigma_tu^2; // variance tail-up
  // var_ed = sigma_ed^2; // variance euclidean
  
  
  // Temperature model
  for (i in 1:(nweek * nsite)) {
    eDischarge[i] = exp(bInterceptDischarge + bPrecip * precip[i]);
  }
  
  // Define 1st mu and epsilon
  mu[1] = eDischarge[1:nsite];
  epsilon[1] = Y[1] - mu[1];
  
  // Define rest of mu and epsilon; ----
  for (t in 2:nweek){
    mu[t] = eDischarge[((t - 1) * nsite + 1):(t * nsite)];
    epsilon[t] = Y[t] - mu[t];
    mu[t] = mu[t] + phi * epsilon[t - 1]; 
  }
  
  // Covariance matrices ----
  // Tail-down exponential model
 // 	for (i in 1:nsite) { 
 //    for (j in 1:nsite) {
 //      if (flow_con_mat[i, j] == 1) { // if points are flow connected 
 //        C_td[i, j] = var_td * exp(-3 * H[i, j] / alpha_td);
 //      }
 //      else{ // if points are flow unconnected
 //        C_td[i, j] = var_td * exp(-3 * (D[i, j] + D[j, i]) / alpha_td);
 //      }
 //    }
 //  }
    
  // Tail up exponential model
  C1 = var_tu * exp(- 3 * H / alpha_tu); // tail up exponential model
  C_tu = C1 .* W; // Hadamard (element-wise) product
  
  // Euclidean exponential model
  // C_ed = var_ed * exp(- 3 * E / alpha_ed); // exponential model
}

model {
  phi ~ normal(0.7, 0.3) T[-1, 1]; // More informative than ssnbayes default (uniform(-1, 1))
  
  sigma_nug ~ exponential(0.1); // sd nugget
  sigma_tu ~ exponential(0.01);  // sd tail-up
  alpha_tu ~ normal(0, 5000) T[0, ];
  // sigma_td ~ exponential(1); // sd tail-down
  // alpha_td ~ normal(0, 1000) T[0, ];
  // sigma_ed ~ exponential(1); // sd euclidean dist
  // alpha_ed ~ normal(0, 1000) T[0, ];
  
  bInterceptDischarge ~ normal(0, 2);
  bPrecip ~ normal(0, 2);
  
  for (t in 1:nweek) {
    // Removed C_ed and C_td
    target += multi_normal_cholesky_lpdf(Y[t] | mu[t], cholesky_decompose(C_tu + var_nug * I + 1e-6));
  }
} 

generated quantities {
  vector[nweek] log_lik;
  for (t in 1:nweek){
    // Removed C_ed and C_td
    log_lik[t] = multi_normal_cholesky_lpdf(Y[t] | mu[t], cholesky_decompose(C_tu + var_nug * I + 1e-6));
  }
}
