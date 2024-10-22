data {
  int nsite;
  int nweek;

  int <lower=0> N_y_obs; // number observed values
  int <lower=0> N_y_mis; // number missing values
  int <lower=1> i_y_obs [N_y_obs] ;  // [N_y_obs,T]
  int <lower=1> i_y_mis [N_y_mis] ;  // [N_y_mis,T]
  vector [N_y_obs] y_obs;  // matrix[N_y_obs,1] y_obs[T];
  
  real solar_rad [nsite * nweek];
  real discharge [nsite * nweek];
  real longwave_rad [nsite * nweek];
  int<lower=0> site[nsite * nweek];

  matrix [nsite, nsite] D;
  matrix [nsite, nsite] I;
  matrix [nsite, nsite] H;
  matrix [nsite, nsite] flow_con_mat;
}

parameters {
  vector[N_y_mis] y_mis; // declaring the missing y

  real<lower=0> sigma_nug; // sd of nugget effect
  real<lower=0> sigma_td; // sd of tail-down
  real<lower=0> alpha_td; // range of the tail-down model
  
  real<lower=0> bInitialTemp;
  real<lower=0> bInitialIceReserve;
  real<lower=0> bDischarge;
  real<lower=0> bHeatLoss;
  real<lower=0> bShortwave;
  real<lower=0> bLongwave;
  
  // real<lower=-1, upper=1> phi; // autoregression par (btwn -1 and 1 to ensure stationarity)
}

transformed parameters {
  vector[nsite * nweek] y; // long vector of y
  vector[nsite] Y[nweek]; // array of y
  // vector[nsite] epsilon[nweek]; // error term
  matrix[nsite, nsite] C_td; // tail-down cov
  real <lower=0> var_nug; // nugget
  real <lower=0> var_td; // partial sill tail-down

  vector[nsite * nweek] eShortwave;
  vector[nsite * nweek] eLongwave;
  vector[nsite * nweek] eHeatLoss;
  vector[nsite * nweek] eDischarge;
  vector[nsite * nweek] eVolume;
  vector[nsite * nweek] eIceReserve;
  vector[nsite * nweek] eTempDiff;
  vector[nsite * nweek] eTemp;
  vector[nsite] mu [nweek];
  
  y[i_y_obs] = y_obs;
  y[i_y_mis] = y_mis;
  
  var_nug = sigma_nug^2; // variance nugget
  var_td = sigma_td^2; // variance tail-down
  
  // Place observations into matrices
  for (t in 1:nweek){
    Y[t] = y[((t - 1) * nsite + 1):(t * nsite)];
  }
  
  eTemp[1:nsite] = rep_vector(bInitialTemp, nsite); // could vary by site? see if there's enough info to estimate it.
  eIceReserve[1:nsite] = rep_vector(bInitialIceReserve, nsite);
  
  for (i in (nsite + 1):(nweek * nsite)) {
    eShortwave[i] = bShortwave * solar_rad[i];
    eLongwave[i] = bLongwave * longwave_rad[i];
    eHeatLoss[i] = bHeatLoss * 0.004727079 * (eTemp[i - nsite] + 273.15)^4;
    eDischarge[i] = bDischarge * 41.8e6 * discharge[i];
    // eGroundwater[i] = bGroundwater[site[i]] * mad[i]; // bGroundwater is a proportion
    eTempDiff[i] = (eShortwave[i] + eLongwave[i] - eHeatLoss[i]) / eDischarge[i];
    
    if ((eTemp[i - nsite] + eTempDiff[i]) <= 0.0) { 
      eIceReserve[i] = eIceReserve[i - nsite] + eTempDiff[i]; // the amount of temp diff below 0
      eTemp[i] = eTemp[i - nsite]; // could be 0.0
    } else {
      if (eIceReserve[i - nsite] > 0.0) {
        eIceReserve[i] = eIceReserve[i - nsite] - eTempDiff[i];
        eTemp[i] = eTemp[i - nsite];
      } else {
        eIceReserve[i] = 0.0;
        eTemp[i] = eTemp[i - nsite] + eTempDiff[i];
      }
    }    
  }
  
  // Define 1st mu
  mu[1] = eTemp[1:nsite];
  // epsilon[1] = Y[1] - mu[1];
  
  // Define rest of mu; ----
  for (t in 2:nweek){
    mu[t] = eTemp[((t - 1) * nsite + 1):(t * nsite)];
    // epsilon[t] = Y[t] - mu[t];
    // mu[t] = mu[t] + phi .* epsilon[t - 1]; // element-wise mult. two vectors
  }
  
  // Covariance matrices ----
  // Tail-down exponential model
 	for (i in 1:nsite) { 
    for (j in 1:nsite) {
      if (flow_con_mat[i, j] == 1) { // if points are flow connected 
        C_td[i, j] = var_td * exp(-3 * H[i, j] / alpha_td);
      }
      else{ // if points are flow unconnected
        C_td[i, j] = var_td * exp(-3 * (D[i, j] + D[j, i]) / alpha_td);
      }
    }
  }
}

model {
  // phi ~ normal(0.7, 0.3) T[-1, 1];
  
  sigma_nug ~ exponential(0.05); // sd nugget
  sigma_td ~ exponential(2); // sd tail-down
  alpha_td ~ normal(0, 20000) T[0, ];
  
  bInitialTemp ~ normal(1, 0.5) T[0, ]; // first week is in January!
  bInitialIceReserve ~ normal(1e3, 1e2) T[0, ]; // first week is in January!
  
  bHeatLoss ~ exponential(1); 
  bDischarge ~ exponential(1); 
  bShortwave ~ exponential(1); 
  bLongwave ~ exponential(1); 
  
  for (t in 1:nweek) {
    target += multi_normal_cholesky_lpdf(Y[t] | mu[t], cholesky_decompose(C_td + var_nug * I + 1e-6));
  }
}
