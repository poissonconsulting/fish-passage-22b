data {
  int nsite;
  int nweek;

  int <lower=0> N_y_obs; // number observed values
  int <lower=0> N_y_mis; // number missing values
  int <lower=1> i_y_obs [N_y_obs] ;  // [N_y_obs,T]
  int <lower=1> i_y_mis [N_y_mis] ;  // [N_y_mis,T]
  vector [N_y_obs] y_obs;  // matrix[N_y_obs,1] y_obs[T];
  
  real discharge [nsite * nweek];
  real air_temp [nsite * nweek];
  int<lower=0> site[nsite * nweek];
  int<lower=0> week[nsite * nweek];
  int<lower=0> week_year[nsite * nweek];
  int<lower=0> max_week_year[nsite * nweek];

  matrix [nsite, nsite] D;
  matrix [nsite, nsite] I;
  matrix [nsite, nsite] H;
  matrix [nsite, nsite] flow_con_mat;
}

parameters {
  vector<lower=0, upper=30>[N_y_mis] y_mis; // declaring the missing y

  real<lower=0> sigma_nug; // sd of nugget effect
  // real<lower=0> sigma_td; // sd of tail-down
  // real<lower=0> alpha_td; // range of the tail-down model
  
  real<lower=0> bInitialTemp;
  
  // real<lower=-1, upper=1> phi; // autoregression par (btwn -1 and 1 to ensure stationarity)
  
  real<lower=-5, upper=15> a1;
  real<lower=0, upper=1.5> a2; // changed lower to 0 from -5
  real<lower=0, upper=5> a3; // changed lower to 0 from -5
  real<lower=-1, upper=1> a4;
  real<lower=0, upper=20> a5;
  real<lower=0, upper=10> a6;
  real<lower=0, upper=1> a7;
  real<lower=0, upper=5> a8; // changed lower to 0 from -1
}

transformed parameters {
  vector[nsite * nweek] y; // long vector of y
  vector[nsite] Y[nweek]; // array of y
  // vector[nsite] epsilon[nweek]; // error term
  // matrix[nsite, nsite] C_td; // tail-down cov
  real <lower=0> var_nug; // nugget
  // real <lower=0> var_td; // partial sill tail-down

  vector[nsite * nweek] eTempDiff;
  vector<lower=0, upper=30>[nsite * nweek] eTemp; // replaced upper bound = 30
  vector[nsite] mu [nweek];
  
  y[i_y_obs] = y_obs;
  y[i_y_mis] = y_mis;
  
  var_nug = sigma_nug^2; // variance nugget
  // var_td = sigma_td^2; // variance tail-down
  
  // Place observations into matrices
  for (t in 1:nweek){
    Y[t] = y[((t - 1) * nsite + 1):(t * nsite)];
  }
  
  eTemp[1:nsite] = rep_vector(bInitialTemp, nsite);
  
  for (i in (nsite + 1):(nweek * nsite)) {
    // eTempDiff[i] = (1/(discharge[i]^a4)) * (a1 + a2 * air_temp[i] - a3 * eTemp[i - nsite] + discharge[i] * (a5 + a6 * cos(2 * 3.141592654 * ((week_year[i] / max_week_year[i]) - a7)) - a8 * eTemp[i - nsite]));

    eTempDiff[i] = (1/(discharge[i]^a4)) * (a1 + a2 * air_temp[i] - a3 * eTemp[i - nsite]);
    
    eTemp[i] = eTemp[i - nsite] + eTempDiff[i];
    if (eTemp[i] < 0) {
      eTemp[i] = 0.0;
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
}

model {
  // phi ~ normal(0.7, 0.3) T[-1, 1];
  
  sigma_nug ~ exponential(0.05); // sd nugget
  // sigma_td ~ exponential(2); // sd tail-down
  // alpha_td ~ normal(0, 20000) T[0, ];
  
  bInitialTemp ~ normal(1, 0.5) T[0, ];
  
  // // try tight priors on msc student's values
  // a1 ~ normal(2.3, 0.01);
  // a2 ~ normal(0.4, 0.01);
  // a3 ~ normal(0.5, 0.01);
  // a4 ~ normal(-1, 0.01);
  // a5 ~ normal(3, 0.01);
  // a6 ~ normal(8, 0.01);
  // a7 ~ beta(40, 20);
  // a8 ~ normal(0.6, 0.01);
  
  // nh converging priors
  // a1 ~ normal(0.2, 0.1);
  // a2 ~ normal(0.05, 0.1);
  // a3 ~ normal(0.1, 0.1);
  // a4 ~ normal(0.6, 0.1);
  // a5 ~ normal(4, 0.1);
  // a6 ~ normal(3, 0.1);
  // a7 ~ beta(4, 2);
  // a8 ~ normal(0, 0.1);
  
  // good predictions priors
  a1 ~ normal(0.5, 0.1);
  a2 ~ normal(0.2, 0.1);
  a3 ~ normal(2, 0.1);
  a4 ~ normal(0, 0.1);
  a5 ~ normal(4, 0.1);
  a6 ~ normal(3, 0.1);
  a7 ~ beta(4, 2);
  a8 ~ normal(2, 0.1);

  for (t in 1:nweek) {
  //   target += multi_normal_cholesky_lpdf(Y[t] | mu[t], cholesky_decompose(C_td + var_nug * I + 1e-6));
    target += multi_normal_lpdf(Y[t] | mu[t], diag_matrix(rep_vector(var_nug, nsite)));
  }
}
