data {
  int nsite;
  int nweek;

  int <lower=0> N_y_obs; // number observed values
  int <lower=0> N_y_mis; // number missing values
  int <lower=1> i_y_obs [N_y_obs] ;  // [N_y_obs,T]
  int <lower=1> i_y_mis [N_y_mis] ;  // [N_y_mis,T]
  vector [N_y_obs] y_obs;  // matrix[N_y_obs,1] y_obs[T];
  
  real net_solar_rad [nsite * nweek];
  real discharge [nsite * nweek];
  real elev [nsite * nweek];
  real air_temp [nsite * nweek];

  matrix [nsite, nsite] W;
  matrix [nsite, nsite] D;
  matrix [nsite, nsite] I;
  matrix [nsite, nsite] H;
  matrix [nsite, nsite] flow_con_mat;
  matrix [nsite, nsite] E;
}

parameters {
  vector[N_y_mis] y_mis; // declaring the missing y
  
  // Logistic w air temp
  real bAlpha;
  real bGamma;
  real bBeta;
  real bMu;

  real<lower=0> sigma_nug; // sd of nugget effect
  real<lower=0> sigma_td; // sd of tail-down
  real<lower=0> alpha_td; // range of the tail-down model
  real<lower=0> sigma_tu; // sd of tail-up
  real<lower=0> alpha_tu; // range of tail-up model
  real<lower=0> sigma_ed; // sd of euclidean distance model
  real<lower=0> alpha_ed; // range of euclidean distance model
  vector <lower=-1, upper=1> [nsite] phi; // vector of autoregression pars (btwn -1 and 1 to ensure stationarity)
}

transformed parameters {
  vector[nsite * nweek] y; // long vector of y
  vector[nsite] Y[nweek]; // array of y
  matrix[nsite, nsite] C_tu; // tail-up cov
  matrix[nsite, nsite] C1; // tail-up cov
  matrix[nsite, nsite] C_td; // tail-down cov
  matrix[nsite, nsite] C_ed ; // euclidean cov
  real <lower=0> var_nug; // nugget
  real <lower=0> var_td; // partial sill tail-down
  real <lower=0> var_tu; // partial sill tail-up
  real <lower=0> var_ed; // euclidean dist var
  
  vector[nsite * nweek] eTemp;
  vector[nsite] mu [nweek];
  vector[nweek] eLogLik;
  
  y[i_y_obs] = y_obs;
  y[i_y_mis] = y_mis;
  
  // Place observations into matrices
  for (t in 1:nweek){
    Y[t] = y[((t - 1) * nsite + 1):(t * nsite)];
  }

  var_nug = sigma_nug^2; // variance nugget
  var_td = sigma_td^2; // variance tail-down
  var_tu = sigma_tu^2; // variance tail-up
  var_ed = sigma_ed^2; // variance euclidean
  
  // Temperature model
  for (i in 1:(nweek * nsite)) {
    eTemp[i] = bMu + (bAlpha - bMu) / (1 + exp(bGamma * (bBeta - air_temp[i])));
  }
  
  // Define 1st mu and epsilon
  mu[1] = eTemp[1:nsite];
  
  // Define rest of mu and epsilon; ----
  for (t in 2:nweek){
    mu[t] = eTemp[((t - 1) * nsite + 1):(t * nsite)];
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
    
  // Tail up exponential model
  C1 = var_tu * exp(- 3 * H / alpha_tu); // tail up exponential model
  C_tu = C1 .* W; // Hadamard (element-wise) product
  
  // Euclidean exponential model
  C_ed = var_ed * exp(- 3 * E / alpha_ed); // exponential model
  
  // Save log-likelihood
  for (t in 1:nweek) {
    eLogLik[t] = multi_normal_cholesky_lpdf(Y[t] | mu[t], cholesky_decompose(C_tu + C_td + C_ed + var_nug * I + 1e-6));
  }
}

model {
  sigma_nug ~ exponential(0.1); // sd nugget
  sigma_tu ~ exponential(0.5); // sd tail-up
  alpha_tu ~ normal(0, 10000) T[0, ];
  sigma_td ~ exponential(2); // sd tail-down
  alpha_td ~ normal(0, 10000) T[0, ];
  sigma_ed ~ exponential(1); // sd euclidean dist
  alpha_ed ~ normal(0, 10000) T[0, ];
  
  bAlpha ~ normal(25, 5);
  bGamma ~ normal(0, 1);
  bBeta ~ normal(10, 5);
  bMu ~ normal(0, 0.5);
  
  for (t in 1:nweek) {
    target += multi_normal_cholesky_lpdf(Y[t] | mu[t], cholesky_decompose(C_tu + C_td + C_ed + var_nug * I + 1e-6));
  }
}
