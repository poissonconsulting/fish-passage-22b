data {
  // int nObs;
  int nsite;
  int ndate;
  // int npredictors;
  vector[nsite * ndate] doy;
  // matrix[nsite, npredictors] X[ndate] ;
  int <lower=0> N_y_obs; // number observed values
  int <lower=0> N_y_mis; // number missing values
  int <lower=1> i_y_obs[N_y_obs] ;  // [N_y_obs,T]
  int <lower=1> i_y_mis[N_y_mis] ;  // [N_y_mis,T]
  vector[N_y_obs] y_obs;  // matrix[N_y_obs,1] y_obs[T];
  
  matrix [nsite, nsite] W;
  matrix [nsite, nsite] D;
  matrix [nsite, nsite] I;
  matrix [nsite, nsite] H;
  matrix [nsite, nsite] flow_con_mat;
  matrix [nsite, nsite] E;
  
  real <lower=1> alpha_max;
}

transformed data {
  vector[nsite] DOY [ndate];

  for (t in 1:ndate) {
    DOY[t] = doy[((t - 1) * nsite + 1):(t * nsite)];
  }
}

parameters {
  vector[N_y_mis] y_mis; // declaring the missing y
  // vector[npredictors] beta;
  real bIntercept;
  real bDoy;
  real bDoy2;
  
  real<lower=0> sigma_nug; // sd of nugget effect
  real<lower=0> sigma_td; // sd of tail-down
  real<lower=0> alpha_td; // range of the tail-down model
  real<lower=0> sigma_tu; // sd of tail-up
  real<lower=0> alpha_tu; // range of tail-up model
  // real<lower=0> sigma_ed; // sd of euclidean distance model
  // real<lower=0> alpha_ed; // range of euclidean distance model
  vector <lower=-1, upper=1> [nsite] phi; // vector of autoregression pars (btwn -1 and 1 to ensure stationarity)
}

transformed parameters {
  vector[nsite * ndate] y; // long vector of y
  vector[nsite] Y[ndate]; // array of y
  vector[nsite] epsilon[ndate]; // error term
  vector[nsite] mu[ndate]; // mean
  matrix[nsite, nsite] C_tu; // tail-up cov
  matrix[nsite, nsite] C1; // tail-up cov
  matrix[nsite, nsite] C_td; // tail-down cov
  // matrix[nsite, nsite] C_ed ; // euclidean cov
  real <lower=0> var_nug; // nugget
  real <lower=0> var_td; // partial sill tail-down
  real <lower=0> var_tu; // partial sill tail-up
  // real <lower=0> var_ed; //  Euclidean dist var
  
  y[i_y_obs] = y_obs;
  y[i_y_mis] = y_mis;
  
  // Place observations into matrices
  for (t in 1:ndate){
    Y[t] = y[((t - 1) * nsite + 1):(t * nsite)];
  }

  var_nug = sigma_nug^2; // variance nugget
  var_td = sigma_td^2; // variance tail-down
  var_tu = sigma_tu^2; // variance tail-up
  // var_ed = sigma_ed^2; // variance euclidean
  
  // Define 1st mu and epsilon
  // mu[1] = X[1] * beta;
  mu[1] = exp(bIntercept + bDoy * DOY[1] + bDoy2 * DOY[1]^2);
  // mu[1] = rep_vector(bIntercept, nsite);
  epsilon[1] = Y[1] - mu[1];
  
  // Define rest of mu and epsilon; ----
  for (t in 2:ndate){
    // mu[t] = X[t] * beta;
    // mu[t] = rep_vector(bIntercept, nsite);
    mu[t] = exp(bIntercept + bDoy * DOY[t] + bDoy2 * DOY[t]^2);
    epsilon[t] = Y[t] - mu[t];
    mu[t] = mu[t] + phi .* epsilon[t - 1]; // element wise mult two vectors
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
  // C_ed = var_ed * exp(- 3 * E / alpha_ed); // exponential model
}

model {
  for (i in 1:nsite) {
    phi[i] ~ normal(0.7, 0.3) T[-1, 1]; // More informative than ssnbayes default (uniform(-1, 1))
  }
  // phi ~ uniform(-1, 1);
  
  // Notes
  sigma_nug ~ exponential(0.2);
  // sigma_nug ~ uniform(0, 50); // cauchy(0,1) prior nugget effect
  // sigma_tu ~ uniform(0, 100);
  sigma_tu ~ exponential(0.5);  // or cauchy(0,2) prior sd  tail-up model
  // alpha_tu ~ uniform(0, alpha_max);
  alpha_tu ~ exponential(0.1);
  // sigma_td ~ uniform(0, 100);
  sigma_td ~ exponential(0.5); // sd tail-down
  // alpha_td ~ uniform(0, alpha_max);
  alpha_td ~ exponential(0.1);
  // sigma_ed ~ uniform(0, 100); 
  // sigma_ed ~ exponential(1); // sd Euclidean dist
  // alpha_ed ~ uniform(0, alpha_max); // Euclidean dist range
  
  // beta ~ normal(0, 100); // prior for beta
  
  bIntercept ~ normal(0, 2);
  bDoy ~ normal(0, 2);
  bDoy2 ~ normal(0, 2);
    
  for (t in 1:ndate) {
    // target += multi_normal_cholesky_lpdf(Y[t] | mu[t], cholesky_decompose(C_tu + C_td + C_ed + var_nug * I + 1e-6));
     target += multi_normal_cholesky_lpdf(Y[t] | mu[t], cholesky_decompose(C_tu + C_td + var_nug * I + 1e-6));
  }
} 

generated quantities {
  vector[ndate] log_lik;
  for (t in 1:ndate){
    // log_lik[t] = multi_normal_cholesky_lpdf(Y[t] | mu[t], cholesky_decompose(C_tu + C_td + C_ed + var_nug * I + 1e-6));
    log_lik[t] = multi_normal_cholesky_lpdf(Y[t] | mu[t], cholesky_decompose(C_tu + C_td + var_nug * I + 1e-6));
  }
}
