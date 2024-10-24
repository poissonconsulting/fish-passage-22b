data {
  int nsite;
  int nweek;

  int <lower=0> N_y_obs; // number observed values
  int <lower=0> N_y_mis; // number missing values
  int <lower=1> i_y_obs [N_y_obs] ;  // [N_y_obs,T]
  int <lower=1> i_y_mis [N_y_mis] ;  // [N_y_mis,T]
  vector [N_y_obs] y_obs;  // matrix[N_y_obs,1] y_obs[T];
  
  real air_temp [nsite * nweek];
  int<lower=0> site[nsite * nweek];

  matrix [nsite, nsite] D;
  matrix [nsite, nsite] I;
  matrix [nsite, nsite] H;
  matrix [nsite, nsite] flow_con_mat;
}

parameters {
  vector[N_y_mis] y_mis; // declaring the missing y
  
  // Logistic equation w air temp
  real bAlphaIntercept;
  real bBetaIntercept;
  real bGammaIntercept;
  
  // Correlated random effects for alpha and beta
  // first element is alpha, second is beta, third is gamma
  vector<lower=0>[3] sLogisticPars;
  cholesky_factor_corr[3] bLRhoLogisticPars;
  matrix[3, nsite] eZLogisticPars;

  real<lower=0> sigma_nug; // sd of nugget effect
  real<lower=0> sigma_td; // sd of tail-down
  real<lower=0> alpha_td; // range of the tail-down model
}

transformed parameters {
  vector[nsite * nweek] y; // long vector of y
  vector[nsite] Y[nweek]; // array of y
  matrix[nsite, nsite] C_td; // tail-down cov
  real <lower=0> var_nug; // nugget
  real <lower=0> var_td; // partial sill tail-down
  
  matrix[nsite, 3] bLogisticPars;
  matrix[3, 3] bRhoLogisticPars;
  
  vector[nsite * nweek] eAlpha;
  vector[nsite * nweek] eBeta;
  vector[nsite * nweek] eGamma;
  
  vector[nsite * nweek] eTemp;
  vector[nsite] mu [nweek];
  vector[nweek] log_lik;
  
  y[i_y_obs] = y_obs;
  y[i_y_mis] = y_mis;
  
  var_nug = sigma_nug^2; // variance nugget
  var_td = sigma_td^2; // variance tail-down
  
  
  // Correlated random effects for logistic parameters by site
  bLogisticPars = (diag_pre_multiply(sLogisticPars, bLRhoLogisticPars) * eZLogisticPars)';
  bRhoLogisticPars = multiply_lower_tri_self_transpose(bLRhoLogisticPars);
  
  // Place observations into matrices
  for (t in 1:nweek){
    Y[t] = y[((t - 1) * nsite + 1):(t * nsite)];
  }
  // Temperature model
  for (i in 1:(nweek * nsite)) {
    eAlpha[i] = bAlphaIntercept + bLogisticPars[site[i], 1];
    eBeta[i] = bBetaIntercept + bLogisticPars[site[i], 2];
    eGamma[i] = bGammaIntercept + bLogisticPars[site[i], 3];
    eTemp[i] = eAlpha[i] / (1 + exp(eGamma[i] * (eBeta[i] - air_temp[i])));
  }
  
  // Define 1st mu
  mu[1] = eTemp[1:nsite];
  
  // Define rest of mu; ----
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
  for (t in 1:nweek) {
    log_lik[t] = multi_normal_cholesky_lpdf(Y[t] | mu[t], cholesky_decompose(C_td + var_nug * I + 1e-6)); 
  }
}

model {
  sLogisticPars ~ exponential(0.5);
  bLRhoLogisticPars ~ lkj_corr_cholesky(2);
  to_vector(eZLogisticPars) ~ std_normal();
  
  sigma_nug ~ exponential(0.05); // sd nugget
  sigma_td ~ exponential(2); // sd tail-down
  alpha_td ~ normal(0, 20000) T[0, ];
  
  bAlphaIntercept ~ normal(25, 5);
  bBetaIntercept ~ normal(10, 5);
  bGammaIntercept ~ normal(0, 1);
  
  for (t in 1:nweek) {
    target += multi_normal_cholesky_lpdf(Y[t] | mu[t], cholesky_decompose(C_td + var_nug * I + 1e-6));
  }
}
