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

  matrix [nsite, nsite] D;
  matrix [nsite, nsite] I;
  matrix [nsite, nsite] H;
  matrix [nsite, nsite] flow_con_mat;
}

parameters {
  vector<lower=0, upper=30>[N_y_mis] y_mis; // declaring the missing y

  real<lower=0> sigma_nug; // sd of nugget effect
  real<lower=0> sigma_td; // sd of tail-down
  real<lower=0> alpha_td; // range of the tail-down model
  
  real<lower=0> bInitialTemp;
  
  real<lower=0> s1;
  real<lower=0> s2;
  real<lower=0> s3;
  real<lower=0> s4;
  
  real<lower=-5, upper=15> m1;
  real<lower=-5, upper=1.5> m2;
  real<lower=-5, upper=5> m3; 
  real<lower=-1, upper=1> m4;
  
  real<lower=-5, upper=15> a1[nsite];
  real<lower=-5, upper=1.5> a2[nsite]; 
  real<lower=-5, upper=5> a3[nsite]; 
  real<lower=-1, upper=1> a4[nsite];
}

transformed parameters {
  vector[nsite * nweek] y; // long vector of y
  vector[nsite] Y[nweek]; // array of y
  matrix[nsite, nsite] C_td; // tail-down cov
  real <lower=0> var_nug; // nugget
  real <lower=0> var_td; // partial sill tail-down

  vector[nsite * nweek] eTempDiff;
  vector<lower=0, upper=30>[nsite * nweek] eTemp;
  vector[nsite] mu [nweek];
  
  y[i_y_obs] = y_obs;
  y[i_y_mis] = y_mis;
  
  var_nug = sigma_nug^2; // variance nugget
  var_td = sigma_td^2; // variance tail-down
  
  // Place observations into matrices
  for (t in 1:nweek){
    Y[t] = y[((t - 1) * nsite + 1):(t * nsite)];
  }
  
  eTemp[1:nsite] = rep_vector(bInitialTemp, nsite);
  
  for (i in (nsite + 1):(nweek * nsite)) {
    eTempDiff[i] = (1/(discharge[i]^a4[site[i]])) * (a1[site[i]] + a2[site[i]] * air_temp[i] - a3[site[i]] * eTemp[i - nsite]);
    
    eTemp[i] = eTemp[i - nsite] + eTempDiff[i];
    if (eTemp[i] < 0) {
      eTemp[i] = 0.0;
    }
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
}

model {
  sigma_nug ~ exponential(0.05); // sd nugget
  sigma_td ~ exponential(2); // sd tail-down
  alpha_td ~ normal(0, 20000) T[0, ]; // range tail-down
  
  bInitialTemp ~ normal(0, 0.1) T[0, ];

  s1 ~ exponential(50);
  s2 ~ exponential(50);
  s3 ~ exponential(50);
  s4 ~ exponential(50);
  
  m1 ~ normal(0.8, 1);
  m2 ~ normal(0.4, 1);
  m3 ~ normal(0.4, 1);
  m4 ~ normal(0.1, 1);
  
  a1 ~ normal(m1, s1);
  a2 ~ normal(m2, s2);
  a3 ~ normal(m3, s3);
  a4 ~ normal(m4, s4);

  for (t in 1:nweek) {
    target += multi_normal_cholesky_lpdf(Y[t] | mu[t], cholesky_decompose(C_td + var_nug * I + 1e-6));
  }
}
