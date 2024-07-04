data {
    int<lower=1> N;
    int<lower=1> K;
    int<lower=1> T;
    matrix[N,K] X[T] ; // real X[N,K,T]; //
    int<lower = 0> N_y_obs; // number observed values
    int<lower = 0> N_y_mis; // number missing values
    int<lower = 1> i_y_obs[N_y_obs] ;  //[N_y_obs,T]
    int<lower = 1> i_y_mis[N_y_mis] ;  // N_y_mis,T]
    vector[N_y_obs] y_obs;    //matrix[N_y_obs,1] y_obs[T];
    matrix[N, N] W ; // spatial weights
    matrix[N, N] h ; // total hydrological dist
    matrix[N, N] I ; // diag matrix
    matrix[N, N] D ; // downstream hydrological dist matrix
    matrix[N, N] flow_con_mat; // flow conected matrix
    matrix[N, N] e ; // Euclidean dist mat
    real<lower=1> alpha_max ;
  }
  parameters {
    vector[K] beta;
    real<lower=0> sigma_nug;
    vector[N_y_mis] y_mis;//declaring the missing y

    real<lower=0> sigma_td; // sd of tail-down
    real<lower=0> alpha_td; // range of the tail-down model

    real <lower=-1, upper = 1> phi; // NB
    }
  transformed parameters {
    vector[N * T] y;
    vector[N] Y[T];
    vector[N] epsilon[T]; // error term
    vector[N] mu[T]; // mean
   real<lower=0> var_nug; // nugget
   matrix[N, N] C_tu; //tail-up cov
   matrix[N, N] C1; //tail-up cov
   matrix[N, N] Ind; //tail-up indicator
   matrix[N, N] C_td; //tail-down cov
   matrix[N, N] Ind2; //tail-down indicator
   matrix[2,1] iji;
   matrix[N, N] C_ed ;// Euclidean cov
   matrix[N, N] C_re ;// random effect cov
   matrix[N, N] RE1; // random effect 1

    real<lower=0> var_td; // parsil tail-down

    y[i_y_obs] = y_obs;
    y[i_y_mis] = y_mis;
    for (t in 1:T){
      Y[t] = y[((t - 1) * N + 1):(t * N)];
    }
    var_nug = sigma_nug ^ 2; // variance nugget
        mu[1] = X[1] * beta;
    epsilon[1] = Y[1] - mu[1];

        for (t in 2:T){
        mu[t] = X[t] * beta;
        epsilon[t] = Y[t] - mu[t];
        mu[t] = mu[t] + phi * epsilon[t-1]; //
    }
    C_tu = rep_matrix(0, N, N);
    var_td= sigma_td ^ 2; // variance tail-down
     	for (i in 1:N) {// Tail-down exponential model
          for (j in 1:N) {
            if(flow_con_mat[i,j] == 1){ // if points are flow connected
               C_td[i,j] = var_td * exp(- 3 * h[i,j] / alpha_td);
            }
            else{// if points are flow unconnected
              C_td[i,j] = var_td * exp(- 3 * (D[i,j] + D[j,i]) / alpha_td);
            }
          }
        }
 C_ed = rep_matrix(0, N, N); C_re = rep_matrix(0, N, N); }

model {
    for (t in 1:T){
      target += multi_normal_cholesky_lpdf(Y[t] | mu[t], cholesky_decompose(C_tu + C_td + C_re + C_ed + var_nug * I + 1e-6) );
    }
    sigma_nug ~ uniform(0,50); // cauchy(0,1) prior nugget effect
    phi ~ uniform(-1, 1); // or can use phi ~ normal(0.5,0.3); //NB informative

    sigma_td ~ uniform(0,100); // sd tail-down
    alpha_td ~ uniform(0, alpha_max);
  
} 