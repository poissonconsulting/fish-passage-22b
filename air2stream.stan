functions {
  int bin_search(real x, int min_val, int max_val){
    int range = (max_val - min_val + 1) %/% 2; 
    int mid_pt = min_val + range;
    int out;
    while(range > 0) {
      if(x == mid_pt){
        out = mid_pt;
        range = 0;
      } else {
        range =  (range + 1) %/% 2; 
        mid_pt = x > mid_pt ? mid_pt + range: mid_pt - range; 
      }
    }
    return out;
  }
  vector air2water(real t,        
                  vector tw0,      
                  real a1, 
                  real a2, 
                  real a3, 
                  real a4, 
                  real a5, 
                  real a6, 
                  vector[] air_temp,
                  int nannual) { 
         
    vector[nannual] dydt;
    real Th = 4;
    real delta;
    int t_int; 
    t_int = bin_search(round(t), 1, 367); 
    
    for(i in 1:nannual){
      real Tw = tw0[i]; 
      real Ta = air_temp[t_int, i];
      if(Tw >= Th){
        delta = exp(-(Tw - Th) / a4);
      } else {
        delta = 1;
      }
      dydt[i] =  (1/delta) * (a1 + a2 * Ta - a3 * Tw + a5 * cos(2 * 3.141592654 * (t / 365 - a6)));
    }
    return dydt;
    }
}
data {
  int<lower = 0> ntime;
  int<lower = 0> ntime1;
  int<lower = 0> nannual;
  array[ntime] vector[nannual] water_temp; 
  array[ntime1] vector[nannual] air_temp; 
  vector[nannual] tw0;
  real t0;
  real t[ntime];
}
parameters {
  real<lower = 0> sigma; 
  real<lower = -0.5, upper = 2> a1; 
  real<lower = 0.001,  upper = 0.2> a2; 
  real<lower = 0.001, upper = 0.5> a3; 
  real<lower = 1, upper = 25> a4; 
  real<lower = 0.0001, upper = 30> a5 ; 
  real<lower = 0, upper = 1> a6; 
}
model {
  array[ntime] vector[nannual] eTw;
  
  eTw = ode_rk45(
    air2water, tw0, t0, t,
    a1, a2, a3, a4, a5, a6, air_temp, nannual);
    
  sigma ~ exponential(1);
  // range -0.5, 2
  a1 ~ normal(0.2, 0.1);
  // range 0.001, 0.1
  a2 ~ normal(0.05, 0.02);
  // range 0.001, 0.5
  a3 ~ normal(0.1, 0.05);
  // range 1, 25
  a4 ~ normal(10, 2);
  // range 0.0001, 30
  a5 ~ normal(0.1, 5);
  // range 0,1
  a6 ~ beta(4, 2);
  for (i in 1:ntime) {
    for(n in 1:nannual){
      water_temp[i, n] ~ normal(eTw[i, n], sigma);
    }
  }
}
generated quantities {
  array[ntime] vector[nannual] eTw;
  
  eTw = ode_rk45(
    air2water, tw0, t0, t,
    a1, a2, a3, a4, a5, a6, air_temp, nannual);
  array[ntime] vector[nannual] simTw;
  for (i in 1:ntime) {
    for(n in 1:nannual){
      simTw[i, n] = normal_rng(eTw[i, n], sigma);
    }
  }
}