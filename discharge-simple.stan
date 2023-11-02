data {
  int <lower=0> nObs;
  
  real discharge[nObs];
  real precip[nObs];
}

parameters {
  real bInterceptDischarge;
  real bPrecip;
  real <lower=0> sDischarge;
}

model {
  real eDischarge[nObs];
  
  bInterceptDischarge ~ normal(0, 2);
  bPrecip ~ normal(0, 2);
  sDischarge ~ exponential(1);
  
  for (i in 1:nObs) {
    eDischarge[i] = exp(bInterceptDischarge + bPrecip * precip[i]);
    discharge[i] ~ normal(eDischarge[i], sDischarge); 
  }
} 
