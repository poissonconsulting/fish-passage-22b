data {
  int <lower=0> nObs;
  int <lower=0> nsite;
  
  int <lower=0> site[nObs];
  real discharge[nObs];
  // real baseline[nObs];
  // real upstream_area[nObs];
  // real precip_vol[nObs];
  real precip_vol_lag[nObs];
  // real snowmelt_vol[nObs];
  real snowmelt_vol_lag[nObs];
  // real lag_us_precip[nObs];
  // real lag_us_snowmelt[nObs];
}

parameters {
  // real bInterceptBaseline;
  // real bAreaBaseline;
  // real bPrecip;
  // real bSnowmelt;
  real bIntercept;
  real bPrecipLag;
  real bSnowmeltLag;
  real <lower=0> sDischarge;
  // real <lower=0> sBaseline;
}

model {
  // real eBaseline[nsite];
  real eDischarge[nObs];
  
  // bInterceptBaseline ~ normal(0, 2);
  // bAreaBaseline ~ normal(0, 2);
  
  // // bPrecip ~ normal(0, 2);
  bPrecipLag ~ normal(0, 2);
  // // bSnowmelt ~ normal(0, 2);
  bSnowmeltLag ~ normal(0, 2);
  // sDischarge ~ exponential(0.001);
  bIntercept ~ normal(0, 2);
  // sBaseline ~ exponential(1);
  
  // for (i in 1:nsite) {
  //   eBaseline[i] = bInterceptBaseline + bAreaBaseline * log(upstream_area[i]);
  // }
  
  for (i in 1:nObs) {
    // baseline[i] ~ lognormal(eBaseline[site[i]], sBaseline);
    // eDischarge[i] = exp(eBaseline[site[i]] + bPrecipLag * precip_vol_lag[i] + bSnowmeltLag * snowmelt_vol_lag[i]);
    eDischarge[i] = exp(bIntercept + bPrecipLag * log(precip_vol_lag[i]) + bSnowmeltLag * log(snowmelt_vol_lag[i]));
    discharge[i] ~ normal(eDischarge[i], sDischarge) T[0, ];
  }
}
