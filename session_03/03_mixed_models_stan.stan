data {
  int<lower=1> n_subjects;  // number of subjects
  int<lower=1> n_obs;      // number of observations
  
  int id[n_obs];       // subject id
  real dose[n_obs];   // applied doses
  real hours[n_obs];   // hours of sleep
}

parameters {
  vector[n_subjects] baseline;   // normal numbrer of sleep hours (by subject)
  real <lower = 0> coef;    // impact of the dose
  real <lower = 0> sigma;   // observation error
  
  real <lower = 6, upper = 9> mu0;      // mean for sleeping time prior
  real <lower = 0> sigma0;   // sd for sleeping time prior
  
}

model {
  // priors
  sigma0 ~ cauchy(0, 5);
  coef ~ normal(0, 2);      // slightly informative prior (note coef > 0)
  sigma ~ normal(1, 2);     // slightly informative prior
  
  // modelo (verosimilitud)
  baseline ~ normal(mu0, sigma0);     
  for (i in 1:n_obs){
      hours[i] ~ normal(baseline[id[i]] + coef * dose[i], sigma);
  }
}