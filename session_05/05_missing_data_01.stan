data {
  int n;
  vector[n] obs;
  int idx_obs[n];
  int<lower = 1> n_preds;
  int idx_preds[n_preds];
}

parameters {
  real<lower = -1, upper = 1> a;
  real<lower = 0> sigma;
  
  vector[n_preds] preds;
}

transformed parameters{
  vector[n + n_preds] y;
  y[idx_obs]   = obs;
  y[idx_preds] = preds;
}

model {
  y[1] ~ normal(0, sigma);
  for (i in 2:(n+n_preds))
    y[i] ~ normal(a * y[i-1], sigma);
}

