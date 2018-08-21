//basicament el que tenim es que es dades, missings i els que volem predir.
//amb stan pots definir i ell et prediu els missings i els que vols predir

data {
  int n;
  int n_obs;
  int n_missing;
  int<lower = 1> n_preds;
  int idx_obs[n_obs];
  int idx_missing[n_missing];
  int idx_preds[n_preds];
  vector[n_obs] obs;
  
}

parameters {
  real<lower = -1, upper = 1> a;
  real<lower = 0> sigma;
  
  vector[n_missing] imputed;
  vector[n_preds] preds;
}

transformed parameters {
  vector[n+n_preds] y;
  y[idx_obs] = obs;
  y[idx_missing] = imputed;
  y[idx_preds] = preds;
}

model {
  y[1] ~ normal(0, sigma);
  for (i in 2:(n+n_preds))
    y[i] ~ normal(a * y[i-1], sigma);
}

