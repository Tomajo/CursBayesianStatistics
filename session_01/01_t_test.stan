data {
  int n1;
  int n2;
  vector[n1] x1;
  vector[n2] x2;
}

parameters {
  real mu;
  real<lower = 0> sigma;
  real delta;
}

model {
  x1 ~ normal(mu,         sigma);
  x2 ~ normal(mu + delta, sigma);
}