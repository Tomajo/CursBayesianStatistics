data {
  int n;
  vector[n] speed;
  vector[n] dist;
}

parameters {
  real a;
  real b;
  real<lower = 0> sigma;
}

model {
  dist ~ normal(a + b * speed, sigma);
}