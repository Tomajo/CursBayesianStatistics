data {
  int n;
  vector[n] speed;
  vector[n] speed2;
  vector[n] dist;
}

parameters {
  real a;
  real b;
  real c;
  real<lower = 0> sigma;
}

model {
  dist ~ normal(a + b*speed+ c*speed2, sigma);
}