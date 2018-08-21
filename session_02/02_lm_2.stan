data {
  int n;
  vector[n] x;
  vector[n] y;
}

parameters {
  real a;
  real b;
  real sigma;
}

model {
  for (i in 1:n)
    y[i] ~ normal(a + b * x[i], sigma * x[i]);
}