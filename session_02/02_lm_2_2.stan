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
  for (i in 1:n)
    dist[i] ~ normal(a + b * speed[i], sigma * speed[i]);
}