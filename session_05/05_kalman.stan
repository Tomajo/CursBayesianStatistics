data {
  int<lower=1> N;   // number of observations
  vector[N] x0;     // observed positions (noisy)
  vector[N] v0;     // observed speeds (noisy)
  real sigmax;      // known sigma for positions
  real sigmav;      // known sigma for speeds
}

parameters {
  vector[N] x;      // unobserved positions
  vector[N] v;      // unobserved speeds
}

model {
  v0[1] ~ normal(v[1], sigmav);
  x0[1] ~ normal(x[1], sigmax);

  for (n in 2:N){
    x[n]  ~ normal(x[n-1] + 1 * v[n-1], 0.1);    // physics for unobserved phenomena (as a prior)
    x0[n] ~ normal(x[n], sigmax);   // observation process
    v0[n] ~ normal(v[n], sigmav);   // observation process
  }
}
