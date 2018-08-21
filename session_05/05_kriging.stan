data {
  int<lower=1> N1; // number of data points
  int<lower=1> N2; // number of grid points
  int<lower=1> N;  // N1 + N2
  real x[N];       // number of outcomes
  real y0[N1];     // observed values
  matrix[N, N] dists;    // distances between points
}

parameters {
  vector[N2] y2;   // predictions for missing values (grid)
  real <lower = 0> sigma;
  real <lower = 0> phi;
  real <lower = 0> sd_noise;
  real mu;
}

model {
  vector[N] y;
  vector[N] means;
  matrix[N,N] Sigma;
  matrix[N,N] L;

  // y values
  for(i in 1:N1) y[i]    = y0[i];
  for(i in 1:N2) y[N1+i] = y2[i];

  // means: equal to mu
  for(i in 1:N) means[i] = mu;

  // priors (non informative)
  sd_noise ~ normal(0, 1);
  sigma ~ normal(0, 1);
  phi   ~ normal(0, 3);
  mu    ~ normal(0, 1);
  y2    ~ normal(0, 1);

  // covariance matrix
  Sigma = sigma^2 * exp(-phi * dists);
  for(i in 1:N) Sigma[i,i] = Sigma[i,i] + sd_noise;

  // model
  L = cholesky_decompose(Sigma);
  y ~ multi_normal_cholesky(means, L);
}
