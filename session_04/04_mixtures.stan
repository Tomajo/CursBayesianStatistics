data {
  int<lower=1> N; // number of data points
  real y[N]; // observations
}

parameters {
  simplex[2] theta; // mixing proportions for each point
  //real<lower=0>mu;     // locations of normal component
  real<lower=0> sigma; // scales of normal components
  real<lower=0> alpha; // gamma distribution alpha parameter
  real<lower=0> beta;  // gamma distribution beta parameter
}

model {
  real tmp;   //temporal variable for accummulated probabilities

  sigma ~ cauchy(0,2.5);
  //mu    ~ normal(0,10);
  alpha ~ normal(0,10);
  beta  ~ normal(0,10);
  

  for (n in 1:N) {
    tmp  = theta[1] * 2 * exp(normal_lpdf(y[n] | 0, sigma));    //truncated normal (explains the x2 factor)
    tmp += exp(log(theta[2]) + gamma_lpdf(y[n] | alpha, beta));
    target += log(tmp);
  }
}
