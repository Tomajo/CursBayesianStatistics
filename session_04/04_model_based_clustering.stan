data {
  int<lower=1> K; // number of mixture components
  int<lower=1> N; // number of data points
  real temp[N]; // temperature
  real gas[N]; // gas consumption
}

parameters {
  simplex[K] theta; // mixing proportions for each point
  real a[K];        // model intercepts
  real<upper = 0> b[K];        // model slopes
  real<lower=0> sigma[K]; // sds of models
}

model {
  real tmp;   //temporal variable for accummulated probabilities

  sigma ~ cauchy(0,2.5);
  a ~ normal(5, 3);
  b ~ normal(0,1);

  for (n in 1:N) {
    tmp = 0;
    for (k in 1:K) {
        //Quan moc els centres ho faig respecte al model, o a quin tenim mes aprop
      tmp += exp(log(theta[k]) + normal_lpdf(gas[n] | a[k] + b[k] * temp[n], sigma[k]));
    }
    target += log(tmp);
  }
}
