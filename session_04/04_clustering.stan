data {
  int<lower=1> K; // number of mixture components
  int<lower=1> N; // number of data points
  real y[N]; // observations
}

parameters {
    //simplex es una estructura de dades que te que summar 1... Com un array...
  simplex[K] theta; // mixing proportions for each point
  real mu[K];       // locations of mixture components
  real<lower=0> sigma[K]; // scales of mixture components
}

model {
  real tmp;   //temporal variable for accummulated probabilities

  sigma ~ cauchy(0,2.5);
  mu ~ normal(0,10);
//per cada observació calcula la probabilitat que pertanyi cada una de les distribucions. 
  for (n in 1:N) {
    tmp = 0;
    for (k in 1:K) {
        //+=  va acumulant
        //faig el exp del log de tot i despres vaig sumant
      tmp += exp(log(theta[k]) + normal_lpdf(y[n] | mu[k], sigma[k]));
    }
    target += log(tmp);
  }
}
