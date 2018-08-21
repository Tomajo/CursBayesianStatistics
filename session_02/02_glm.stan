data {
  int n;
  vector[n] x;
  int y[n];
}

parameters {
  real a;
  real b;
}

model {
        y ~ bernoulli_logit(a+b*x);
    }
