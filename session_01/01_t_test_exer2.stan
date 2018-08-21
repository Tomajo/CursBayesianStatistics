data {
  int n1;
  int n2;
  vector[n1] x1;
  vector[n2] x2;
}

parameters {
  real mu;
  real<lower = 18> dof;
  real<lower = 0> sigma1;
  real<lower = 0> sigma2;
  real delta;
}

model {
  x1 ~ student_t(dof,mu,         sigma1);
  x2 ~ student_t(dof,mu + delta, sigma2);
}
