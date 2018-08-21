############################################################################################
# @gilbellosta, 2018-05-27
# poisson test
############################################################################################

library(rstan)

stancode <- '
  parameters{
    real<lower = 0> lambda;
    real<lower = -1, upper=1> incr;
  }
  
  model{
    1160 ~ poisson(lambda * (1 + incr));
    1131 ~ poisson(lambda);
  }
' 


fit <- stan(model_code = stancode, chains = 1, 
            iter = 12000, warmup = 2000, thin = 10) 

summary(fit)$summary[1:2, c("mean", "2.5%", "97.5%")]
