
# Agafem el mateix exemple que ens ha posat ell i el canviem 
# Als comntaris d'abaix faig els canvis, a la part de d'alt ho deixo tot igual

############################################################################################
# @gilbellosta, 2018-05-27
# t-test and hypothesis tests
############################################################################################

library(rstan)
library(psych)
setwd("~/Projectes/R/CursBayesianStatistics/session_01")
options(mc.cores = parallel::detectCores())
#---------------------------------------------------------------------------
# t-test
#---------------------------------------------------------------------------

n <- 20

delta <- 1
mu <- 0

x1 <- rnorm(n, mu, 1)
x2 <- rnorm(n, mu + delta, 1+1)

t.test(x1, x2)

stan_data <- list(n1 = length(x1), n2 = length(x2), x1 = x1, x2 = x2)

fit <- stan("01_t_test.stan", data = stan_data, chains = 1, 
            iter = 5000, warmup = 2000, thin = 10)

res <- as.data.frame(fit)
hist(res$delta, breaks = 40)

tmp <- res[, c("mu", "sigma", "delta")]
plot(tmp)


pairs.panels(tmp, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)



# checking the convergence

traceplot(fit)

# Exercise 1: Create a stan program (and the R wrapper) to check the equality of variances 
#   of two random normal samples (regardless of the mean)
#---------------------------------------------------------------------------
# t-test
#---------------------------------------------------------------------------

n <- 20

delta <- 1
mu <- 0

x1 <- rnorm(n, mu, 1)
x2 <- rnorm(n, mu + delta, 1+1)

t.test(x1, x2)

stan_data <- list(n1 = length(x1), n2 = length(x2), x1 = x1, x2 = x2)

fit <- stan("01_t_test_exer1.stan", data = stan_data, chains = 1, 
            iter = 5000, warmup = 2000, thin = 10)

res <- as.data.frame(fit)
hist(res$delta, breaks = 40)

tmp <- res[, c("mu", "sigma1","sigma2", "delta")]
plot(tmp)


pairs.panels(tmp, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)



# checking the convergence

traceplot(fit)

# Exercise 2: Modify the t-test above to cover the case when the variables are a t distribution with
#   a given degrees of freedom; consider the possibility that variances may be different
#---------------------------------------------------------------------------
# t-test
#---------------------------------------------------------------------------

n <- 20
dof<-20
sigma1<-0.5
sigma2<-0.75
delta <- 1
mu <- 0

x1 <- rt(n,dof)*sigma1+mu
x2 <- rt(n,dof)*sigma2+mu+delta

t.test(x1, x2)

stan_data <- list(n1 = length(x1), n2 = length(x2), x1 = x1, x2 = x2)

fit <- stan("01_t_test_exer2.stan", data = stan_data, chains = 1, 
            iter = 10000, warmup = 4000, thin = 10, control = list(max_treedepth = 15,adapt_delta=0.99))

res <- as.data.frame(fit)
res<-cbind(res,res[2]-res[3])
hist(res$delta, breaks = 40)

tmp <- res[, c("mu", "sigma1","sigma2", "delta")]
plot(tmp)


pairs.panels(tmp, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

#Solucio: En el exercici he modificat la generació de les dades per generarles amb tstudent i despres
# canvio la definició del model a stan perquè modelitzi seguint una t de student.
#també faig la difernecia per veure si està aprop del 0,
diferencia<-res[,2]-res[,3]

# checking the convergence

traceplot(fit)


# Exercise 3: A web portal is considering a redesign and 3 proposals are made. The company has 10M
#   monthly visits and a conversion rate of 0.5%. The portal splits visitors into 4 different versions
#   of the portal (equally) and tries to measure increments in conversion rate. Suppose these increments
#   are of -5%, 2% and 10% for the new versions. Create random data and a Stan program to measure the
#   conversion rate differences.


#---------------------------------------------------------------------------
# Tests based on the Poisson distribution
#---------------------------------------------------------------------------

# Exercise 4: In a country, in year 1(2), 1160(1131) people die in traffic accidents. Create a Poisson model
#   to estimate the probability of an increase of the underlying death rate. Note: try to solve it by yourself,
#   but there is a complete solution at 01_poisson.R.

# Exercise 5: You are proof-reading a manuscript and find no errors in page 1, 2,..., 20. Normally, errors 
#   are distributed according to a Poisson distribution whose intensity is the average number of errors by 
#   page. The MLE estimator of the intensity in this case is 0. Get a Bayesian estimation of the intensity.

# Exercise 6: Compare the solution of the previous exercise 
#   with https://www.johndcook.com/blog/2010/03/30/statistical-rule-of-three/









