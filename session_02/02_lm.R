############################################################################################
# @gilbellosta, 2018-05-27
# Linear model with Stan
############################################################################################
setwd("~/Projectes/R/CursBayesianStatistics/session_02")
library(rstan)
library(psych)

# the data:
plot(cars$speed, cars$dist)
abline(lm(dist ~ speed, data = cars), col = "red")

lm_model <- lm(dist ~ speed, data = cars)
summary(lm_model)


stan_data <- list(
  n = nrow(cars),
  speed = cars$speed,
  dist  = cars$dist
)

fit <- stan("02_lm.stan", data = stan_data, chains = 1, 
            iter = 5000, warmup = 2000, thin = 10)

summary(fit)

# Exercise: compare the output of this model with that of lm


res <- as.data.frame(fit)
hist(res$b, breaks = 40)

tmp <- res[, c("a", "b", "sigma")]
#ens apareix una correlació espurea entre la a i la b. Hauríem de centrar les velocitats... 
# restant la velocitat mitja a totes les velocitats. Ho faig a l exercici següent
plot(tmp)


pairs.panels(tmp, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

# Exercise: plot possible, likely curves (one for each sample)



# Exercise: introduce a quadratic term in the model equation; study the distribution of the coefficient
setwd("~/Projectes/R/CursBayesianStatistics/session_02")
library(rstan)
library(psych)

# the data:
plot(cars$speed, cars$dist)
abline(lm(dist ~ speed, data = cars), col = "red")
#resto velocitat  mitja per treure correlació espurea. 
cars$speed<-cars$speed-mean(cars$speed)
cars$speed2<-cars$speed^2
lm_model <- lm(dist ~ speed, data = cars)
summary(lm_model)


stan_data <- list(
    n = nrow(cars),
    speed = cars$speed,
    speed2 = cars$speed2,
    dist  = cars$dist
)

fit <- stan("02_lm_ex2.stan", data = stan_data, chains = 1, 
            iter = 5000, warmup = 2000, thin = 10)

summary(fit)

# Exercise: compare the output of this model with that of lm


res <- as.data.frame(fit)
hist(res$b, breaks = 40)

tmp <- res[, c("a", "b", "c" , "sigma")]
plot(tmp)


pairs.panels(tmp, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)


#------------------------------------------------------------------------------------
# Unequal (growing) variances
#quan les dades fan una forma de trompeta
#------------------------------------------------------------------------------------
setwd("~/Projectes/R/CursBayesianStatistics/session_02")
library(rstan)
library(psych)
n <- 100
x <- runif(n)

sigma_coef <- 0.5

a <- -0.5
b <- 1

y <- sapply(1:n, function(i) rnorm(1, a + b * x[i], sigma_coef * x[i]))

plot(x, y)
summary(lm(y ~ x))


stan_data <- list(
  n = length(x),
  x = x,
  y = y
)



fit <- stan("02_lm_2.stan", data = stan_data, chains = 1, 
            iter = 5000, warmup = 2000, thin = 10)

summary(fit)
res <- as.data.frame(fit)
hist(res$b, breaks = 40)

tmp <- res[, c("a", "b" , "sigma")]
plot(tmp)


pairs.panels(tmp, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)
# Exercise: explore the output, the convergence, etc.

# Exercise: test whether in the cars example, the variance grows (p.e., linearly) with the speed
    setwd("~/Projectes/R/CursBayesianStatistics/session_02")
    library(rstan)
    library(psych)
    
    # the data:
    plot(cars$speed, cars$dist)
   
    abline(lm(dist ~ speed, data = cars), col = "red")
    
    lm_model <- lm(dist ~ speed, data = cars)
    summary(lm_model)
    
    
    stan_data <- list(
        n = nrow(cars),
        speed = cars$speed,
        dist  = cars$dist
    )
    
    fit <- stan("02_lm_2_2.stan", data = stan_data, chains = 1, 
                iter = 5000, warmup = 2000, thin = 10)
    
    summary(fit)
    
    # Exercise: compare the output of this model with that of lm
    
    
    res <- as.data.frame(fit)
    hist(res$b, breaks = 40)
    
    tmp <- res[, c("a", "b", "sigma")]
    #ens apareix una correlació espurea entre la a i la b. Hauríem de centrar les velocitats... 
    # restant la velocitat mitja a totes les velocitats. Ho faig a l exercici següent
    plot(tmp)
    
    
    pairs.panels(tmp, 
                 method = "pearson", # correlation method
                 hist.col = "#00AFBB",
                 density = TRUE,  # show density plots
                 ellipses = TRUE # show correlation ellipses
    )


#------------------------------------------------------------------------------------
# GLMs
#------------------------------------------------------------------------------------

## logistic

n <- 100

a <- -0.5
b <- 0.8

x <- runif(n, -2, 2)
probs <- exp(a + b * x)
probs <- probs / (1 + probs)

y <- sapply(probs, function(p) rbinom(1, 1, p))

res <- glm(y ~ x, family = binomial)
summary(res)

# Exercise: fit the model using Stan; you'll need to specify y ~ bernouilli(...) at some point
stan_data <- list(
    n = length(x),
    x = x,
    y = y
)

fit <- stan("02_glm.stan", data = stan_data, chains = 1, 
            iter = 5000, warmup = 2000, thin = 10)

summary(fit)
## Poisson

# Exercise: create data for a Poisson regression and fit it using Stan




