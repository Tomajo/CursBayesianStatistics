##########################################################################################
# He d'adaptar el exemple a 2 dimensions... Chungo segur... perque no se com modelar la gausiana en dos dimensions
# 
##########################################################################################
setwd("~/Projectes/R/CursBayesianStatistics/session_04")
library(rstan)
library(plyr)
library(ggplot2)
library(psych)
library(MASS)


#-----------------------------------------------------------------------------------------
# K-means via generative models
#-----------------------------------------------------------------------------------------

# https://www.naftaliharris.com/blog/visualizing-k-means-clustering/

# generative models

n <- 2000

weights <- c(0.4, 0.2, 0.4)

means <- matrix(c(-3,0,2,-3,0,2),nrow = 3,ncol = 2)
sds   <- matrix(c(1, 0.3, 2,1, 0.3, 2),nrow = 3,ncol = 2)

origin <- sample(1:3, n, replace = TRUE, prob = weights)

datos <- sapply(origin, function(x) mvrnorm(1, mean = means[x], sd = sds[x]))

hist(datos, breaks = 50)

fit <- stan("04_clustering.stan", 
            data = list(K = 3, N = length(datos), y = datos), 
            iter=10000, warmup=2000, 
            chains=1, thin=10)

# Exercise: analize the results
summary(fit)
# Exercise: try with two or more chains... Do you notice any issues?
fit <- stan("04_clustering.stan", 
            data = list(K = 3, N = length(datos), y = datos), 
            iter=10000, warmup=2000, 
            chains=3, thin=10)
traceplot(fit)
# Exercise: try with fewer points up to the point where setting more informative
#   makes sense.

