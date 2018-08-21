##########################################################################################
# @gilbellosta, 2018-06-22
# Mixtures and clustering (with and without models)
##########################################################################################
setwd("~/Projectes/R/CursBayesianStatistics/session_04")
library(rstan)
library(plyr)
library(ggplot2)
library(psych)


#-----------------------------------------------------------------------------------------
# K-means via generative models
#-----------------------------------------------------------------------------------------

# https://www.naftaliharris.com/blog/visualizing-k-means-clustering/

# generative models

n <- 2000

weights <- c(0.4, 0.2, 0.4)

means <- c(-3, 0, 2)
sds   <- c(1, 0.3, 2)

origin <- sample(1:3, n, replace = TRUE, prob = weights)

datos <- sapply(origin, function(x) rnorm(1, mean = means[x], sd = sds[x]))

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


#-----------------------------------------------------------------------------------------
# Mixtures of distributions
#-----------------------------------------------------------------------------------------

n1 <- 1000
n2 <- 1000

y0 <- abs(rnorm(n1, 0, 2))
y1 <- rgamma(n2, 10, 2)

y <- c(y0, y1)
hist(y)

# Exercise: create the stan file to fit these values and then run

fit <- stan("04_mixtures.stan", 
            data = list(N = length(y), y = y), 
            iter=10000, warmup=2000, 
            chains=1, thin=10)

# and examine the results. You can fix the mean of the normal distribution
# to 0. Note: a version of `04_mixtures.stan` is provided, but try not to 
# look at it, try your own version.
summary(fit)

#-----------------------------------------------------------------------------------------
# Clustering based on models
#-----------------------------------------------------------------------------------------

library(MASS)

plot(whiteside$Temp, whiteside$Gas, col = whiteside$Insul)

m1 <- lm(Gas ~ Temp, data = whiteside[whiteside$Insul == "Before",])
summary(m1)
m2 <- lm(Gas ~ Temp, data = whiteside[whiteside$Insul == "After",])
summary(m2)

#tenim unes dades que son de cases de UK i el consum de gas amb la temperatura. 
# Unes s'han arreglat per aillar-les i les altres no.

#El que volem es per cluster no en un centre si no per la proximitat a una linia.... 
#Hem de fer regressió i clustering a la vegada!!!1 OMG
#Es pot fer servir en lloc de LM amb GLM, o poissons o el que sigui.
fit <- stan("04_model_based_clustering.stan", 
            data = list(K = 2, N = nrow(whiteside), temp = whiteside$Temp, gas = whiteside$Gas), 
            iter=10000, warmup=2000, 
            chains=1, thin=10)

summary(fit)

res <- as.data.frame(fit)

plot(whiteside$Temp, whiteside$Gas, col = whiteside$Insul)
for (i in 1:nrow(res)) abline(res[i, "a[1]"], res[i, "b[1]"], col = rgb(0,0,0,alpha=0.01))
for (i in 1:nrow(res)) abline(res[i, "a[2]"], res[i, "b[2]"], col = rgb(1,0,0,alpha=0.01))

# Exercise: find a way to attribute dots to either model

