##########################################################################################
# @gilbellosta, 2018-06-22
# Krigging
##########################################################################################
#El que es proposa es fer models sobre mapes on cada punt mesurat suposem que està correlacionat 
# amb els altres i que la correlació decreix amb la distancia. 
#aquest exemple es sobre una dimensió
#tractem els missings com a valors a predir.
#una de les sortides del procés es com decau la correlació amb la distancia.
setwd("~/Projectes/R/CursBayesianStatistics/session_05")
library(rstan)
library(reshape2)
library(ggplot2)
library(plyr)

n_sample <- 30
n_grid   <- 100
sd_error <- 0.1

x <- sort(runif(n_sample) * 2 * pi)
y <- sin(x) + rnorm(n_sample, 0, sd_error)

rejilla <- seq(0, 2 * pi, length.out = n_grid)

all_x <- c(x, rejilla)

distances <- as.matrix(dist(as.matrix(all_x), method = "manhattan"))
distances <- distances^1.5

stan_data <- list(
  N1 = n_sample,
  N2 = n_grid,
  N  = n_sample + n_grid,
  x  = all_x,
  y0 = y,
  dists = distances
)

fit_alt <- stan(file = "05_kriging.stan",
                data = stan_data,
                iter = 10000, warmup = 5000, 
                chains = 1, thin = 10)

res <- as.data.frame(fit_alt)

preds <- res[, grep("^y2", colnames(res))]
preds$id <- 1:nrow(preds)
preds <- melt(preds, id.vars = "id")
preds$x <- gsub("y2\\[(.*)\\]", "\\1", preds$variable)
preds$x <- as.numeric(preds$x)

preds$x <- rejilla[preds$x]

ggplot(preds, aes(x = x, y = value)) + geom_point(alpha = 0.1)

tmp <- ddply(preds, .(x), summarize, y = median(value))

plot(x, y, main = "Kriging fit to noise curve observations",
     xlab = "", ylab = "")
lines(tmp$x, tmp$y, type = "l", col = "red")