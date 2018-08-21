##########################################################################################
# @gilbellosta, 2018-06-22
# Hidden models
##########################################################################################
##Es un exemple tipic: tinc un cotxe amb un gps que em va donant la posició amb un error aleatori
##Com que tenim les diferents mesures podem millorar el resultat reduint els possibles salts.
##Això es fa amb un filtre de Kalman
##
setwd("~/Projectes/R/CursBayesianStatistics/session_05")
library(rstan)

#-----------------------------------------------------------------------------------------
# Kalman filtering
#-----------------------------------------------------------------------------------------

n <- 100

# real (unobserved) values
# real values follow physical laws

v.real <- rnorm(n, 1, 0.2)

x.real <- rep(0, n)
for (i in 2:length(v.real))
  x.real[i] <- x.real[i-1] + 1 * v.real[i-1]

plot(x.real, type = "l")

# observed values (with noise)

sigma.x <- 0.2
sigma.v <- 0.2
v0 <- v.real + rnorm(n, 0, sigma.v)
x0 <- x.real + rnorm(n, 0, sigma.x)

fit <- stan("05_kalman.stan", 
            data = list(N = n, x0 = x0, v0 = v0,
                        sigmax = sigma.x,
                        sigmav = sigma.v), 
            iter=12000, warmup=2000, 
            chains=4, thin=10, cores = 4)
summary(fit)
# Exercise: check whether Kalman filtering improves the precision of the observations with respect to a
#   naive model (positions are just the observed values)
    naiveSumX<-mean(sqrt(abs(x.real-x0)))
    naiveSumV<-mean(sqrt(abs(v.real-v0)))
    #Falta calcular l'error amb lo del Kalman dels prebrots.   
    res <- as.data.frame(fit)
    
    res$id <- 1:nrow(res)
    res <- melt(res, id.vars = "id")
    
    res <- res[grep("^x\\[*", res$variable),]
    res$variable <- gsub("^x\\[", "", res$variable)
    res$variable <- gsub("\\]$", "", res$variable)
    res$variable <- as.numeric(res$variable)
    
