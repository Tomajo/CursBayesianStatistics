##########################################################################################
# @gilbellosta, 2018-05-26
# Longitudinal model using stan
##########################################################################################

library(plyr)
library(ggplot2)
library(lme4)
library(rstan)

#-----------------------------------------------------------------------------------------
# Random intercepts, fixed slopes
#-----------------------------------------------------------------------------------------

slope <- 0.2

dose_levels <- 0:4
n_subjects <- 20

hours_base <- rnorm(n_subjects, 7, 1)

datos <- ldply(1:n_subjects, function(i) data.frame(id = i, 
                                                   dose = dose_levels, 
                                                   hours = hours_base[i] + slope * dose_levels + rnorm(length(dose_levels), 0, 0.5)))

datos$id <- factor(datos$id)

ggplot(datos, aes(x = dose, y = hours)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  facet_wrap(~id)


## ajuste del modelo (primero, modelo lineal (sin efectos aleatorios))

modelo_00 <- lm(hours ~ id + dose, data = datos)
summary(modelo_00)

modelo_01 <- lmer(hours ~ dose + (1 | id), data = datos)
summary(modelo_01)

## Stan fit

stan_data <- list(
  n_subjects = n_subjects,
  n_obs     = nrow(datos),
  id    = as.numeric(as.character(datos$id)),
  dose = datos$dose,
  hours = datos$hours
)

fit_alt <- stan(file = "03_mixed_models_stan.stan",
                data = stan_data,
                iter = 10000, warmup = 5000, 
                chains = 1, thin = 10)

res <- as.data.frame(fit_alt)

hist(res$coef)

tmp <- res[, c("coef", "sigma", "mu0", "sigma0")]
plot(tmp)

library(psych)
pairs.panels(tmp, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

# Exercise: modify the previous exercise to fit the model with fixed and random factors with Stan.




