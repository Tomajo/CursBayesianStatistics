##########################################################################################
# @gilbellosta, 2018-05-26
# Generative model for longitudinal data (mixed models)
##########################################################################################
setwd("~/Projectes/R/CursBayesianStatistics/session_03")
library(plyr)
library(ggplot2)
library(lme4)
library(lattice)

#-----------------------------------------------------------------------------------------
# Random intercepts, fixed slopes
#-----------------------------------------------------------------------------------------

slope <- 0.2

dose_levels <- 0:4
n_subjects <- 20

hours_without <- rnorm(n_subjects, 7, 1)

datos <- ldply(1:n_subjects, function(i) 
  data.frame(id = i, 
             dosis = dose_levels, 
             hours = hours_without[i] + slope * dose_levels + 
               rnorm(length(dose_levels), 0, 0.5)
             ))

datos$id <- factor(datos$id)

ggplot(datos, aes(x = dosis, y = hours)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  facet_wrap(~id)


## model fitting (without using mixed models yet)

plot(datos$dosis, datos$hours)
abline(lm(hours ~ dosis, data = datos), col = "red")

modelo_00 <- lm(hours ~ dosis, data = datos)
summary(modelo_00)

modelo_01 <- lm(hours ~ id + dosis, data = datos)
modelo_01 <- lm(hours ~ -1 + id + dosis, data = datos)
summary(modelo_01)

modelo_02 <- lmer(hours ~ dosis + (1 | id), data = datos)
summary(modelo_02)

dotplot(ranef(modelo_02, condVar = TRUE))

#-----------------------------------------------------------------------------------------
# Random intercepts, random slopes
#-----------------------------------------------------------------------------------------

slope <- 0.2

dose_levels <- 0:4
n_subjects <- 20

hours_without <- rnorm(n_subjects, 7, 1)
slopes_sj <- rnorm(n_subjects, slope, 0.1)

datos <- ldply(1:n_subjects, function(i) data.frame(id = i, 
                                                   dosis = dose_levels, 
                                                   hours = hours_without[i] + slopes_sj[i] * dose_levels + 
                                                     rnorm(length(dose_levels), 0, 0.5)))

datos$id <- factor(datos$id)

ggplot(datos, aes(x = dosis, y = hours)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  facet_wrap(~id)


## model fitting, non-mixed modelling first

modelo_00 <- lm(hours ~ id + dosis * id, data = datos)
summary(modelo_00)

#diem que es dependent la dosis i el id
modelo_01 <- lmer(hours ~ 1 + dosis + (1 + dosis| id), data = datos)
#diem que no hi ha dependencia entre la dosis i el id...  o al reves. S'ha de revisar.
#modelo_01 <- lmer(hours ~ 1 + dosis + (1 | id) + (dosis| id), data = datos)
summary(modelo_01)

datos_alt <- datos
datos_alt$dosis <- datos_alt$dosis - 2
modelo_01 <- lmer(hours ~ 1 + dosis + (1 + dosis| id), data = datos_alt)
summary(modelo_01)

dotplot(ranef(modelo_01, condVar = TRUE))

#-----------------------------------------------------------------------------------------
# Random intercepts, random slopes, more effects
#-----------------------------------------------------------------------------------------

slope <- 0.2

dose_levels <- 0:4
n_subjects <- 20

sexo <- rep(0:1, each = n_subjects / 2)


hours_without <- rnorm(n_subjects, 7, 1)
slopes_sj <- rnorm(n_subjects, slope, 0.1)

datos <- ldply(1:n_subjects, function(i) data.frame(id = i, 
                                                   dosis = dose_levels, 
                                                   sexo  = sexo[i],
                                                   hours = hours_without[i]  - 0.5 * sexo[i] + slopes_sj[i] * dose_levels + rnorm(length(dose_levels), 0, 0.5)))

datos$id <- factor(datos$id)
datos$sexo <- factor(datos$sexo)

ggplot(datos, aes(x = dosis, y = hours)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  facet_wrap(~id)


## model fitting 

modelo_00 <- lm(hours ~ id + sexo + dosis * id, data = datos)
summary(modelo_00)

modelo_01 <- lmer(hours ~ 1 + sexo + dosis + (1 + dosis| id), data = datos)
summary(modelo_01)
