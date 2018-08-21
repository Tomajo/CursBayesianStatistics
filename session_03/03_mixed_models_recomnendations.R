##########################################################################################
# @gilbellosta, 2018-05-26
# Mixed models for recommendation systems
# Based on: http://slatestarcodex.com/2016/03/01/2016-nootropics-survey-results/
##########################################################################################

library(readxl)
library(reshape2)
library(lme4)
library(plyr)
library(lattice)

download.file("http://www.datanalytics.com/wp-uploads/2016/03/recomendador_drogas.xlsx",
              destfile = "recomendador_drogas.xlsx")
raw <- read_excel("recomendador_drogas.xlsx", 1)

# interesting columns
dat <- raw[, c(8:40, 42, 48, 53)]

# cleanup
colnames(dat)[grep("Semax", colnames(dat))] <- "Semax"
colnames(dat)[grep("Selank", colnames(dat))] <- "Selank"
colnames(dat)[grep("Alpha.Brain", colnames(dat))] <- "Alpha.Brain"
colnames(dat)[grep("Epicor", colnames(dat))] <- "Epicor"
colnames(dat)[grep("LSD.Microdosing", colnames(dat))] <- "LSD.Microdosing"
colnames(dat)[grep("Adderall", colnames(dat))] <- "Adderall"
colnames(dat)[grep("Phenibut", colnames(dat))] <- "Phenibut"

# data preparation
dat$id <- 1:nrow(dat)
dat <- melt(dat, id.vars = "id")
colnames(dat) <- c("usuario", "droga", "nota")
dat <- na.omit(dat)
dat$usuario <- as.character(dat$usuario)

# simple table
ranking <- ddply(dat, .(droga), summarize, nota = mean(nota))
ranking <- ranking[order(-ranking$nota),]

# mixed model
modelo <- lmer(nota ~ 1 + (1|usuario) + (1|droga), data = dat)

dotplot(ranef(modelo, condVar = TRUE))

## Exercise: reimplement the previous exercise going full Bayesian (with Stan)



