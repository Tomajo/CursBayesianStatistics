##########################################################################################
# @gilbellosta, 2018-06-22
# Missing data & predictions
##########################################################################################
library(reshape2)
library(rstan)

n <- 100
a <- -0.9

sigma <- 0.3

y <- rep(0, n)
y[1] <- rnorm(1, 0, 1)
for (i in 2:n)
  y[i] <- rnorm(1, a * y[i-1], sigma)

stan_data <- list(n = n, y = y)

fit <- stan("05_missing_data_00.stan", data = stan_data, chains = 1, 
            iter = 5000, warmup = 2000, thin = 10)

summary(fit)

#-----------------------------------------------------------------------------------------
# Now, with predictions
# Note: predictions are "missing data" imputed according to the model
#-----------------------------------------------------------------------------------------

stan_data <- list(n = n, obs = y)
stan_data$idx_obs <- 1:n
stan_data$n_preds <- 5
stan_data$idx_preds <- length(y) + 1:stan_data$n_preds

fit <- stan("05_missing_data_01.stan", data = stan_data, chains = 1, 
            iter = 5000, warmup = 2000, thin = 10)

summary(fit)

# Exercise: create a plot and add prediction bands (for given confidence intervals)

#-----------------------------------------------------------------------------------------
# Now, with missing values
#-----------------------------------------------------------------------------------------

n_preds <- 5
n_missing <- 5
missing <- sort(sample(1:length(y), n_missing))

idx_obs <- (1:length(y))[-missing]
obs <- y[idx_obs]

# keep the missing values, to check later
my_missing <- y[missing]

missing_mask <- rep(0, length(y))
missing_mask[missing] <- 1

stan_data <- list(
  n = length(y),
  n_obs = length(obs),
  n_missing = n_missing,
  n_preds = n_preds,
  idx_obs     = idx_obs,
  idx_missing = missing,
  idx_preds   = 1:n_preds + max(idx_obs),
  obs = obs
)

fit <- stan("05_missing_data_02.stan", data = stan_data, chains = 1, 
            iter = 5000, warmup = 2000, thin = 10)
summary(fit)

# Exercise: create a plot and add prediction bands (for given confidence intervals) both for
#   predictions and imputed values
res <- as.data.frame(fit)

res$id <- 1:nrow(res)
res <- melt(res, id.vars = "id")

res <- res[grep("^y\\[*", res$variable),]
res$variable <- gsub("^y\\[", "", res$variable)
res$variable <- gsub("\\]$", "", res$variable)
res$variable <- as.numeric(res$variable)

ggplot(res, aes(x = variable, y = value, group = id)) +
    geom_line(alpha = 0.1)

tmp <- res[res$variable > 180,]
ggplot(tmp, aes(x = variable, y = value, group = id)) +
    geom_line(alpha = 0.1)
#-----------------------------------------------------------------------------------------
# Now, with missing values
#-----------------------------------------------------------------------------------------

n <- 200
a <- 0.3
b <- 1

sigma <- 0.3

# Unobserved sequence:

y <- rep(0, n)
y[1] <- rnorm(1, 0, 1)
for (i in 2:n)
  y[i] <- rnorm(1, a * y[i-1], sigma)

# observed sequence:

obs <- sapply(y, function(x) rpois(1, exp(b * y)))

# Exercise: estimate a, b and sigma given obs (but not y)













