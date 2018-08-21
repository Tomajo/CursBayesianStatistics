############################################################################################
# @gilbellosta, 2018-05-27
# Posteriors "by hand" in a coin tossing problem (dirty solution)
############################################################################################

tosses <- 100
heads  <-  56

grid <- 0:1000 / 1000    # between 0 and 1

## with uniform prior

my_weights <- sapply(grid, function(p) dbinom(heads, tosses, p))
normalized_weights <- my_weights / sum(my_weights)

my_sample <- sample(grid, 1e4, replace = T, prob = normalized_weights)
hist(my_sample)

foo <- ecdf(my_sample)
summary(foo)
plot(foo)


# Exercise: repeat the previous exercise when there is a beta(3, 4) acting as a prior.



