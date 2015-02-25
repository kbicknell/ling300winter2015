# playing with mixture of gaussians and vowels in JAGS
library('ggplot2')
library('Hmisc')
library('dplyr')
library('rjags')

## generate simulated vowels
set.seed(13)
vowels_per_cat <- 100
x_i <- rnorm(vowels_per_cat, 342, sd = 50)
x_e <- rnorm(vowels_per_cat, 580, sd = 50)
x <- c(x_i, x_e)
dat_vowels <- data_frame(x)
ggplot(dat_vowels, aes(x)) + geom_histogram()

## MoG modeling: uniform priors (simple but slow)
model_string_unif <-
  "model {
    pi ~ dbeta(1, 1)
    for (i in 1:2) {
      tau[i] ~ dunif(0, 10)
      mu[i] ~ dunif(0, 2000)
    }
    for (i in 1:length(x)) {
      z[i] ~ dbern(pi)
      x[i] ~ dnorm(mu[z[i]+1], tau[z[i]+1])
    }
  }"

num_samples_to_keep_unif <- 500
num_samples_to_toss_unif <- num_samples_to_keep_unif

model_unif <-
  jags.model(textConnection(model_string_unif),
             data = list(x = dat_vowels$x), n.chains = 2)
update(model_unif, num_samples_to_toss_unif)
mcmc_samples_unif <-
  coda.samples(model_unif,
               variable.names = c("pi", "mu", "tau"),
               n.iter = num_samples_to_keep_unif)

gelman.diag(mcmc_samples_unif) # on some runs, will be nowhere near convergence because the clusters will be reversed
summary(mcmc_samples_unif)
plot(mcmc_samples_unif, ask = TRUE)

## MoG modeling: conditionally conjugate priors (faster)
model_string_conj <-
  "model {
    pi ~ dbeta(1, 1)
    for (i in 1:2) {
      tau[i] ~ dgamma(0.001, 2.5)
      mu[i] ~ dnorm(450, tau[i] * .0001)
    }
    for (i in 1:length(x)) {
      z[i] ~ dbern(pi)
      x[i] ~ dnorm(mu[z[i]+1], tau[z[i]+1])
    }
  }"

num_samples_to_keep_conj <- 500
num_samples_to_toss_conj <- num_samples_to_keep_conj

model_conj <- jags.model(textConnection(model_string_conj),
                         data = list(x = dat_vowels$x), n.chains = 2)
update(model_conj, num_samples_to_toss_conj)
mcmc_samples_conj <- coda.samples(model_conj,
                                   variable.names = c("pi", "mu", "tau"),
                                   n.iter = num_samples_to_keep_conj)

gelman.diag(mcmc_samples_conj) # on some runs, will be nowhere near convergence because the clusters will be reversed
summary(mcmc_samples_conj)
plot(mcmc_samples_conj, ask = TRUE)
