# linear regression with JAGS
library('rjags')
library('ggplot2')
library('dplyr')

data('lexdec', package = "languageR") # install languageR first if needed!
summary(lexdec)
ggplot(lexdec, aes(Frequency, RT)) +
  stat_summary(fun.dat = mean_cl_boot) +
  stat_smooth(method = lm) # builds a linear regression model

simple_model_string <-
  "model {
    beta_0 ~ dunif(-100, 100)
    beta_1 ~ dunif(-100, 100)
    sigma ~ dunif(0, 100)
    for (i in 1:length(rt)) {
      rt[i] ~ dnorm(beta_0 + beta_1 * freq[i], 1/sigma^2)
    }
  }"

ct <- function(x) {
  return(x - mean(x))
}

lexdec <- lexdec %>%
  mutate(Frequency = ct(Frequency))

samps_to_keep_simple <-samps_to_toss_simple <- 1000
simple_model <-
  jags.model(textConnection(simple_model_string),
             data = list(rt = lexdec$RT, freq = lexdec$Frequency),
             n.chains = 4)

update(simple_model, samps_to_toss_simple)
simple_mcmc_samples <-
  coda.samples(simple_model,
               variable.names=c("beta_0", "beta_1", "sigma"),
               n.iter = samps_to_keep_simple)

gelman.diag(simple_mcmc_samples)
simple_samples_dat <-
  as.data.frame(as.matrix(simple_mcmc_samples))
summary(simple_samples_dat) # ensure min and max aren't near prior boundaries
summary(simple_mcmc_samples)

## hierarchical model
lexdec <- lexdec %>%
  mutate(Subject = as.numeric(Subject))

ggplot(lexdec, aes(Frequency, RT)) +
  geom_point() + geom_smooth(method = lm) +
  facet_wrap(~ Subject)

hier_model_string <-
  "model {
    beta_0 ~ dunif(-100, 100)
    beta_1 ~ dunif(-100, 100)
    sigma_error ~ dunif(0, 100)
    sigma_beta_0 ~ dunif(0, 100)
    sigma_beta_1 ~ dunif(0, 100)
    for (i in 1:num_subj) {
      beta_0s[i] ~ dnorm(beta_0, 1 / sigma_beta_0^2)
      beta_1s[i] ~ dnorm(beta_1, 1 / sigma_beta_1^2)
    }
    for (i in 1:length(rt)) {
      rt[i] ~ dnorm(beta_0s[subject[i]] + beta_1s[subject[i]] * freq[i], 1/sigma_error^2)
    }
  }"

samps_to_keep_hier <- samps_to_toss_hier <- 1000
hier_model <-
  jags.model(textConnection(hier_model_string),
             data = list(rt = lexdec$RT,
                         freq = lexdec$Frequency,
                         subject = lexdec$Subject,
                         num_subj = max(lexdec$Subject)),
             n.chains = 4)

update(hier_model, samps_to_toss_hier)
hier_mcmc_samples <-
  coda.samples(hier_model,
               variable.names=c("beta_0", "beta_1",
                                "sigma_error", "sigma_beta_0", "sigma_beta_1",
                                "beta_0s", "beta_1s"),
               n.iter = samps_to_keep_hier)

gelman.diag(hier_mcmc_samples)
hier_samples_dat <-
  as.data.frame(as.matrix(hier_mcmc_samples))
summary(hier_samples_dat)
summary(hier_mcmc_samples)
