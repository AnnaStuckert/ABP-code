##########Bayes testing - not usedRStudio.Version() 
pacman::p_load(
  rethinking,
  brms,
  tidyverse,
  bayesplot,
  viridis,
  dplyr, 
  ggplot2,
  tibble, 
  boot,
  cowplot
)

### Investigating only threshold

#scale variables
data$threshold_scaled <- scale(data$Threshold)
data$perc_risk_NoAbg_scaled <- scale(data$perc_risk_NoAbg)

# Define the formula - designing the model. We have a between-study design. 
threshold_f0 <- bf(perc_risk_NoAbg_scaled ~ 1 + threshold_scaled)

# data_bays <- select(data_all, threshold_scaled, SIAS_scaled)
# write.csv(data_bays, "Peter-data.csv")

#if you scale y and x see riccardos video for assignment 3. If not, intercept should be the avarage of y and I don't know about the betas
# Design the priors
prior_f0 <- get_prior(threshold_f0, family = gaussian, data) # The family = Guassian, because the outcome is a continuous variable
view(prior_f0)
# We see that we need a beta prior and a sigma prior
summary(data$threshold_scaled) 
priorScales <- c(
  prior(student_t(3, 0, 2.5), class = Intercept), # the intercept prior needs to reflect the average y-value (where SIAS is 0, threshold is supposed to be average)
  prior(normal(0, 4), class = b), # beta is the expectation of the difference between schizophrenia and controls. We say that the beta is normally distributed, and that the mean is 0 and the standard deviation is 1 (not sure this is correct).
  prior(student_t(3, 0, 2.5), class = sigma) # sigma is the average error that we expect. 
) 

# Test the priors. We want to check whether the priors make any sense.
threshold_PriorCheck_m <- brm( # m stands for model
  formula = threshold_f0,
  data = data,
  family = gaussian, # Gaussian because the outcome is continuous
  prior = priorScales,
  sample_prior = "only",# we sample the prior in order to test the prior
  control = list(adapt_delta = 0.9)
)

pairs(threshold_PriorCheck_m)
# We check the what the predictions look like given the prior and not the data. We set the number of simulations to 100.
pp_check(threshold_PriorCheck_m, nsamples = 100)

# What we see is that the prior has a very long tail. In order to fix this we make a prior for the sigma - in order to expect an error (we edited the first one)

## Fitting the model
threshold_m <- brm(
  formula = threshold_f0,
  data = data,
  family = gaussian,
  prior = priorScales,
  sample_prior = T,
  control = list(adapt_delta = 0.9)
)

# Posterior predictive check. # We want to look at whether the posterior has learned from the prior, which is what we expect when we look at the posterior predictive check
pp_check(threshold_m, nsamples = 100)
# The data looks good!
# The light blue is the prior for the difference between schizophrenia and control (it is very spread, which means that it is very uncertain). 
# The dark blue is the posterior (this is much more certain - must less variance), which tells us that it has actually learned from the data, and makes more confident predictions. 

# Conclusion: the prior is not off, and the posterior has learned from the data. This is good!

summary(threshold_m)

hypothesis(threshold_m, "threshold_scaled > 0")


### INVESTIGATING EFFECT OF SLOPE

data <- int_shortened
#scale variables
data$slope_scaled <- scale(data$Slope)
data$perc_risk_NoAbg_scaled <- scale(data$perc_risk_NoAbg)

# Define the formula - designing the model. We have a between-study design. 
slope_f0 <- bf(perc_risk_NoAbg_scaled ~ 1 + slope_scaled)

# data_bays <- select(data_all, threshold_scaled, SIAS_scaled)
# write.csv(data_bays, "Peter-data.csv")

#if you scale y and x see riccardos video for assignment 3. If not, intercept should be the avarage of y and I don't know about the betas
# Design the priors
prior_f0 <- get_prior(slope_f0, family = gaussian, data) # The family = Guassian, because the outcome is a continuous variable
view(prior_f0)
# We see that we need a beta prior and a sigma prior
summary(data$threshold_scaled) 
priorScales <- c(
  prior(student_t(3, 0, 2.5), class = Intercept), # the intercept prior needs to reflect the average y-value (where SIAS is 0, threshold is supposed to be average)
  prior(normal(0, 4), class = b), # beta is the expectation of the difference between schizophrenia and controls. We say that the beta is normally distributed, and that the mean is 0 and the standard deviation is 1 (not sure this is correct).
  prior(student_t(3, 0, 2.5), class = sigma) # sigma is the average error that we expect. 
) 

# Test the priors. We want to check whether the priors make any sense.
slope_PriorCheck_m <- brm( # m stands for model
  formula = slope_f0,
  data = data,
  family = gaussian, # Gaussian because the outcome is continuous
  prior = priorScales,
  sample_prior = "only",# we sample the prior in order to test the prior
  control = list(adapt_delta = 0.9)
)

pairs(slope_PriorCheck_m)
# We check the what the predictions look like given the prior and not the data. We set the number of simulations to 100.
pp_check(slope_PriorCheck_m, nsamples = 100)

# What we see is that the prior has a very long tail. In order to fix this we make a prior for the sigma - in order to expect an error (we edited the first one)

## Fitting the model
slope_m <- brm(
  formula = slope_f0,
  data = data,
  family = gaussian,
  prior = priorScales,
  sample_prior = T,
  control = list(adapt_delta = 0.9)
)

# Posterior predictive check. # We want to look at whether the posterior has learned from the prior, which is what we expect when we look at the posterior predictive check
pp_check(slope_m, nsamples = 100)
# The data looks good!
# The light blue is the prior for the difference between schizophrenia and control (it is very spread, which means that it is very uncertain). 
# The dark blue is the posterior (this is much more certain - must less variance), which tells us that it has actually learned from the data, and makes more confident predictions. 

# Conclusion: the prior is not off, and the posterior has learned from the data. This is good!

summary(slope_m)

hypothesis(slope_m, "slope_scaled > 0")


#COMBINED MODEL

### INVESTIGATING EFFECT OF SLOPE + dPrime

data <- int_shortened
#scale variables
data$slope_scaled <- scale(data$Slope)
data$dprime_scaled <- scale(data$dPrime)
data$perc_risk_NoAbg_scaled <- scale(data$perc_risk_NoAbg)

# Define the formula - designing the model. We have a between-study design. 
combined_f0 <- bf(perc_risk_NoAbg_scaled ~ 1 + slope_scaled:dprime_scaled)

# data_bays <- select(data_all, threshold_scaled, SIAS_scaled)
# write.csv(data_bays, "Peter-data.csv")

#if you scale y and x see riccardos video for assignment 3. If not, intercept should be the avarage of y and I don't know about the betas
# Design the priors
combined_f0 <- get_prior(combined_f0, family = gaussian, data) # The family = Guassian, because the outcome is a continuous variable
view(prior_f0)
# We see that we need a beta prior and a sigma prior
summary(data$dprime_scaled)
summary(data$slope_scaled) 

priorScales <- c(
  prior(student_t(3, 0, 2.5), class = Intercept), # the intercept prior needs to reflect the average y-value (where SIAS is 0, threshold is supposed to be average)
  prior(normal(0, 4), class = b), # beta is the expectation of the difference between schizophrenia and controls. We say that the beta is normally distributed, and that the mean is 0 and the standard deviation is 1 (not sure this is correct).
  prior(student_t(3, 0, 2.5), class = sigma) # sigma is the average error that we expect. 
) 

# Test the priors. We want to check whether the priors make any sense.
combined_PriorCheck_m <- brm( # m stands for model
  formula = combined_f0,
  data = data,
  family = gaussian, # Gaussian because the outcome is continuous
  prior = priorScales,
  sample_prior = "only",# we sample the prior in order to test the prior
  control = list(adapt_delta = 0.9)
)

pairs(combined_PriorCheck_m)
# We check the what the predictions look like given the prior and not the data. We set the number of simulations to 100.
pp_check(combined_PriorCheck_m, nsamples = 100)

# What we see is that the prior has a very long tail. In order to fix this we make a prior for the sigma - in order to expect an error (we edited the first one)

## Fitting the model
combined_m <- brm(
  formula = combined_f0,
  data = data,
  family = gaussian,
  prior = priorScales,
  sample_prior = T,
  control = list(adapt_delta = 0.9)
)

# Posterior predictive check. # We want to look at whether the posterior has learned from the prior, which is what we expect when we look at the posterior predictive check
pp_check(combined_m, nsamples = 100)
# The data looks good!
# The light blue is the prior for the difference between schizophrenia and control (it is very spread, which means that it is very uncertain). 
# The dark blue is the posterior (this is much more certain - must less variance), which tells us that it has actually learned from the data, and makes more confident predictions. 

# Conclusion: the prior is not off, and the posterior has learned from the data. This is good!

summary(combined_m)

hypothesis(combined_m, "slope_scaled > 0")

