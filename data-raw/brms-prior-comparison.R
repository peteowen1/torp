# Define the model formula
formula <- bf(score_diff ~ 0 + ., center = FALSE)

# Define candidate priors
candidate_priors <- list(
  priors_1 = c(prior(normal(0, 3), class = "b"), prior(cauchy(0, 1.5), class = "sigma")),
  priors_2 = c(prior(normal(0, 4), class = "b"), prior(cauchy(0, 2), class = "sigma")),
  priors_3 = c(prior(normal(0, 5), class = "b"), prior(cauchy(0, 2.5), class = "sigma"))
)

# Prior predictive checks
for (i in seq_along(candidate_priors)) {
  fit_prior <- brm(formula, data = df_brms, prior = candidate_priors[[i]], family = gaussian(), chains = 2, iter = 1000, cores = available_cores, sample_prior = "only")
  print(paste("Prior Predictive Check for Priors Set", i))
  pp_check(fit_prior)
}

# Fit models with candidate priors
fits <- lapply(candidate_priors, function(priors) {
  brm(formula, data = df_brms, prior = priors, family = gaussian(), chains = 2, iter = 1000, cores = available_cores)
})

# Compare models using LOO cross-validation
loos <- lapply(fits, loo)
loo_compare(loos[[1]], loos[[2]], loos[[3]])
