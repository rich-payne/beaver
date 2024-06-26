\donttest{
# The {beaver} package, by definition, performs MCMC for multiple models.
# Even with a small number of chains/burn-ins/samples, a minimally illustrative
# example requires >5s to run.

library(dplyr)

# No covariates----

set.seed(100)

df <- data_negbin_emax(
  n_per_arm = 10,
  doses = 0:3,
  b1 = 0,
  b2 = 2.5,
  b3 = 0.5,
  ps = 0.75
)

df %>%
  group_by(dose) %>%
  summarize(
    mean = mean(response),
    se = sd(response) / sqrt(n()),
    .groups = "drop"
  )

mcmc <- beaver_mcmc(
  emax = model_negbin_emax(
    mu_b1 = 0,
    sigma_b1 = 10,
    mu_b2 = 0,
    sigma_b2 = 10,
    mu_b3 = 1.5,
    sigma_b3 = 3,
    w_prior = 1 / 4
  ),
  linear = model_negbin_linear(
    mu_b1 = 0,
    sigma_b1 = 10,
    mu_b2 = 0,
    sigma_b2 = 10,
    w_prior = 1 / 4
  ),
  quad = model_negbin_quad(
    mu_b1 = 0,
    sigma_b1 = 10,
    mu_b2 = 0,
    sigma_b2 = 10,
    mu_b3 = 1.5,
    sigma_b3 = 3,
    w_prior = 1 / 4
  ),
  exp = model_negbin_exp(
    mu_b1 = 0,
    sigma_b1 = 10,
    mu_b2 = 0,
    sigma_b2 = 10,
    mu_b3 = 0,
    sigma_b3 = 3,
    w_prior = 1 / 4
  ),
  formula = ~ 1,
  data = df,
  n_iter = 1e2,
  n_chains = 1,
  quiet = TRUE
)

mcmc$w_post

draws <- try(draws(mcmc)) #draws() is intended for single model fits only
draws_emax <- draws(mcmc$models$emax$mcmc)
draws_linear <- draws(mcmc$models$linear$mcmc)
draws_quad <- draws(mcmc$models$quad$mcmc)
draws_exp <- draws(mcmc$models$exp$mcmc)

post <- posterior(
  mcmc,
  contrast = matrix(1, 1, 1),
  doses = 0:3,
  reference_dose = 0,
  reference_type = "difference"
)

pr_eoi(
  mcmc,
  eoi = c(5, 8),
  contrast = matrix(1, 1, 1),
  reference_dose = 0,
  reference_type = "difference"
)

post_g_comp <- posterior_g_comp(
  mcmc,
  new_data = df,
  reference_dose = 0,
  reference_type = "difference"
)

pr_eoi_g_comp(
  mcmc,
  eoi = c(5, 8),
  new_data = df,
  reference_dose = 0,
  reference_type = "difference"
)

plot(mcmc, contrast = matrix(1, 1, 1))

# With covariates----

set.seed(1000)

x <-
  data.frame(
    gender = factor(sample(c("F", "M"), 40, replace = TRUE))
  ) %>%
  model.matrix(~ gender, data = .)

df_cov <-
  data_negbin_emax(
    n_per_arm = 10,
    doses = 0:3,
    b1 = c(0, 0.5),
    b2 = 2.5,
    b3 = 0.5,
    ps = 0.75,
    x = x
  ) %>%
  mutate(
    gender = case_when(
      genderM == 1 ~ "M",
      TRUE ~ "F"
    ),
    gender = factor(gender)
  ) %>%
  select(subject, dose, gender, response)

df_cov %>%
  group_by(dose, gender) %>%
  summarize(
    mean = mean(response),
    se = sd(response) / sqrt(n()),
    .groups = "drop"
  )

mcmc_cov <- beaver_mcmc(
  emax = model_negbin_emax(
    mu_b1 = 0,
    sigma_b1 = 10,
    mu_b2 = 0,
    sigma_b2 = 10,
    mu_b3 = 1.5,
    sigma_b3 = 3,
    w_prior = 1 / 4
  ),
  linear = model_negbin_linear(
    mu_b1 = 0,
    sigma_b1 = 10,
    mu_b2 = 0,
    sigma_b2 = 10,
    w_prior = 1 / 4
  ),
  quad = model_negbin_quad(
    mu_b1 = 0,
    sigma_b1 = 10,
    mu_b2 = 0,
    sigma_b2 = 10,
    mu_b3 = 1.5,
    sigma_b3 = 3,
    w_prior = 1 / 4
  ),
  exp = model_negbin_exp(
    mu_b1 = 0,
    sigma_b1 = 10,
    mu_b2 = 0,
    sigma_b2 = 10,
    mu_b3 = 0,
    sigma_b3 = 3,
    w_prior = 1 / 4
  ),
  formula = ~ gender,
  data = df_cov,
  n_iter = 1e2,
  n_chains = 1,
  quiet = TRUE
)

mcmc_cov$w_post

draws_cov <- try(draws(mcmc_cov)) #draws() is intended for single model fits only
draws_cov_emax <- draws(mcmc_cov$models$emax$mcmc)
draws_cov_linear <- draws(mcmc_cov$models$linear$mcmc)
draws_cov_quad <- draws(mcmc_cov$models$quad$mcmc)
draws_cov_exp <- draws(mcmc_cov$models$exp$mcmc)

post_cov <- posterior(
  mcmc_cov,
  contrast = matrix(c(1, 1, 0, 1), 2, 2),
  doses = 0:3,
  reference_dose = 0,
  reference_type = "difference"
)

pr_eoi(
  mcmc_cov,
  eoi = c(5, 8),
  contrast = matrix(c(1, 1, 0, 1), 2, 2),
  reference_dose = 0,
  reference_type = "difference"
)

post_g_comp_cov <- posterior_g_comp(
  mcmc_cov,
  new_data = df_cov,
  reference_dose = 0,
  reference_type = "difference"
)

pr_eoi_g_comp(
  mcmc_cov,
  eoi = c(5, 8),
  new_data = df_cov,
  reference_dose = 0,
  reference_type = "difference"
)

plot(mcmc_cov, new_data = df_cov, type = "g-comp")
}
