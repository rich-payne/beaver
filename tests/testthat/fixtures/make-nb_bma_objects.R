
#General----

formula <- ~ 1
formula_cov <- ~ age


#Functions----

makemean_negbin_linear <- function(x,
                                   b1,
                                   b2) {
  exp(b1 + b2 * x)
}

makemean_negbin_quad <- function(x,
                                 b1,
                                 b2,
                                 b3) {
  exp(b1 + b2 * x + b3 * x ^ 2)
}

makemean_negbin_emax <- function(x,
                                 b1,
                                 b2,
                                 b3) {
  exp(b1 + b2 * x / (b3 + x))
}

makemean_negbin_exp <- function(x,
                                b1,
                                b2,
                                b3) {
  exp(b1 + b2 * (1 - exp(- b3 * x)))
}

makeget_log_post_pred <- function(samples,
                                  data,
                                  x,
                                  n_models) {
  samps <- dplyr::as_tibble(as.matrix(samples)) %>%
    dplyr::mutate(iter = seq_len(n()))
  n_mcmc <- nrow(samps)

  data_doses <- data %>%
    dplyr::distinct(.data$dose) %>%
    dplyr::arrange(.data$dose) %>%
    dplyr::pull(dose)

  data <- data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(dose_index = which(.data$dose == data_doses)) %>%
    dplyr::ungroup()

  b1 <-
    dplyr::select(
      samps,
      - matches("^b[[:digit:]]*$"),
      - matches("^b2\\[[[:digit:]]\\]*$"),
      - matches("^p\\[[[:digit:]]*\\]$"),
      - "iter"
    ) %>%
    as.matrix()
  intercept <- b1 %*% t(x)

  doses <- rep(data$dose, each = n_mcmc)

  b_params <- grep("^b", names(samps), value = TRUE)
  b_params_list <- lapply(b_params, function(x) samps[[x]])
  names(b_params_list) <- b_params

  mu <- do.call(
    what = paste0(
      "makemean_", stringr::word(class(samples)[1], 3, 4, sep = "_")
    ),
    args = c(
      list(
        x = doses,
        b1 = intercept
      ),
      b_params_list
    )
  )

  p <- matrix(NA_real_, nrow(samps), length(data$dose_index))
  for (i in seq_len(length(data$dose_index))) {
    p[, i] <- samps[[paste0("p[", data$dose_index[i], "]")]]
  }
  r <- mu * p / (1 - p)
  log_post_pred <- dnbinom(data$response, size = r, prob = p, log = TRUE)

  return(log_post_pred)
}


#Load data----

#>Monotone increasing----

nb_monotone_incr <- readRDS(test_path("fixtures", "nb_monotone_incr.rds"))

nb_monotone_incr_doses <- nb_monotone_incr %>%
  dplyr::distinct(dose) %>%
  dplyr::arrange(dose) %>%
  dplyr::pull(dose)

nb_monotone_incr_x <- model.matrix(formula, data = nb_monotone_incr)

#>Monotone increasing w/covariates----

nb_monotone_incr_cov <- readRDS(test_path("fixtures", "nb_monotone_incr_cov.rds")) # nolint

nb_monotone_incr_cov_doses <- nb_monotone_incr_cov %>%
  dplyr::distinct(dose) %>%
  dplyr::arrange(dose) %>%
  dplyr::pull(dose)

nb_monotone_incr_cov_x <- model.matrix(formula_cov, data = nb_monotone_incr_cov)


#BMA object: Negative Binomial Linear and Negative Binomial Quadratic----

load(test_path("fixtures", "nb_linear_mcmc+_objects.Rdata"))
load(test_path("fixtures", "nb_quad_mcmc+_objects.Rdata"))

models <- list(
  linear = list(
    mu_b1 = priors_nb_linear$mu_b1,
    sigma_b1 = priors_nb_linear$sigma_b1,
    mu_b2 = priors_nb_linear$mu_b2,
    sigma_b2 = priors_nb_linear$sigma_b2,
    w_prior = 1 / 2
  ),
  quad = list(
    mu_b1 = priors_nb_quad$mu_b1,
    sigma_b1 = priors_nb_quad$sigma_b1,
    mu_b2 = priors_nb_quad$mu_b2,
    sigma_b2 = priors_nb_quad$sigma_b2,
    mu_b3 = priors_nb_quad$mu_b3,
    sigma_b3 = priors_nb_quad$sigma_b3,
    w_prior = 1 / 2
  )
)
class(models[["linear"]]) <- c("beaver_negbin_linear", "beaver_model")
class(models[["quad"]]) <- c("beaver_negbin_quad", "beaver_model")

nb_linear_log_post_pred <- makeget_log_post_pred(
  samples = nb_linear_model_samples_updatedattr,
  data = nb_monotone_incr,
  x = nb_monotone_incr_x,
  n_models = 2
)
nb_quad_log_post_pred <- makeget_log_post_pred(
  samples = nb_quad_model_samples_updatedattr,
  data = nb_monotone_incr,
  x = nb_monotone_incr_x,
  n_models = 2
)

nb_linear <- yodel::model_bma_predictive(
  log_post_pred = nb_linear_log_post_pred,
  adjustment = - (
    length(attr(nb_linear_model_samples_updatedattr, "doses")) +
      attr(nb_linear_model_samples_updatedattr, "n_b1") +
      1
  ) / 2,
  w_prior = models$linear$w_prior,
  mcmc = nb_linear_model_samples_updatedattr,
  fun = posterior_bma #update here later?? does not appear to be called in initial unit tests. # nolint
)
nb_quad <- yodel::model_bma_predictive(
  log_post_pred = nb_quad_log_post_pred,
  adjustment = - (
    length(attr(nb_quad_model_samples_updatedattr, "doses")) +
      attr(nb_quad_model_samples_updatedattr, "n_b1") +
      2
  ) / 2,
  w_prior = models$quad$w_prior,
  mcmc = nb_quad_model_samples_updatedattr,
  fun = posterior_bma #update here later?? does not appear to be called in initial unit tests. # nolint
)
args <- list(linear = nb_linear, quad = nb_quad, seed = 1234)
nb_bma <- rlang::exec(yodel::bma, !!!args)
attr(nb_bma, "formula") <- formula
attr(nb_bma, "doses") <- nb_monotone_incr_doses
class(nb_bma) <- c("beaver_mcmc_bma", class(nb_bma),  "beaver_mcmc")
attr(nb_bma$models[[1]]$fun, "srcref") <- NULL
attr(nb_bma$models[[2]]$fun, "srcref") <- NULL


#>Save----

save(
  nb_bma,
  file = test_path("fixtures", "nb_bma_objects.Rdata")
)


# nolint start
#BMA object: Negative Binomial Emax and Negative Binomial Exponential (both with covariates)----
# nolint end

load(test_path("fixtures", "nb_emax_cov_mcmc+_objects.Rdata"))
load(test_path("fixtures", "nb_exp_cov_mcmc+_objects.Rdata"))

models_cov <- list(
  emax = list(
    mu_b1 = priors_nb_emax$mu_b1,
    sigma_b1 = priors_nb_emax$sigma_b1,
    mu_b2 = priors_nb_emax$mu_b2,
    sigma_b2 = priors_nb_emax$sigma_b2,
    mu_b3 = priors_nb_emax$mu_b3,
    sigma_b3 = priors_nb_emax$sigma_b3,
    w_prior = 1 / 2
  ),
  exp = list(
    mu_b1 = priors_nb_exp$mu_b1,
    sigma_b1 = priors_nb_exp$sigma_b1,
    mu_b2 = priors_nb_exp$mu_b2,
    sigma_b2 = priors_nb_exp$sigma_b2,
    mu_b3 = priors_nb_exp$mu_b3,
    sigma_b3 = priors_nb_exp$sigma_b3,
    w_prior = 1 / 2
  )
)
class(models_cov[["emax"]]) <- c("beaver_negbin_emax", "beaver_model")
class(models_cov[["exp"]]) <- c("beaver_negbin_exp", "beaver_model")

nb_emax_log_post_pred <- makeget_log_post_pred(
  samples = nb_emax_model_cov_samples_updatedattr,
  data = nb_monotone_incr_cov,
  x = nb_monotone_incr_cov_x,
  n_models = 2
)
nb_exp_log_post_pred <- makeget_log_post_pred(
  samples = nb_exp_model_cov_samples_updatedattr,
  data = nb_monotone_incr_cov,
  x = nb_monotone_incr_cov_x,
  n_models = 2
)

nb_emax <- yodel::model_bma_predictive(
  log_post_pred = nb_emax_log_post_pred,
  adjustment = - (
    length(attr(nb_emax_model_cov_samples_updatedattr, "doses")) +
      attr(nb_emax_model_cov_samples_updatedattr, "n_b1") +
      2
  ) / 2,
  w_prior = models_cov$emax$w_prior,
  mcmc = nb_emax_model_cov_samples_updatedattr,
  fun = posterior_bma #update here later?? does not appear to be called in initial unit tests. # nolint
)
nb_exp <- yodel::model_bma_predictive(
  log_post_pred = nb_exp_log_post_pred,
  adjustment = - (
    length(attr(nb_exp_model_cov_samples_updatedattr, "doses")) +
      attr(nb_exp_model_cov_samples_updatedattr, "n_b1") +
      2
  ) / 2,
  w_prior = models_cov$exp$w_prior,
  mcmc = nb_exp_model_cov_samples_updatedattr,
  fun = posterior_bma #update here later?? does not appear to be called in initial unit tests. # nolint
)
args_cov <- list(emax = nb_emax, exp = nb_exp, seed = 1234)
nb_bma_cov <- rlang::exec(yodel::bma, !!!args_cov)
attr(nb_bma_cov, "formula") <- formula_cov
attr(nb_bma_cov, "doses") <- nb_monotone_incr_cov_doses
class(nb_bma_cov) <- c("beaver_mcmc_bma", class(nb_bma_cov),  "beaver_mcmc")
attr(nb_bma_cov$models[[1]]$fun, "srcref") <- NULL
attr(nb_bma_cov$models[[2]]$fun, "srcref") <- NULL


#>Save----

save(
  nb_bma_cov,
  file = test_path("fixtures", "nb_bma_cov_objects.Rdata")
)


#Cleanup----

rm(formula, formula_cov, models, models_cov, args, args_cov, nb_bma, nb_bma_cov)
rm(
  makemean_negbin_linear,
  makemean_negbin_quad,
  makemean_negbin_emax,
  makemean_negbin_exp,
  makeget_log_post_pred
)
rm(list = ls(pattern = "nb_monotone_incr"))
rm(list = ls(pattern = "nb_"))
