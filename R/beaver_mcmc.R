#' Bayesian Model Averaging of Covariate Adjusted Neg-Binomial Dose-Response
#' @param data a dataframe with columns "dose", "response" and any covariates
#'   listed in the `formula` argument.
#' @param formula a right-hand sided formula specifying the covariates.
#' @param ... candidate models to be included in Bayesian model averaging.
#'   These should be created from calls to the `model_negbin_*` functions
#'   (e.g. `model_negbin_emax()`).
#' @param n_adapt the number of iterations used to tune the MCMC algorithm.
#' @param n_burn the number of MCMC iterations used for burn-in.
#' @param n_iter the number of MCMC iterations to save.
#' @param n_chains the number of MCMC chains.
#' @param thin thinning for the MCMC chain.
#' @param quiet logical indicating if MCMC chain progress output should be
#'   silenced.
#' @example man/examples/ex-beaver_mcmc.R
#' @family models
#' @family posterior calculations
#' @return A list (with appropriate S3 classes) with the prior and posterior
#'   weights, sampled model index, and individual MCMC fits.
#' @export
beaver_mcmc <- function(
  data,
  formula = ~ 1,
  ...,
  n_adapt = 1e3,
  n_burn = 1e3,
  n_iter = 1e4,
  n_chains = 4,
  thin = 1,
  quiet = FALSE
) {
  jags_modules <- rjags::list.modules()
  on.exit(restore_jags_modules(jags_modules))
  load_jags_modules()
  models <- list(...)
  assert_models(models)
  assert_prior_weights(models)
  assert_data(data, formula)
  x <- model.matrix(formula, data = data)
  mcmc <- purrr::map(
    models,
    run_mcmc,
    data = data,
    x = x,
    n_adapt = n_adapt,
    n_burn = n_burn,
    n_iter = n_iter,
    n_chains = n_chains,
    thin = thin,
    quiet = quiet,
    formula = formula,
    n_models = length(models)
  )
  bma_fit <- get_bma(mcmc, models)
  attr(bma_fit, "formula") <- formula
  attr(bma_fit, "doses") <- sort(unique(data$dose))
  class(bma_fit) <- c("beaver_mcmc_bma", class(bma_fit),  "beaver_mcmc")
  return(bma_fit)
}

run_mcmc <- function(
  model,
  data,
  x,
  n_adapt,
  n_burn,
  n_iter,
  n_chains,
  thin,
  quiet,
  formula,
  n_models
) {
  jags_data <- get_jags_data(model, data, x)
  mod <- rjags::jags.model(
    file = get_jags_model(model),
    data = jags_data,
    inits = set_jags_seed(n_chains),
    n.chains = n_chains,
    n.adapt = n_adapt,
    quiet = quiet
  )
  stats::update(mod, n.iter = n_burn, progress.bar = get_prog_bar(quiet))
  samples <- rjags::coda.samples(
    mod,
    variable.names = get_var_names(model),
    n.iter = n_iter,
    thin = thin,
    progress.bar = get_prog_bar(quiet)
  )
  samples <- rename_b1(samples, colnames(x))
  samples <- add_class_and_attrs(model, samples, data, x, formula)
  log_post_pred <- get_log_post_pred(samples, data, x, n_models)
  list(samples = samples, log_post_pred = log_post_pred)
}

restore_jags_modules <- function(original_modules) {
  current_modules <- rjags::list.modules()
  ind <- which(!(current_modules %in% original_modules))
  if (length(ind) > 0)
    purrr::walk(current_modules[ind], ~ rjags::unload.module(.x, quiet = TRUE))
}

load_jags_modules <- function() {
  rjags::load.module("glm", quiet = TRUE)
}

set_jags_seed <- function(n_chains) {
  purrr::map(
    1:n_chains,
    ~ list(
      .RNG.seed = sample(.Machine$integer.max, 1),
      .RNG.name = "base::Mersenne-Twister"
    )
  )
}

get_prog_bar <- function(quiet) {
  if (quiet) {
    progress_bar <- "none"
  } else {
    progress_bar <- "text"
  }
  progress_bar
}

rename_b1 <- function(samples, b1_names) {
  if (is.null(b1_names)) return(samples)
  for (i in seq_len(length(samples))) {
    param_names <- attr(samples[[i]], "dimnames")[[2]]
    ind <- grep("^b1", param_names)
    param_names[ind] <- b1_names
    attr(samples[[i]], "dimnames")[[2]] <- param_names
  }
  return(samples)
}

get_bma <- function(mcmc, models) {
  bma_args <- purrr::map2(mcmc, models, get_bma_arg)
  bma_fit <- rlang::exec(yodel::bma, !!!bma_args)
  return(bma_fit)
}

get_bma_arg <- function(mcmc, model) {
  yodel::model_bma_predictive(
    log_post_pred = mcmc$log_post_pred,
    adjustment = - get_n_params(mcmc$samples) / 2,
    w_prior = model$w_prior,
    mcmc = mcmc$samples,
    fun = posterior_bma
  )
}
