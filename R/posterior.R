#' Posterior Samples from Bayesian Model Averaging
#' @description Calculate posterior quantities of interest using Bayesian model
#'   averaging.
#' @param x an object output from `beaver_mcmc()`.
#' @param doses doses at which to obtain the posterior.
#' @param reference_dose dose to which to compare as either a difference or
#'   ratio.
#' @param prob the percentiles of the posterior to calculate for each dose.
#' @param return_stats logical indicating if the posterior mean and quantiles
#'   should be returned.
#' @param return_samples logical indicating if posterior mean samples should
#'   be returned.
#' @param new_data a dataframe for which the posterior will be calculated for
#'   each observation's covariate values.
#' @param contrast a matrix containing where each row contains a contrast for
#'   which the posterior will be calculated.
#' @param reference_type whether to provide the posterior of the difference or
#'   the ratio between each dose and the reference dose.
#' @param ... additional arguments will throw an error.
#' @return A list with the elements `stats` and `samples`. When using this
#'   function with default settings, `samples` is NULL and `stats` is a
#'   dataframe summarizing the posterior samples. `stats` contains, at a
#'   minimum, the columns "dose", ".contrast_index", "(Intercept)", "value",
#'   and variables corresponding to the values passed in `prob` ("2.50%" and
#'   "97.50%" by default). When `return_stats` is set to `FALSE`, `stats` is
#'   NULL. When `return_samples` is set to `TRUE`, `samples` is a dataframe
#'   with the posterior samples for each iteration of the MCMC. The dataframe
#'   will have, at a minimum, the columns "iter" and "model", indicating the
#'   MCMC iteration and the model that was used in the calculations, as well as
#'   the columns "dose", ".contrast_index", "(Intercept)", and "value". The
#'   functions used for each model are defined within the `model_negbin_XYZ()`
#'   functions and used in the `beaver_mcmc()` function.
#' @example man/examples/ex-beaver_mcmc.R
#' @family posterior calculations
#' @export
posterior.beaver_mcmc_bma <- function( # nolint
  x,
  doses = attr(x, "doses"),
  reference_dose = NULL,
  prob = c(.025, .975),
  return_stats = TRUE,
  return_samples = FALSE,
  new_data = NULL,
  contrast = NULL,
  reference_type = c("difference", "ratio"),
  ...
) {
  samps <- NextMethod("posterior", x)
  stats <- get_stats(samps, prob, return_stats)
  if (!return_samples) samps <- NULL
  out <- list(stats = stats, samples = samps)
  return(out)
}

#' @inherit posterior.beaver_mcmc_bma
#' @param x an object output from (internal function) `run_mcmc()`.
#' @return A list with the elements `stats` and `samples`. When using this
#'   function with default settings, `samples` is NULL and `stats` is a
#'   dataframe summarizing the posterior samples. `stats` contains, at a
#'   minimum, the columns "dose", ".contrast_index", "(Intercept)", "value",
#'   and variables corresponding to the values passed in `prob` ("2.50%" and
#'   "97.50%" by default). When `return_stats` is set to `FALSE`, `stats` is
#'   NULL. When `return_samples` is set to `TRUE`, `samples` is a dataframe
#'   with the posterior samples for each iteration of the MCMC. The dataframe
#'   will have, at a minimum, the column "iter", indicating the MCMC iteration,
#'   as well as the columns "dose", ".contrast_index", "(Intercept)", and
#'   "value". The functions used for each model are defined within the
#'   `model_negbin_XYZ()` functions and used in the `run_mcmc()` function.
#' @family posterior calculations
#' @export
posterior.beaver_mcmc <- function( # nolint
  x,
  doses = attr(x, "doses"),
  reference_dose = NULL,
  prob = c(.025, .975),
  return_stats = TRUE,
  return_samples = FALSE,
  new_data = NULL,
  contrast = NULL,
  reference_type = c("difference", "ratio"),
  ...
) {
  ellipsis::check_dots_empty()
  reference_type <- match.arg(reference_type)
  assert_doses(x, doses)
  assert_doses(x, reference_dose)
  assert_length(reference_dose, 1, can_be_null = TRUE)
  doses <- unique(c(reference_dose, doses))
  draws <- draws(x)
  contrast <- get_contrast(x, new_data, contrast)
  samps <- get_samps(x, draws, doses, contrast)
  samps <- adjust_reference(
    x,
    samps,
    draws,
    contrast,
    reference_dose,
    reference_type
  )
  stats <- get_stats(samps, prob, return_stats)
  if (!return_samples) samps <- NULL
  out <- list(stats = stats, samples = samps)
  return(out)
}

posterior_bma <- function(
  x,
  doses = attr(x, "doses"),
  reference_dose = NULL,
  prob = c(.025, .975),
  return_stats, # needed, but not used
  return_samples, # needed, but not used
  new_data = NULL,
  contrast = NULL,
  reference_type = c("difference", "ratio"),
  ...
) {
  posterior(
    x = x,
    doses = doses,
    reference_dose = reference_dose,
    prob = prob,
    new_data = new_data,
    contrast = contrast,
    reference_type = reference_type,
    return_stats = FALSE,
    return_samples = TRUE,
    ...
  )$samples
}

get_samps <- function(x, draws, doses, contrast) {
  dose_contrast_grid <- tidyr::expand_grid(
    dose = doses,
    tibble::tibble(
      .contrast_index = seq_len(nrow(contrast)),
      contrast = contrast_to_list(contrast)
    )
  )
  samps <- purrr::pmap_dfr(
    dose_contrast_grid,
    post_mean,
    x = x,
    samps = draws
  )
  invisible(samps)
}

adjust_reference <- function(
  x,
  samps,
  draws,
  contrast,
  reference_dose,
  reference_type
) {
  if (is.null(reference_dose)) return(samps)
  samps_ref <- get_samps(x, draws, reference_dose, contrast)
  adjust_reference_impl(samps, samps_ref, reference_type, reference_dose)
}

adjust_reference_impl <- function(
  samps,
  samps_ref,
  reference_type,
  reference_dose
) {
  samples <- dplyr::left_join(
    samps,
    samps_ref,
    by = c("iter", ".contrast_index"),
    suffix = c("", "_ref")
  )
  if (reference_type == "ratio") {
    samples <- samples %>%
      dplyr::mutate(
        value = .data$value / .data$value_ref,
        reference_dose = !!reference_dose
      )
  } else if (reference_type == "difference") {
    samples <- samples %>%
      dplyr::mutate(
        value = .data$value - .data$value_ref,
        reference_dose = !!reference_dose
      )
  }
  samples %>%
    dplyr::mutate(reference_type = !!reference_type) %>%
    dplyr::select(- ends_with("_ref"))
}

get_stats <- function(
  samples,
  prob,
  return_stats
) {
  if (!return_stats) return(NULL)
  assertNumeric(
    prob,
    lower = 0,
    upper = 1,
    any.missing = FALSE,
    len = 2,
    sorted = TRUE
  )
  stats <- samples %>%
    dplyr::group_by(across(- any_of(c("value", "iter", "model")))) %>%
    dplyr::summarize(
      across(
        all_of("value"),
        list(
          value = ~ mean(.x),
          qtile_l = ~ quantile(.x, prob = !!prob[1], names = FALSE),
          qtile_u = ~ quantile(.x, prob = !!prob[2], names = FALSE)
        )
      ),
      .groups = "keep"
    ) %>%
    tidyr::pivot_longer(
      cols = contains("qtile"),
      names_to = "names",
      values_to = "value_qtile"
    ) %>%
    dplyr::select(- "names") %>%
    dplyr::mutate(prob = prob) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(prob = sprintf("%.2f%%", 100 * prob)) %>%
    tidyr::pivot_wider(names_from = "prob", values_from = "value_qtile") %>%
    dplyr::rename(value = "value_value")
  return(stats)
}

contrast_to_list <- function(contrast) {
  out <- list()
  for (i in seq_len(nrow(contrast))) {
    out[[i]] <- contrast[i, , drop = FALSE]
  }
  out
}

get_contrast <- function(x, new_data, contrast) {
  assert_new_data_and_contrast(new_data, contrast)
  if (!is.null(contrast)) {
    colnames(contrast) <- attr(x, "covariate_names")
    return(contrast)
  }
  form <- attr(x, "formula")
  contrast <- model.matrix(form, data = new_data)
}

post_mean <- function(x, dose, samps, intercept, contrast, .contrast_index) {
  intercept <- get_intercept(samps, contrast)
  u_doses <- attr(x, "doses")
  samps %>%
    dplyr::mutate(
      value = c(get_mean(
        !!x, !!dose, !!intercept, !!samps, u_doses = !!u_doses
      )),
      dose = !!dose,
      .contrast_index = !!.contrast_index
    ) %>%
    select_cols() %>%
    dplyr::bind_cols(tibble::as_tibble(contrast))
}

get_intercept <- function(mcmc, contrast) {
  b1 <- dplyr::select(
    mcmc,
    - matches("^b[[:digit:]]*$"),
    - matches("^b2\\[[[:digit:]]\\]*$"),
    - matches("^p\\[[[:digit:]]*\\]$"),
    - "iter"
  ) %>%
    as.matrix()
  intercept <- b1 %*% t(contrast)
  return(intercept)
}

select_cols <- function(x) {
  dplyr::select(
    x,
    "dose",
    "value",
    "iter",
    ".contrast_index",
    - matches("^b[[:digit:]]*$"),
    - matches("^p\\[[[:digit:]]*\\]$")
  )
}

#' Compute Posterior G-Computation Estimate
#' @description Calculate the estimated effect for each observation
#'   at each dose and average over all observations.  This function calculates
#'   the posterior marginal treatment effect at each dose.
#' @inheritParams posterior.beaver_mcmc_bma
#' @param x an object output from `beaver_mcmc()` or (internal function)
#'   `run_mcmc()`.
#' @param new_data a dataframe containing all the variables used in the
#'   covariate adjustments to the model used to obtain `x`.  Usually this
#'   will be the same dataframe used to fit the model.
#' @return A list with the elements `stats` and `samples`. When using this
#'   function with default settings, `samples` is NULL and `stats` is a
#'   dataframe summarizing the posterior samples. `stats` contains, at a
#'   minimum, the columns "dose", "value", and variables corresponding to the
#'   values passed in `prob` ("2.50%" and "97.50%" by default). When
#'   `return_stats` is set to `FALSE`, `stats` is NULL. When `return_samples`
#'   is set to `TRUE`, `samples` is a dataframe with the posterior samples for
#'   each iteration of the MCMC.
#'   \describe{
#'     \item{When x is of class 'beaver_mcmc_bma':}{
#'       The dataframe will have, at a minimum, the columns "iter" and "model",
#'       indicating the MCMC iteration and the model that was used in the
#'       calculations, as well as the columns "dose" and "value". The functions
#'        used for each model are defined within the `model_negbin_XYZ()`
#'        functions and used in the `beaver_mcmc()` function.
#'     }
#'     \item{When x is of class 'beaver_mcmc':}{
#'       The dataframe will have, at a minimum, the column "iter", indicating
#'       the MCMC iteration, as well as the columns "dose" and "value". The
#'       functions used for each model are defined within the
#'       `model_negbin_XYZ()` functions and used in the `run_mcmc()` function.
#'     }
#'   }
#' @example man/examples/ex-beaver_mcmc.R
#' @family posterior calculations
#' @export
posterior_g_comp <- function(
  x,
  doses = attr(x, "doses"),
  reference_dose = NULL,
  prob = c(.025, .975),
  return_stats = TRUE,
  return_samples = FALSE,
  new_data = NULL,
  reference_type = c("difference", "ratio")
) {
  reference_type <- match.arg(reference_type)
  assert_doses(x, doses)
  assert_doses(x, reference_dose)
  assert_length(reference_dose, 1, can_be_null = TRUE)
  doses <- unique(c(reference_dose, doses))
  assert_doses(x, unique(new_data$dose))
  value_sum <- 0
  n_new_data <- nrow(new_data)
  for (i in seq_len(n_new_data)) {
    post_single <- posterior(
      x,
      dose = doses,
      prob = prob,
      new_data = new_data[i, ],
      return_stats = FALSE,
      return_samples = TRUE
    )$samples
    if (i == 1) {
      samps <- post_single
    }
    value_sum <- value_sum + post_single$value
  }
  samps$value <- value_sum / n_new_data
  if (!is.null(reference_dose)) {
    samps_ref <- samps %>%
      dplyr::filter(.data$dose == !!reference_dose)
    samps <- adjust_reference_impl(
      samps,
      samps_ref,
      reference_type,
      reference_dose
    )
  }
  samps <- dplyr::select(
    samps,
    any_of(
      c("dose", "value", "iter", "model", "reference_dose", "reference_type")
    )
  )
  stats <- get_stats(samps, prob, return_stats)
  if (!return_samples) samps <- NULL
  output <- list(stats = stats, samples = samps)
  return(output)
}
