#' Calculate Probability of Meeting Effect of Interest
#' @description Calculate a posterior quantity such as
#'   Pr(trt_arm1 - trt_arm2 > eoi)
#' @inheritParams posterior.beaver_mcmc_bma
#' @param x an object output from `beaver_mcmc()` or (internal function)
#'   `run_mcmc()`.
#' @param eoi effects of interest in the probability equation.
#' @param direction calculate whether the posterior quantity is greater or less
#'   than the eoi
#' @return A dataframe or tibble with the posterior quantities.
#' @example man/examples/ex-beaver_mcmc.R
#' @family posterior calculations
#' @export
pr_eoi <- function( # nolint
  x,
  eoi,
  doses = attr(x, "doses"),
  reference_dose = NULL,
  new_data = NULL,
  contrast = NULL,
  reference_type = c("difference", "ratio"),
  direction = c("greater", "less")
) {
  direction <- match.arg(direction)
  samps <- posterior(
    x,
    doses = doses,
    reference_dose = reference_dose,
    new_data = new_data,
    contrast = contrast,
    reference_type = reference_type,
    return_stats = FALSE,
    return_samples = TRUE
  )$samples

  out <- purrr::map_dfr(eoi, pr_eoi_impl, x = samps) %>%
    apply_direction(direction) %>%
    dplyr::select(
      "dose", "eoi", "prob", "direction", everything()
    )
  return(out)
}

pr_eoi_impl <- function(x, eoi) {
  x %>%
    dplyr::group_by(across(- any_of(c("value", "iter", "model")))) %>%
    dplyr::summarize(prob = mean(.data$value > !!eoi)) %>%
    dplyr::mutate(eoi = !!eoi)
}

apply_direction <- function(x, direction) {
  x <- dplyr::mutate(x, direction = !!direction)
  if (direction == "greater") return(x)
  x %>%
    dplyr::mutate(prob = 1 - .data$prob)
}

#' Calculate Probability of Meeting Effect of Interest using G-Computation
#' @description Calculate a posterior quantity such as
#'   Pr(trt_arm1 - trt_arm2 > eoi) based on the posterior marginal treatment
#'   effect at each dose.
#' @inheritParams pr_eoi
#' @param new_data a dataframe containing all the variables used in the
#'   covariate adjustments to the model used to obtain `x`.  Usually this
#'   will be the same dataframe used to fit the model.
#' @inherit pr_eoi return
#' @example man/examples/ex-beaver_mcmc.R
#' @family posterior calculations
#' @export
pr_eoi_g_comp <- function( # nolint
  x,
  eoi,
  doses = attr(x, "doses"),
  reference_dose = NULL,
  new_data = NULL,
  reference_type = c("difference", "ratio"),
  direction = c("greater", "less")
) {
  direction <- match.arg(direction)
  samps <- posterior_g_comp(
    x,
    doses = doses,
    reference_dose = reference_dose,
    new_data = new_data,
    reference_type = reference_type,
    return_stats = FALSE,
    return_samples = TRUE
  )$samples
  out <- purrr::map_dfr(eoi, pr_eoi_impl, x = samps) %>%
    apply_direction(direction) %>%
    dplyr::select(
      "dose", "eoi", "prob", "direction", everything()
    )
  return(out)
}
