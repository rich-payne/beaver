mean_negbin_emax <- function(x, b1, b2, b3) {
  exp(b1 + b2 * x / (b3 + x))
}

mean_negbin_sigmoid_emax <- function(x, b1, b2, b3, b4) {
  exp(b1 + b2 * x ^ b4 / (b3 ^ b4 + x ^ b4))
}

mean_negbin_linear <- function(x, b1, b2) {
  exp(b1 + b2 * x)
}

mean_negbin_loglinear <- function(x, b1, b2) {
  exp(b1 + b2 * log(1 + x))
}

mean_negbin_quad <- function(x, b1, b2, b3) {
  exp(b1 + b2 * x + b3 * x ^ 2)
}

mean_negbin_logquad <- function(x, b1, b2, b3) {
  exp(b1 + b2 * log(1 + x) + b3 * log(1 + x) ^ 2)
}

mean_negbin_exp <- function(x, b1, b2, b3) {
  exp(b1 + b2 * (1 - exp(- b3 * x)))
}

get_mean <- function(samples, doses, intercept, samps, ...) {
  UseMethod("get_mean")
}

#' @keywords internal
#' @export
get_mean.beaver_mcmc_negbin_emax <- function( # nolint
  samples,
  doses,
  intercept,
  samps = NULL,
  ...
) {
  if (is.null(samps))
    samps <- draws(samples)
  mean_negbin_emax(doses, intercept, samps$b2, samps$b3)
}

#' @keywords internal
#' @export
get_mean.beaver_mcmc_negbin_sigmoid_emax <- function( # nolint
  samples,
  doses,
  intercept,
  samps = NULL,
  ...
) {
  if (is.null(samps))
    samps <- draws(samples)
  mean_negbin_sigmoid_emax(doses, intercept, samps$b2, samps$b3, samps$b4)
}

#' @keywords internal
#' @export
get_mean.beaver_mcmc_negbin_linear <- function( # nolint
  samples,
  doses,
  intercept,
  samps = NULL,
  ...
) {
  if (is.null(samps))
    samps <- draws(samples)
  mean_negbin_linear(doses, intercept, samps$b2)
}

#' @keywords internal
#' @export
get_mean.beaver_mcmc_negbin_loglinear <- function( # nolint
  samples,
  doses,
  intercept,
  samps = NULL,
  ...
) {
  if (is.null(samps))
    samps <- draws(samples)
  mean_negbin_loglinear(doses, intercept, samps$b2)
}

#' @keywords internal
#' @export
get_mean.beaver_mcmc_negbin_quad <- function( # nolint
  samples,
  doses,
  intercept,
  samps = NULL,
  ...
) {
  if (is.null(samps))
    samps <- draws(samples)
  mean_negbin_quad(doses, intercept, samps$b2, samps$b3)
}

#' @keywords internal
#' @export
get_mean.beaver_mcmc_negbin_logquad <- function( # nolint
  samples,
  doses,
  intercept,
  samps = NULL,
  ...
) {
  if (is.null(samps))
    samps <- draws(samples)
  mean_negbin_logquad(doses, intercept, samps$b2, samps$b3)
}

#' @keywords internal
#' @export
get_mean.beaver_mcmc_negbin_exp <- function( # nolint
  samples,
  doses,
  intercept,
  samps = NULL,
  ...
) {
  if (is.null(samps))
    samps <- draws(samples)
  mean_negbin_exp(doses, intercept, samps$b2, samps$b3)
}

#' @keywords internal
#' @export
get_mean.beaver_mcmc_negbin_indep <- function( # nolint
  samples,
  doses,
  intercept,
  samps = NULL,
  u_doses,
  ...
) {
  # (N x k) * (k X MCMC)
  if (is.null(samps))
    samps <- draws(samples)
  doses <- matrix(doses, nrow(intercept), ncol(intercept))[1, ]
  ind <- vapply(
    doses,
    function(xx, u_doses) which(xx == u_doses),
    integer(1),
    u_doses = u_doses
  )
  x_b2 <- model.matrix(
    ~ b2, data = data.frame(b2 = factor(ind, levels = seq_along(u_doses)))
  )
  b2 <- as.matrix(dplyr::select(samps, starts_with("b2")))
  b2_mat <- t(x_b2 %*% t(b2))
  exp(intercept + b2_mat)
}
