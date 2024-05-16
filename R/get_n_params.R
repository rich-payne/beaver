get_n_params <- function(model) UseMethod("get_n_params")

#' @keywords internal
#' @export
get_n_params.beaver_mcmc_negbin_emax <- function(model) { # nolint
  n_p <- length(attr(model, "doses"))
  n_b1 <- attr(model, "n_b1")
  n_bs <- 2
  n_p + n_b1 + n_bs
}

#' @keywords internal
#' @export
get_n_params.beaver_mcmc_negbin_sigmoid_emax <- function(model) { # nolint
  n_p <- length(attr(model, "doses"))
  n_b1 <- attr(model, "n_b1")
  n_bs <- 3
  n_p + n_b1 + n_bs
}

#' @keywords internal
#' @export
get_n_params.beaver_mcmc_negbin_linear <- function(model) { # nolint
  n_p <- length(attr(model, "doses"))
  n_b1 <- attr(model, "n_b1")
  n_bs <- 1
  n_p + n_b1 + n_bs
}

#' @keywords internal
#' @export
get_n_params.beaver_mcmc_negbin_loglinear <-
  get_n_params.beaver_mcmc_negbin_linear

#' @keywords internal
#' @export
get_n_params.beaver_mcmc_negbin_quad <- function(model) { # nolint
  n_p <- length(attr(model, "doses"))
  n_b1 <- attr(model, "n_b1")
  n_bs <- 2
  n_p + n_b1 + n_bs
}

#' @keywords internal
#' @export
get_n_params.beaver_mcmc_negbin_logquad <-
  get_n_params.beaver_mcmc_negbin_quad

#' @keywords internal
#' @export
get_n_params.beaver_mcmc_negbin_exp <- function(model) { # nolint
  n_p <- length(attr(model, "doses"))
  n_b1 <- attr(model, "n_b1")
  n_bs <- 2
  n_p + n_b1 + n_bs
}

#' @keywords internal
#' @export
get_n_params.beaver_mcmc_negbin_indep <- function(model) { # nolint
  n_p <- length(attr(model, "doses"))
  n_b1 <- attr(model, "n_b1")
  n_bs <- n_p - 1
  n_p + n_b1 + n_bs
}
