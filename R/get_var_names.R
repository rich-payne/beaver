get_var_names <- function(model) UseMethod("get_var_names")

#' @keywords internal
#' @export
get_var_names.beaver_negbin_emax <- function(model) { # nolint
  c("b1", "b2", "b3", "p")
}

#' @keywords internal
#' @export
get_var_names.beaver_negbin_sigmoid_emax <- function(model) { # nolint
  c("b1", "b2", "b3", "b4", "p")
}

#' @keywords internal
#' @export
get_var_names.beaver_negbin_linear <- function(model) { # nolint
  c("b1", "b2", "p")
}

#' @keywords internal
#' @export
get_var_names.beaver_negbin_loglinear <-
  get_var_names.beaver_negbin_linear

#' @keywords internal
#' @export
get_var_names.beaver_negbin_quad <- function(model) { # nolint
  c("b1", "b2", "b3", "p")
}

#' @keywords internal
#' @export
get_var_names.beaver_negbin_logquad <-
  get_var_names.beaver_negbin_quad

#' @keywords internal
#' @export
get_var_names.beaver_negbin_exp <- function(model) { # nolint
  c("b1", "b2", "b3", "p")
}

#' @keywords internal
#' @export
get_var_names.beaver_negbin_indep <- function(model) { # nolint
  c("b1", "b2", "p")
}
