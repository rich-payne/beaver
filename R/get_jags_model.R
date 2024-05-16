get_jags_model <- function(model) UseMethod("get_jags_model")

#' @keywords internal
#' @export
get_jags_model.beaver_negbin_emax <- function(model) { # nolint
  fs::path_package("beaver", "jags", "negbin_emax.jags")
}

#' @keywords internal
#' @export
get_jags_model.beaver_negbin_sigmoid_emax <- function(model) { # nolint
  fs::path_package("beaver", "jags", "negbin_sigmoidemax.jags")
}

#' @keywords internal
#' @export
get_jags_model.beaver_negbin_linear <- function(model) { # nolint
  fs::path_package("beaver", "jags", "negbin_linear.jags")
}

#' @keywords internal
#' @export
get_jags_model.beaver_negbin_loglinear <- function(model) { # nolint
  fs::path_package("beaver", "jags", "negbin_loglinear.jags")
}

#' @keywords internal
#' @export
get_jags_model.beaver_negbin_quad <- function(model) { # nolint
  fs::path_package("beaver", "jags", "negbin_quad.jags")
}

#' @keywords internal
#' @export
get_jags_model.beaver_negbin_logquad <- function(model) { # nolint
  fs::path_package("beaver", "jags", "negbin_logquad.jags")
}

#' @keywords internal
#' @export
get_jags_model.beaver_negbin_exp <- function(model) { # nolint
  fs::path_package("beaver", "jags", "negbin_exp.jags")
}

#' @keywords internal
#' @export
get_jags_model.beaver_negbin_indep <- function(model) { # nolint
  fs::path_package("beaver", "jags", "negbin_indep.jags")
}
