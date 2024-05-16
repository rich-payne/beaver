add_class_and_attrs <- function(model, samples, data, x, formula) {
  UseMethod("add_class_and_attrs")
}

add_common_attrs <- function(samples, data, x, formula) {
  attr(samples, "doses") <- sort(unique(data$dose))
  attr(samples, "n_b1") <- sum(grepl("^b1", colnames(samples[[1]])))
  attr(samples, "covariate_names") <- colnames(x)
  attr(samples, "formula") <- formula
  samples
}

#' @keywords internal
#' @export
add_class_and_attrs.beaver_negbin_emax <- function(model, samples, data, x, formula) { # nolint
  samples <- add_common_attrs(samples, data, x, formula)
  class(samples) <- c(
    "beaver_mcmc_negbin_emax",
    "beaver_mcmc",
    class(samples)
  )
  return(samples)
}

#' @keywords internal
#' @export
add_class_and_attrs.beaver_negbin_sigmoid_emax <- function(model, samples, data, x, formula) { # nolint
  samples <- add_common_attrs(samples, data, x, formula)
  class(samples) <- c(
    "beaver_mcmc_negbin_sigmoid_emax",
    "beaver_mcmc",
    class(samples)
  )
  return(samples)
}

#' @keywords internal
#' @export
add_class_and_attrs.beaver_negbin_linear <- function(model, samples, data, x, formula) { # nolint
  samples <- add_common_attrs(samples, data, x, formula)
  class(samples) <- c(
    "beaver_mcmc_negbin_linear",
    "beaver_mcmc",
    class(samples)
  )
  return(samples)
}

#' @keywords internal
#' @export
add_class_and_attrs.beaver_negbin_loglinear <- function(model, samples, data, x, formula) { # nolint
  samples <- add_common_attrs(samples, data, x, formula)
  class(samples) <- c(
    "beaver_mcmc_negbin_loglinear",
    "beaver_mcmc",
    class(samples)
  )
  return(samples)
}

#' @keywords internal
#' @export
add_class_and_attrs.beaver_negbin_quad <- function(model, samples, data, x, formula) { # nolint
  samples <- add_common_attrs(samples, data, x, formula)
  class(samples) <- c(
    "beaver_mcmc_negbin_quad",
    "beaver_mcmc",
    class(samples)
  )
  return(samples)
}

#' @keywords internal
#' @export
add_class_and_attrs.beaver_negbin_logquad <- function(model, samples, data, x, formula) { # nolint
  samples <- add_common_attrs(samples, data, x, formula)
  class(samples) <- c(
    "beaver_mcmc_negbin_logquad",
    "beaver_mcmc",
    class(samples)
  )
  return(samples)
}

#' @keywords internal
#' @export
add_class_and_attrs.beaver_negbin_exp <- function(model, samples, data, x, formula) { # nolint
  samples <- add_common_attrs(samples, data, x, formula)
  class(samples) <- c(
    "beaver_mcmc_negbin_exp",
    "beaver_mcmc",
    class(samples)
  )
  return(samples)
}

#' @keywords internal
#' @export
add_class_and_attrs.beaver_negbin_indep <- function(model, samples, data, x, formula) { # nolint
  samples <- add_common_attrs(samples, data, x, formula)
  class(samples) <- c(
    "beaver_mcmc_negbin_indep",
    "beaver_mcmc",
    class(samples)
  )
  return(samples)
}
