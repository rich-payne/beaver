#' Posterior Draws
#' @description Extracts posterior draws and puts them into a dataframe or
#'   tibble.
#' @param x MCMC output.
#' @param ... additional arguments passed to methods.
#' @return
#' \describe{
#'   \item{For generic:}{
#'     See specific method.
#'   }
#' }
#' @example man/examples/ex-beaver_mcmc.R
#' @export
draws <- function(x, ...) {
  UseMethod("draws", x)
}

#' @rdname draws
#' @return
#' \describe{
#'   \item{For class 'beaver_mcmc':}{
#'     A dataframe or tibble of MCMC draws.
#'   }
#' }
#' @export
draws.beaver_mcmc <- function(x, ...) { # nolint
  ellipsis::check_dots_empty()
  draws <- dplyr::as_tibble(as.matrix(x)) %>%
    dplyr::mutate(iter = seq_len(n()))
  return(draws)
}

#' @rdname draws
#' @return
#' \describe{
#'   \item{For class 'beaver_mcmc_bma':}{
#'     An error.
#'   }
#' }
#' @export
draws.beaver_mcmc_bma <- function(x, ...) { # nolint
  stop("draws is intended for single model fits only")
}
