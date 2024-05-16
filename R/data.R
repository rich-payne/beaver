#' Generate data from a negative binomial EMAX model
#' @param n_per_arm number of subjects in each dose arm.
#' @param doses doses at which to simulate subjects.
#' @param b1,b2,b3,ps parameters from which to simulate data.  See model
#'   description below.  If covariates are specified (through `x`), then
#'   `b1` should be a vector of length `ncol(x)`.
#' @param x the model matrix for the covariates.  Must have the same number of
#'   rows as the total number of subjects
#'   (`sum(n_per_arm * rep(1, length(doses)))`).  If `NULL`, then an
#'   intercept term is used by default.
#' @example man/examples/ex-beaver_mcmc.R
#' @return A dataframe with columns "subject", "dose", and "response".
#' @inheritSection model_negbin_emax Negative Binomial EMAX
#' @export
data_negbin_emax <- function(n_per_arm, doses, b1, b2, b3, ps, x = NULL) {
  assert_matrix(x, can_be_null = TRUE)
  covariates <- !is.null(x)
  nx <- sum(n_per_arm * rep(1, length(doses)))
  if (!covariates) {
    x <- matrix(1, nx, 1)
    colnames(x) <- "intercept"
  } else {
    assert_n_rows(x, nx)
  }
  assert_length(b1, ncol(x))
  assert_length(b2, 1)
  assert_length(b3, 1)
  assert_misc_lengths(ps, doses)
  assert_misc_lengths(n_per_arm, doses)
  data <- purrr::pmap_dfr(
    tibble::tibble(n_per_arm = n_per_arm, doses = doses, ps = ps),
    function(n_per_arm, doses, ps) {
      tidyr::expand_grid(
        subject = 1:n_per_arm,
        data.frame(dose = doses, p = ps)
      )
    }
  ) %>%
    dplyr::bind_cols(tibble::as_tibble(x)) %>%
    dplyr::mutate(
      subject = seq_len(n()),
      b1 = c(x %*% !!b1),
      mu = mean_negbin_emax(.data$dose, .data$b1, !!b2, !!b3),
      r = .data$mu * .data$p / (1 - .data$p)
    )
  assert_r_positive(data)
  data <- dplyr::mutate(data, response = stats::rnbinom(n(), .data$r, .data$p))
  if (!covariates) {
    data <- dplyr::select(data, - "intercept", - "b1")
  }
  return(data)
}
