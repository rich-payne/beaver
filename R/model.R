#' Negative Binomial EMAX Dose Response
#' @description Model settings for a negative binomial distribution assuming
#'   an EMAX Model on the mean.  This function is to be used within a call
#'   to `beaver_mcmc()`.
#' @param mu_b1,sigma_b1,mu_b2,sigma_b2,mu_b3,sigma_b3 hyperparameters.  See
#'   the model description below for context.
#' @param w_prior the prior weight for the model.
#' @section Negative Binomial EMAX:
#'   Let \eqn{y_{ij}} be the \eqn{j}th subject on dose \eqn{d_i}.
#'   The model is
#'   \deqn{y_{ij} ~ NB(p_i, r_i)}
#'   \deqn{p_i ~ Uniform(0, 1)}
#'   \deqn{r_{ij} = (\mu_{ij} * p_i) / (1 - p_i)}
#'   \deqn{log(\mu_{ij}) = x_{ij} * b1 + b2 * d_i / (b3 + d_i)}
#'   \deqn{b1 ~ N(`mu_b1`, `sigma_b1`^2)}
#'   \deqn{b2 ~ N(`mu_b2`, `sigma_b2`^2)}
#'   \deqn{b3 ~ N(`mu_b3`, `sigma_b3`^2) (Truncated to be positive)}
#'   The model is parameterized in terms of the mean of the
#'     negative binomial distribution and the usual probability parameter p.
#'     The prior on the mean is an EMAX
#'     model, and the prior on p at each dose is Uniform(0, 1).
#'     The model can adjust for baseline covariates, (\deqn{x_{ij}}).
#' @family models
#' @return A list with the model's prior weight and hyperparameter values.
#' @export
model_negbin_emax <- function(
  mu_b1,
  sigma_b1,
  mu_b2,
  sigma_b2,
  mu_b3,
  sigma_b3,
  w_prior = 1
) {
  out <- list(
    mu_b1 = mu_b1,
    sigma_b1 = sigma_b1,
    mu_b2 = mu_b2,
    sigma_b2 = sigma_b2,
    mu_b3 = mu_b3,
    sigma_b3 = sigma_b3,
    w_prior = w_prior
  )
  class(out) <- c("beaver_negbin_emax", "beaver_model")
  return(out)
}

#' Negative Binomial Sigmoidal EMAX Dose Response
#' @description Model settings for a negative binomial distribution assuming
#'   a Sigmoidal EMAX Model on the mean.  This function is to be used within a
#'   call to `beaver_mcmc()`.
#' @param mu_b1,sigma_b1,mu_b2,sigma_b2,mu_b3,sigma_b3,mu_b4,sigma_b4
#'   hyperparameters.  See the model description below for context.
#' @param w_prior the prior weight for the model.
#' @section Negative Binomial Sigmoidal EMAX:
#'   Let \eqn{y_{ij}} be the \eqn{j}th subject on dose \eqn{d_i}.
#'   The model is
#'   \deqn{y_{ij} ~ NB(p_i, r_i)}
#'   \deqn{p_i ~ Uniform(0, 1)}
#'   \deqn{r_{ij} = (\mu_{ij} * p_i) / (1 - p_i)}
#'   \deqn{log(\mu_{ij}) = x_{ij} * b1 + b2 * d_i ^ b4 / (b3 + d_i ^ b4)}
#'   \deqn{b1 ~ N(`mu_b1`, `sigma_b1`^2)}
#'   \deqn{b2 ~ N(`mu_b2`, `sigma_b2`^2)}
#'   \deqn{b3 ~ N(`mu_b3`, `sigma_b3`^2) (Truncated to be positive)}
#'   \deqn{b3 ~ N(`mu_b4`, `sigma_b4`^2) (Truncated to be positive)}
#'   The model is parameterized in terms of the mean of the
#'     negative binomial distribution and the usual probability parameter p.
#'     The prior on the mean is an EMAX
#'     model, and the prior on p at each dose is Uniform(0, 1).
#'     The model can adjust for baseline covariates, (\deqn{x_{ij}}).
#' @family models
#' @return A list with the model's prior weight and hyperparameter values.
#' @export
model_negbin_sigmoid_emax <- function(
  mu_b1,
  sigma_b1,
  mu_b2,
  sigma_b2,
  mu_b3,
  sigma_b3,
  mu_b4,
  sigma_b4,
  w_prior = 1
) {
  out <- list(
    mu_b1 = mu_b1,
    sigma_b1 = sigma_b1,
    mu_b2 = mu_b2,
    sigma_b2 = sigma_b2,
    mu_b3 = mu_b3,
    sigma_b3 = sigma_b3,
    mu_b4 = mu_b4,
    sigma_b4 = sigma_b4,
    w_prior = w_prior
  )
  class(out) <- c("beaver_negbin_sigmoid_emax", "beaver_model")
  return(out)
}

#' Negative Binomial Linear Dose Response
#' @description Model settings for a negative binomial distribution assuming
#'   an linear model on the mean.  This function is to be used within a call
#'   to `beaver_mcmc()`.
#' @param mu_b1,sigma_b1,mu_b2,sigma_b2 hyperparameters.  See
#'   the model description below for context.
#' @param w_prior the prior weight for the model.
#' @section Negative Binomial Linear:
#'   Let \eqn{y_{ij}} be the \eqn{j}th subject on dose \eqn{d_i}.
#'   The model is
#'   \deqn{y_{ij} ~ NB(p_i, r_i)}
#'   \deqn{p_i ~ Uniform(0, 1)}
#'   \deqn{r_{ij} = (\mu_{ij} * p_i) / (1 - p_i)}
#'   \deqn{log(\mu_{ij}) = x_{ij} * b1 + b2 * d_i}
#'   \deqn{b1 ~ N(`mu_b1`, `sigma_b1`^2)}
#'   \deqn{b2 ~ N(`mu_b2`, `sigma_b2`^2)}
#'   The model is parameterized in terms of the mean of the
#'     negative binomial distribution and the usual probability parameter p.
#'     The prior on the mean is a linear
#'     model, and the prior on p at each dose is Uniform(0, 1).
#'     The model can adjust for baseline covariates, (\deqn{x_{ij}}).
#' @family models
#' @return A list with the model's prior weight and hyperparameter values.
#' @export
model_negbin_linear <- function(
  mu_b1,
  sigma_b1,
  mu_b2,
  sigma_b2,
  w_prior = 1
) {
  out <- list(
    mu_b1 = mu_b1,
    sigma_b1 = sigma_b1,
    mu_b2 = mu_b2,
    sigma_b2 = sigma_b2,
    w_prior = w_prior
  )
  class(out) <- c("beaver_negbin_linear", "beaver_model")
  return(out)
}

#' Negative Binomial Log-Linear Dose Response
#' @description Model settings for a negative binomial distribution assuming
#'   a log-linear model on the mean.  This function is to be used within a call
#'   to `beaver_mcmc()`.
#' @param mu_b1,sigma_b1,mu_b2,sigma_b2 hyperparameters.  See
#'   the model description below for context.
#' @param w_prior the prior weight for the model.
#' @section Negative Binomial Log-Linear:
#'   Let \eqn{y_{ij}} be the \eqn{j}th subject on dose \eqn{d_i}.
#'   The model is
#'   \deqn{y_{ij} ~ NB(p_i, r_i)}
#'   \deqn{p_i ~ Uniform(0, 1)}
#'   \deqn{r_{ij} = (\mu_{ij} * p_i) / (1 - p_i)}
#'   \deqn{log(\mu_{ij}) = x_{ij} * b1 + b2 * log(1 + d_i)}
#'   \deqn{b1 ~ N(`mu_b1`, `sigma_b1`^2)}
#'   \deqn{b2 ~ N(`mu_b2`, `sigma_b2`^2)}
#'   The model is parameterized in terms of the mean of the
#'     negative binomial distribution and the usual probability parameter p.
#'     The prior on the mean is a log-linear
#'     model, and the prior on p at each dose is Uniform(0, 1).
#'     The model can adjust for baseline covariates, (\deqn{x_{ij}}).
#' @family models
#' @return A list with the model's prior weight and hyperparameter values.
#' @export
model_negbin_loglinear <- function(
  mu_b1,
  sigma_b1,
  mu_b2,
  sigma_b2,
  w_prior = 1
) {
  out <- list(
    mu_b1 = mu_b1,
    sigma_b1 = sigma_b1,
    mu_b2 = mu_b2,
    sigma_b2 = sigma_b2,
    w_prior = w_prior
  )
  class(out) <- c("beaver_negbin_loglinear", "beaver_model")
  return(out)
}

#' Negative Binomial Quadratic Dose Response
#' @description Model settings for a negative binomial distribution assuming
#'   an quadratic model on the mean.  This function is to be used within a call
#'   to `beaver_mcmc()`.
#' @param mu_b1,sigma_b1,mu_b2,sigma_b2,mu_b3,sigma_b3 hyperparameters.  See
#'   the model description below for context.
#' @param w_prior the prior weight for the model.
#' @section Negative Binomial Quadratic:
#'   Let \eqn{y_{ij}} be the \eqn{j}th subject on dose \eqn{d_i}.
#'   The model is
#'   \deqn{y_{ij} ~ NB(p_i, r_i)}
#'   \deqn{p_i ~ Uniform(0, 1)}
#'   \deqn{r_{ij} = (\mu_{ij} * p_i) / (1 - p_i)}
#'   \deqn{log(\mu_{ij}) = x_{ij} * b1 + b2 * d_i + b3 * d_i ^ 2}
#'   \deqn{b1 ~ N(`mu_b1`, `sigma_b1`^2)}
#'   \deqn{b2 ~ N(`mu_b2`, `sigma_b2`^2)}
#'   \deqn{b3 ~ N(`mu_b3`, `sigma_b3`^2)}
#'   The model is parameterized in terms of the mean of the
#'     negative binomial distribution and the usual probability parameter p.
#'     The prior on the mean is a quadratic
#'     model, and the prior on p at each dose is Uniform(0, 1).
#'     The model can adjust for baseline covariates, (\deqn{x_{ij}}).
#' @family models
#' @return A list with the model's prior weight and hyperparameter values.
#' @export
model_negbin_quad <- function(
  mu_b1,
  sigma_b1,
  mu_b2,
  sigma_b2,
  mu_b3,
  sigma_b3,
  w_prior = 1
) {
  out <- list(
    mu_b1 = mu_b1,
    sigma_b1 = sigma_b1,
    mu_b2 = mu_b2,
    sigma_b2 = sigma_b2,
    mu_b3 = mu_b3,
    sigma_b3 = sigma_b3,
    w_prior = w_prior
  )
  class(out) <- c("beaver_negbin_quad", "beaver_model")
  return(out)
}

#' Negative Binomial Log-Quadratic Dose Response
#' @description Model settings fora negative binomial distribution assuming
#'   a log-quadratic model on the mean.  This function is to be used within a
#'   call to `beaver_mcmc()`.
#' @param mu_b1,sigma_b1,mu_b2,sigma_b2,mu_b3,sigma_b3 hyperparameters.  See
#'   the model description below for context.
#' @param w_prior the prior weight for the model.
#' @section Negative Binomial Quadratic:
#'   Let \eqn{y_{ij}} be the \eqn{j}th subject on dose \eqn{d_i}.
#'   The model is
#'   \deqn{y_{ij} ~ NB(p_i, r_i)}
#'   \deqn{p_i ~ Uniform(0, 1)}
#'   \deqn{r_{ij} = (\mu_{ij} * p_i) / (1 - p_i)}
#'   \deqn{
#'     log(\mu_{ij}) = x_{ij} * b1 + b2 * log(1 + d_i) + b3 * log(1 + d_i) ^ 2
#'   }
#'   \deqn{b1 ~ N(`mu_b1`, `sigma_b1`^2)}
#'   \deqn{b2 ~ N(`mu_b2`, `sigma_b2`^2)}
#'   \deqn{b3 ~ N(`mu_b3`, `sigma_b3`^2)}
#'   The model is parameterized in terms of the mean of the
#'     negative binomial distribution and the usual probability parameter p.
#'     The prior on the mean is a quadratic
#'     model, and the prior on p at each dose is Uniform(0, 1).
#'     The model can adjust for baseline covariates, (\deqn{x_{ij}}).
#' @family models
#' @return A list with the model's prior weight and hyperparameter values.
#' @export
model_negbin_logquad <- function(
  mu_b1,
  sigma_b1,
  mu_b2,
  sigma_b2,
  mu_b3,
  sigma_b3,
  w_prior = 1
) {
  out <- list(
    mu_b1 = mu_b1,
    sigma_b1 = sigma_b1,
    mu_b2 = mu_b2,
    sigma_b2 = sigma_b2,
    mu_b3 = mu_b3,
    sigma_b3 = sigma_b3,
    w_prior = w_prior
  )
  class(out) <- c("beaver_negbin_logquad", "beaver_model")
  return(out)
}

#' Negative Binomial Exponential Dose Response
#' @description Model settings for a negative binomial distribution assuming
#'   an exponential model on the mean.  This function is to be used within a
#'   call to `beaver_mcmc()`.
#' @param mu_b1,sigma_b1,mu_b2,sigma_b2,mu_b3,sigma_b3 hyperparameters.  See
#'   the model description below for context.
#' @param w_prior the prior weight for the model.
#' @section Negative Binomial Exponential:
#'   Let \eqn{y_{ij}} be the \eqn{j}th subject on dose \eqn{d_i}.
#'   The model is
#'   \deqn{y_{ij} ~ NB(p_i, r_i)}
#'   \deqn{p_i ~ Uniform(0, 1)}
#'   \deqn{r_{ij} = (\mu_{ij} * p_i) / (1 - p_i)}
#'   \deqn{log(\mu_{ij}) = x_{ij} * b1 + b2 * (1 - exp(-b3 * d_i))}
#'   \deqn{b1 ~ N(`mu_b1`, `sigma_b1`^2)}
#'   \deqn{b2 ~ N(`mu_b2`, `sigma_b2`^2)}
#'   \deqn{b3 ~ N(`mu_b3`, `sigma_b3`^2) (Truncated to be positive)}
#'   The model is parameterized in terms of the mean of the
#'     negative binomial distribution and the usual probability parameter p.
#'     The prior on the mean is an exponential
#'     model, and the prior on p at each dose is Uniform(0, 1).
#'     The model can adjust for baseline covariates, (\deqn{x_{ij}}).
#' @family models
#' @return A list with the model's prior weight and hyperparameter values.
#' @export
model_negbin_exp <- function(
  mu_b1,
  sigma_b1,
  mu_b2,
  sigma_b2,
  mu_b3,
  sigma_b3,
  w_prior = 1
) {
  out <- list(
    mu_b1 = mu_b1,
    sigma_b1 = sigma_b1,
    mu_b2 = mu_b2,
    sigma_b2 = sigma_b2,
    mu_b3 = mu_b3,
    sigma_b3 = sigma_b3,
    w_prior = w_prior
  )
  class(out) <- c("beaver_negbin_exp", "beaver_model")
  return(out)
}

#' Negative Binomial Independent Dose Response
#' @description Model settings for a negative binomial
#'   distribution with an independent mean for each dose.
#'   This function is to be used within a call
#'   to `beaver_mcmc()`.
#' @param mu_b1,sigma_b1,mu_b2,sigma_b2 hyperparameters.  See
#'   the model description below for context.
#' @param w_prior the prior weight for the model.
#' @section Negative Binomial Independent:
#'   Let \eqn{y_{ij}} be the \eqn{j}th subject on the \eqn{k}th dose.
#'   The model is
#'   \deqn{y_{ij} ~ NB(p_i, r_i)}
#'   \deqn{p_i ~ Uniform(0, 1)}
#'   \deqn{r_{ij} = (\mu_{ij} * p_i) / (1 - p_i)}
#'   \deqn{log(\mu_{ij}) = x_{ij} * b1 + b2_k}
#'   \deqn{b1 ~ N(`mu_b1`, `sigma_b1`^2)}
#'   \deqn{b2_k ~ N(`mu_b2`, `sigma_b2`^2)}
#'   The model is parameterized in terms of the mean of the
#'     negative binomial distribution and the usual probability parameter p.
#'     The prior on the mean is an exponential
#'     model, and the prior on p at each dose is Uniform(0, 1).
#'     The model can adjust for baseline covariates, (\deqn{x_{ij}}).
#' @family models
#' @return A list with the model's prior weight and hyperparameter values.
#' @export
model_negbin_indep <- function(
  mu_b1,
  sigma_b1,
  mu_b2,
  sigma_b2,
  w_prior = 1
) {
  out <- list(
    mu_b1 = mu_b1,
    sigma_b1 = sigma_b1,
    mu_b2 = mu_b2,
    sigma_b2 = sigma_b2,
    w_prior = w_prior
  )
  class(out) <- c("beaver_negbin_indep", "beaver_model")
  return(out)
}
