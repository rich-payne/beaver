% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model.R
\name{model_negbin_emax}
\alias{model_negbin_emax}
\title{Negative Binomial EMAX Dose Response}
\usage{
model_negbin_emax(
  mu_b1,
  sigma_b1,
  mu_b2,
  sigma_b2,
  mu_b3,
  sigma_b3,
  w_prior = 1
)
}
\arguments{
\item{mu_b1, sigma_b1, mu_b2, sigma_b2, mu_b3, sigma_b3}{hyperparameters.  See
the model description below for context.}

\item{w_prior}{the prior weight for the model.}
}
\value{
A list with the model's prior weight and hyperparameter values.
}
\description{
Model settings for a negative binomial distribution assuming
an EMAX Model on the mean.  This function is to be used within a call
to \code{beaver_mcmc()}.
}
\section{Negative Binomial EMAX}{

Let \eqn{y_{ij}} be the \eqn{j}th subject on dose \eqn{d_i}.
The model is
\deqn{y_{ij} ~ NB(p_i, r_i)}
\deqn{p_i ~ Uniform(0, 1)}
\deqn{r_{ij} = (\mu_{ij} * p_i) / (1 - p_i)}
\deqn{log(\mu_{ij}) = x_{ij} * b1 + b2 * d_i / (b3 + d_i)}
\deqn{b1 ~ N(`mu_b1`, `sigma_b1`^2)}
\deqn{b2 ~ N(`mu_b2`, `sigma_b2`^2)}
\deqn{b3 ~ N(`mu_b3`, `sigma_b3`^2) (Truncated to be positive)}
The model is parameterized in terms of the mean of the
negative binomial distribution and the usual probability parameter p.
The prior on the mean is an EMAX
model, and the prior on p at each dose is Uniform(0, 1).
The model can adjust for baseline covariates, (\deqn{x_{ij}}).
}

\seealso{
Other models: 
\code{\link{beaver_mcmc}()},
\code{\link{model_negbin_exp}()},
\code{\link{model_negbin_indep}()},
\code{\link{model_negbin_linear}()},
\code{\link{model_negbin_loglinear}()},
\code{\link{model_negbin_logquad}()},
\code{\link{model_negbin_quad}()},
\code{\link{model_negbin_sigmoid_emax}()}
}
\concept{models}
