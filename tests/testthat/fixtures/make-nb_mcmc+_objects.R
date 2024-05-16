
#General----

formula <- ~ 1
formula_cov <- ~ age

n_chains <- 2
n_adapt <- 1e3
n_burn <- 1e3
n_iter <- 2.5e3
thin <- 50
quiet <- TRUE


#Functions----

create_nb_indep_model <- function(priors_list) {
  checkmate::assertNamed(priors_list)
  checkmate::assertNames(
    names(priors_list),
    must.include = c("mu_b1", "sigma_b1", "mu_b2", "sigma_b1")
  )

  jags_model <- paste0("

    model {
      for (i in 1:n_obs) {
        y[i] ~ dnegbin(p[dose_index[i]], r[i])
        intercept[i] <- x[i, ] %*% b1
        # intercept[i] <- x[i] %*% b1
        r[i] <- mu[i] * p[dose_index[i]] / (1 - p[dose_index[i]])
        log(mu[i]) <- intercept[i] + b2[dose_index[i]]
      }
      for (i in 1:n_doses) {
        p[i] ~ dunif(0, 1)
      }
      for (i in 1:k) {
        b1[i] ~ dnorm(", priors_list$mu_b1, ", tau2_b1)
      }
      b2[1] <- 0
      for (i in 1:(n_doses-1)) {
        b2[i + 1] ~ dnorm(", priors_list$mu_b2, ", tau2_b2)
      }
      tau2_b1 <- 1 / ", priors_list$sigma_b1 ^ 2, "
      tau2_b2 <- 1 / ", priors_list$sigma_b2 ^ 2, "
      # sigma <- sqrt(sigma2)
    }
  ")

  return(jags_model)
}

create_nb_linear_model <- function(priors_list) {
  checkmate::assertNamed(priors_list)
  checkmate::assertNames(
    names(priors_list),
    must.include = c("mu_b1", "sigma_b1", "mu_b2", "sigma_b1")
  )

  jags_model <- paste0("

    model {
      for (i in 1:n_obs) {
        y[i] ~ dnegbin(p[dose_index[i]], r[i])
        intercept[i] <- x[i, ] %*% b1
        r[i] <- mu[i] * p[dose_index[i]] / (1 - p[dose_index[i]])
        log(mu[i]) <- intercept[i] + b2 * dose[dose_index[i]]
      }
      for (i in 1:n_doses) {
        p[i] ~ dunif(0, 1)
      }
      for (i in 1:k) {
        b1[i] ~ dnorm(", priors_list$mu_b1, ", tau2_b1)
      }
      b2 ~ dnorm(", priors_list$mu_b2, ", tau2_b2)
      tau2_b1 <- 1 / ", priors_list$sigma_b1 ^ 2, "
      tau2_b2 <- 1 / ", priors_list$sigma_b2 ^ 2, "
      # sigma <- sqrt(sigma2)
    }
  ")

  return(jags_model)
}

create_nb_quadratic_model <- function(priors_list) {
  checkmate::assertNamed(priors_list)
  checkmate::assertNames(
    names(priors_list),
    must.include = c(
      "mu_b1",
      "sigma_b1",
      "mu_b2",
      "sigma_b1",
      "mu_b3",
      "sigma_b3"
    )
  )

  # nolint start
  jags_model <- paste0("

    model {
      for (i in 1:n_obs) {
        y[i] ~ dnegbin(p[dose_index[i]], r[i])
        intercept[i] <- x[i, ] %*% b1
        r[i] <- mu[i] * p[dose_index[i]] / (1 - p[dose_index[i]])
        log(mu[i]) <- intercept[i] + b2 * dose[dose_index[i]] + b3 * dose[dose_index[i]] ^ 2
      }
      for (i in 1:n_doses) {
        p[i] ~ dunif(0, 1)
      }
      for (i in 1:k) {
        b1[i] ~ dnorm(", priors_list$mu_b1, ", tau2_b1)
      }
      b2 ~ dnorm(", priors_list$mu_b2, ", tau2_b2)
      b3 ~ dnorm(", priors_list$mu_b3, ", tau2_b3)
      tau2_b1 <- 1 / ", priors_list$sigma_b1 ^ 2, "
      tau2_b2 <- 1 / ", priors_list$sigma_b2 ^ 2, "
      tau2_b3 <- 1 / ", priors_list$sigma_b3 ^ 2, "
      # sigma <- sqrt(sigma2)
    }
  ")
  # nolint end

  return(jags_model)
}

create_nb_emax_model <- function(priors_list) {
  checkmate::assertNamed(priors_list)
  checkmate::assertNames(
    names(priors_list),
    must.include = c(
      "mu_b1",
      "sigma_b1",
      "mu_b2",
      "sigma_b1",
      "mu_b3",
      "sigma_b3"
    )
  )

  # nolint start
  jags_model <- paste0("

    model {
      for (i in 1:n_obs) {
        y[i] ~ dnegbin(p[dose_index[i]], r[i])
        intercept[i] <- x[i, ] %*% b1
        r[i] <- mu[i] * p[dose_index[i]] / (1 - p[dose_index[i]])
        log(mu[i]) <- intercept[i] + b2 * dose[dose_index[i]] / (b3 + dose[dose_index[i]])
      }
      for (i in 1:n_doses) {
        p[i] ~ dunif(0, 1)
      }
      for (i in 1:k) {
        b1[i] ~ dnorm(", priors_list$mu_b1, ", tau2_b1)
      }
      b2 ~ dnorm(", priors_list$mu_b2, ", tau2_b2)
      b3 ~ dnorm(", priors_list$mu_b3, ", tau2_b3) T(0, )
      tau2_b1 <- 1 / ", priors_list$sigma_b1 ^ 2, "
      tau2_b2 <- 1 / ", priors_list$sigma_b2 ^ 2, "
      tau2_b3 <- 1 / ", priors_list$sigma_b3 ^ 2, "
      # sigma <- sqrt(sigma2)
    }
  ")
  # nolint end

  return(jags_model)
}

create_nb_exp_model <- function(priors_list) {
  checkmate::assertNamed(priors_list)
  checkmate::assertNames(
    names(priors_list),
    must.include = c(
      "mu_b1",
      "sigma_b1",
      "mu_b2",
      "sigma_b1",
      "mu_b3",
      "sigma_b3"
    )
  )

  jags_model <- paste0("

    model {
      for (i in 1:n_obs) {
        y[i] ~ dnegbin(p[dose_index[i]], r[i])
        intercept[i] <- x[i, ] %*% b1
        r[i] <- mu[i] * p[dose_index[i]] / (1 - p[dose_index[i]])
        log(mu[i]) <- intercept[i] + b2 * (1 - exp(- b3 * dose[dose_index[i]]))
      }
      for (i in 1:n_doses) {
        p[i] ~ dunif(0, 1)
      }
      for (i in 1:k) {
        b1[i] ~ dnorm(", priors_list$mu_b1, ", tau2_b1)
      }
      b2 ~ dnorm(", priors_list$mu_b2, ", tau2_b2)
      b3 ~ dnorm(", priors_list$mu_b3, ", tau2_b3) T(0, )
      tau2_b1 <- 1 / ", priors_list$sigma_b1 ^ 2, "
      tau2_b2 <- 1 / ", priors_list$sigma_b2 ^ 2, "
      tau2_b3 <- 1 / ", priors_list$sigma_b3 ^ 2, "
      # sigma <- sqrt(sigma2)
    }
  ")

  return(jags_model)
}

create_nb_sigmoid_emax_model <- function(priors_list) {
  checkmate::assertNamed(priors_list)
  checkmate::assertNames(
    names(priors_list),
    must.include = c(
      "mu_b1",
      "sigma_b1",
      "mu_b2",
      "sigma_b1",
      "mu_b3",
      "sigma_b3",
      "mu_b4",
      "sigma_b4"
    )
  )

  # nolint start
  jags_model <- paste0("

    model {
      for (i in 1:n_obs) {
        y[i] ~ dnegbin(p[dose_index[i]], r[i])
        intercept[i] <- x[i, ] %*% b1
        r[i] <- mu[i] * p[dose_index[i]] / (1 - p[dose_index[i]])
        log(mu[i]) <- intercept[i] + b2 * dose[dose_index[i]] ^ b4 / (b3 ^ b4 + dose[dose_index[i]] ^ b4)
      }
      for (i in 1:n_doses) {
        p[i] ~ dunif(0, 1)
      }
      for (i in 1:k) {
        b1[i] ~ dnorm(", priors_list$mu_b1, ", tau2_b1)
      }
      b2 ~ dnorm(", priors_list$mu_b2, ", tau2_b2)
      b3 ~ dnorm(", priors_list$mu_b3, ", tau2_b3) T(0, )
      b4 ~ dnorm(", priors_list$mu_b4, ", tau2_b4) T(0, )
      tau2_b1 <- 1 / ", priors_list$sigma_b1 ^ 2, "
      tau2_b2 <- 1 / ", priors_list$sigma_b2 ^ 2, "
      tau2_b3 <- 1 / ", priors_list$sigma_b3 ^ 2, "
      tau2_b4 <- 1 / ", priors_list$sigma_b4 ^ 2, "
      # sigma <- sqrt(sigma2)
    }
  ")
  # nolint end

  return(jags_model)
}

create_nb_loglinear_model <- function(priors_list) {
  checkmate::assertNamed(priors_list)
  checkmate::assertNames(
    names(priors_list),
    must.include = c("mu_b1", "sigma_b1", "mu_b2", "sigma_b1")
  )

  jags_model <- paste0("

    model {
      for (i in 1:n_obs) {
        y[i] ~ dnegbin(p[dose_index[i]], r[i])
        intercept[i] <- x[i, ] %*% b1
        r[i] <- mu[i] * p[dose_index[i]] / (1 - p[dose_index[i]])
        log(mu[i]) <- intercept[i] + b2 * log(1 + dose[dose_index[i]])
      }
      for (i in 1:n_doses) {
        p[i] ~ dunif(0, 1)
      }
      for (i in 1:k) {
        b1[i] ~ dnorm(", priors_list$mu_b1, ", tau2_b1)
      }
      b2 ~ dnorm(", priors_list$mu_b2, ", tau2_b2)
      tau2_b1 <- 1 / ", priors_list$sigma_b1 ^ 2, "
      tau2_b2 <- 1 / ", priors_list$sigma_b2 ^ 2, "
      # sigma <- sqrt(sigma2)
    }
  ")

  return(jags_model)
}

create_nb_logquadratic_model <- function(priors_list) {
  checkmate::assertNamed(priors_list)
  checkmate::assertNames(
    names(priors_list),
    must.include = c(
      "mu_b1",
      "sigma_b1",
      "mu_b2",
      "sigma_b1",
      "mu_b3",
      "sigma_b3"
    )
  )

  # nolint start
  jags_model <- paste0("

    model {
      for (i in 1:n_obs) {
        y[i] ~ dnegbin(p[dose_index[i]], r[i])
        intercept[i] <- x[i, ] %*% b1
        r[i] <- mu[i] * p[dose_index[i]] / (1 - p[dose_index[i]])
        log(mu[i]) <- intercept[i] + b2 * log(1 + dose[dose_index[i]]) + b3 * log(1 + dose[dose_index[i]]) ^ 2
      }
      for (i in 1:n_doses) {
        p[i] ~ dunif(0, 1)
      }
      for (i in 1:k) {
        b1[i] ~ dnorm(", priors_list$mu_b1, ", tau2_b1)
      }
      b2 ~ dnorm(", priors_list$mu_b2, ", tau2_b2)
      b3 ~ dnorm(", priors_list$mu_b3, ", tau2_b3)
      tau2_b1 <- 1 / ", priors_list$sigma_b1 ^ 2, "
      tau2_b2 <- 1 / ", priors_list$sigma_b2 ^ 2, "
      tau2_b3 <- 1 / ", priors_list$sigma_b3 ^ 2, "
      # sigma <- sqrt(sigma2)
    }
  ")
  # nolint end

  return(jags_model)
}

fit_jags <- function(model,
                     jags_data,
                     parameters,
                     inits = NULL,
                     n_chains = 1,
                     n_adapt = 1000,
                     n_burn = 1000,
                     n_iter,
                     thin = 1,
                     quiet = TRUE) {
  if (is.null(inits)) {
    if (n_chains == 1) {
      inits1 <- list(
        .RNG.name = "base::Wichmann-Hill",
        .RNG.seed = 1
      )
    } else {
      inits1 <- list()
      for (i in 1:n_chains) {
        inits1[[i]] <- list(
          .RNG.name = "base::Wichmann-Hill",
          .RNG.seed = i
        )
      }
    }
  } else {
    inits1 <- inits
  }

  model_init <- rjags::jags.model(
    textConnection(eval(parse(text = "model"))),
    data = jags_data,
    inits = inits1,
    n.chains = n_chains,
    n.adapt = as.integer(n_adapt),
    quiet = quiet
  )

  stats::update(
    model_init,
    n.iter = as.integer(n_burn),
    progress.bar = "none"
  )

  fit <- rjags::coda.samples(
    model_init,
    variable.names = parameters,
    n.iter = as.integer(n_iter),
    thin = as.integer(thin),
    progress.bar = "none"
  )

  return(fit)
}


#Load data----

#>Monotone increasing----

nb_monotone_incr <- readRDS(test_path("fixtures", "nb_monotone_incr.rds"))

nb_monotone_incr_doses <- nb_monotone_incr %>%
  dplyr::distinct(dose) %>%
  dplyr::arrange(dose) %>%
  dplyr::pull(dose)

nb_monotone_incr <- nb_monotone_incr %>%
  dplyr::rowwise() %>%
  dplyr::mutate(dose_index =  which(dose == nb_monotone_incr_doses)) %>%
  dplyr::ungroup()

nb_monotone_incr_x <- model.matrix(formula, data = nb_monotone_incr)

nb_monotone_incr_jags <- list(
  n_obs = nrow(nb_monotone_incr),
  dose_index = nb_monotone_incr %>% dplyr::pull(dose_index),
  dose = nb_monotone_incr_doses,
  n_doses = length(nb_monotone_incr_doses),
  x = nb_monotone_incr_x,
  k = ncol(nb_monotone_incr_x),
  y = nb_monotone_incr %>% dplyr::pull(response)
)

#>Monotone increasing w/covariates----

nb_monotone_incr_cov <- readRDS(test_path("fixtures", "nb_monotone_incr_cov.rds")) # nolint

nb_monotone_incr_cov_doses <- nb_monotone_incr_cov %>%
  dplyr::distinct(dose) %>%
  dplyr::arrange(dose) %>%
  dplyr::pull(dose)

nb_monotone_incr_cov <- nb_monotone_incr_cov %>%
  dplyr::rowwise() %>%
  dplyr::mutate(dose_index =  which(dose == nb_monotone_incr_cov_doses)) %>%
  dplyr::ungroup()

nb_monotone_incr_cov_x <- model.matrix(
  formula_cov,
  data = nb_monotone_incr_cov
)

nb_monotone_incr_cov_jags <- list(
  n_obs = nrow(nb_monotone_incr_cov),
  dose_index = nb_monotone_incr_cov %>% dplyr::pull(dose_index),
  dose = nb_monotone_incr_cov_doses,
  n_doses = length(nb_monotone_incr_cov_doses),
  x = nb_monotone_incr_cov_x,
  k = ncol(nb_monotone_incr_cov_x),
  y = nb_monotone_incr_cov %>% dplyr::pull(response)
)


#Negative Binomial Independent Model----

#>Set-up model----

priors_nb_indep <- list(
  mu_b1 = 0,
  sigma_b1 = 10,
  mu_b2 = 0,
  sigma_b2 = 10
)

nb_indep_model <- create_nb_indep_model(priors_list = priors_nb_indep)
cat(nb_indep_model)

parameters_nb_indep <- c("p", "b1", "b2")

#>Run model----

nb_indep_model_samples <- fit_jags(
  model = nb_indep_model,
  jags_data = within(nb_monotone_incr_jags, rm(dose)),
  parameters = parameters_nb_indep,
  n_chains = n_chains,
  n_adapt = n_adapt,
  n_burn = n_burn,
  n_iter = n_iter,
  thin = thin,
  quiet = quiet
)

for (i in seq_len(length(nb_indep_model_samples))) {

  param_names <- attr(nb_indep_model_samples[[i]], "dimnames")[[2]]
  ind <- grep("^b1", param_names)
  param_names[ind] <- colnames(nb_monotone_incr_x)
  attr(nb_indep_model_samples[[i]], "dimnames")[[2]] <- param_names

}
rm(i, param_names, ind)

# nolint start
nb_indep_model_samples_updatedattr <- nb_indep_model_samples
attr(nb_indep_model_samples_updatedattr, "doses") <- nb_monotone_incr_doses
attr(nb_indep_model_samples_updatedattr, "n_b1") <- sum(grepl("^b1", colnames(nb_indep_model_samples_updatedattr[[1]])))
attr(nb_indep_model_samples_updatedattr, "covariate_names") <- colnames(nb_monotone_incr_x)
attr(nb_indep_model_samples_updatedattr, "formula") <- formula
class(nb_indep_model_samples_updatedattr) <- c(
  "beaver_mcmc_negbin_indep",
  "beaver_mcmc",
  class(nb_indep_model_samples_updatedattr)
)
# nolint end

#>Save----

save(
  priors_nb_indep,
  nb_indep_model_samples,
  nb_indep_model_samples_updatedattr,
  file = test_path("fixtures", "nb_indep_mcmc+_objects.Rdata")
)


#Negative Binomial Linear Model----

#>Set-up model----

priors_nb_linear <- list(
  mu_b1 = 0,
  sigma_b1 = 10,
  mu_b2 = 0,
  sigma_b2 = 10
)

nb_linear_model <- create_nb_linear_model(priors_list = priors_nb_linear)
cat(nb_linear_model)

parameters_nb_linear <- c("p", "b1", "b2")

#>Run model----

nb_linear_model_samples <- fit_jags(
  model = nb_linear_model,
  jags_data = nb_monotone_incr_jags,
  parameters = parameters_nb_linear,
  n_chains = n_chains,
  n_adapt = n_adapt,
  n_burn = n_burn,
  n_iter = n_iter,
  thin = thin,
  quiet = quiet
)

for (i in seq_len(length(nb_linear_model_samples))) {

  param_names <- attr(nb_linear_model_samples[[i]], "dimnames")[[2]]
  ind <- grep("^b1", param_names)
  param_names[ind] <- colnames(nb_monotone_incr_x)
  attr(nb_linear_model_samples[[i]], "dimnames")[[2]] <- param_names

}
rm(i, param_names, ind)

# nolint start
nb_linear_model_samples_updatedattr <- nb_linear_model_samples
attr(nb_linear_model_samples_updatedattr, "doses") <- nb_monotone_incr_doses
attr(nb_linear_model_samples_updatedattr, "n_b1") <- sum(grepl("^b1", colnames(nb_linear_model_samples_updatedattr[[1]])))
attr(nb_linear_model_samples_updatedattr, "covariate_names") <- colnames(nb_monotone_incr_x)
attr(nb_linear_model_samples_updatedattr, "formula") <- formula
class(nb_linear_model_samples_updatedattr) <- c(
  "beaver_mcmc_negbin_linear",
  "beaver_mcmc",
  class(nb_linear_model_samples_updatedattr)
)
# nolint end

#>Save----

save(
  priors_nb_linear,
  nb_linear_model_samples,
  nb_linear_model_samples_updatedattr,
  file = test_path("fixtures", "nb_linear_mcmc+_objects.Rdata")
)


#Negative Binomial Quadratic Model----

#>Set-up model----

priors_nb_quad <- list(
  mu_b1 = 0,
  sigma_b1 = 10,
  mu_b2 = 0,
  sigma_b2 = 10,
  mu_b3 = 0,
  sigma_b3 = 10
)

nb_quad_model <- create_nb_quadratic_model(priors_list = priors_nb_quad)
cat(nb_quad_model)

parameters_nb_quad <- c("p", "b1", "b2", "b3")

#>Run model----

nb_quad_model_samples <- fit_jags(
  model = nb_quad_model,
  jags_data = nb_monotone_incr_jags,
  parameters = parameters_nb_quad,
  n_chains = n_chains,
  n_adapt = n_adapt,
  n_burn = n_burn,
  n_iter = n_iter,
  thin = thin,
  quiet = quiet
)

for (i in seq_len(length(nb_quad_model_samples))) {

  param_names <- attr(nb_quad_model_samples[[i]], "dimnames")[[2]]
  ind <- grep("^b1", param_names)
  param_names[ind] <- colnames(nb_monotone_incr_x)
  attr(nb_quad_model_samples[[i]], "dimnames")[[2]] <- param_names

}
rm(i, param_names, ind)

# nolint start
nb_quad_model_samples_updatedattr <- nb_quad_model_samples
attr(nb_quad_model_samples_updatedattr, "doses") <- nb_monotone_incr_doses
attr(nb_quad_model_samples_updatedattr, "n_b1") <- sum(grepl("^b1", colnames(nb_quad_model_samples_updatedattr[[1]])))
attr(nb_quad_model_samples_updatedattr, "covariate_names") <- colnames(nb_monotone_incr_x)
attr(nb_quad_model_samples_updatedattr, "formula") <- formula
class(nb_quad_model_samples_updatedattr) <- c(
  "beaver_mcmc_negbin_quad",
  "beaver_mcmc",
  class(nb_quad_model_samples_updatedattr)
)
# nolint end

#>Save----

save(
  priors_nb_quad,
  nb_quad_model_samples,
  nb_quad_model_samples_updatedattr,
  file = test_path("fixtures", "nb_quad_mcmc+_objects.Rdata")
)


#Negative Binomial Emax Model----

#>Set-up model----

priors_nb_emax <- list(
  mu_b1 = 0,
  sigma_b1 = 10,
  mu_b2 = 0,
  sigma_b2 = 10,
  mu_b3 = 0.25,
  sigma_b3 = 0.05
)

nb_emax_model <- create_nb_emax_model(priors_list = priors_nb_emax)
cat(nb_emax_model)

parameters_nb_emax <- c("p", "b1", "b2", "b3")

#>Run model----

nb_emax_model_samples <- fit_jags(
  model = nb_emax_model,
  jags_data = nb_monotone_incr_jags,
  parameters = parameters_nb_emax,
  n_chains = n_chains,
  n_adapt = n_adapt,
  n_burn = n_burn,
  n_iter = n_iter,
  thin = thin,
  quiet = quiet
)

for (i in seq_len(length(nb_emax_model_samples))) {

  param_names <- attr(nb_emax_model_samples[[i]], "dimnames")[[2]]
  ind <- grep("^b1", param_names)
  param_names[ind] <- colnames(nb_monotone_incr_x)
  attr(nb_emax_model_samples[[i]], "dimnames")[[2]] <- param_names

}
rm(i, param_names, ind)

# nolint start
nb_emax_model_samples_updatedattr <- nb_emax_model_samples
attr(nb_emax_model_samples_updatedattr, "doses") <- nb_monotone_incr_doses
attr(nb_emax_model_samples_updatedattr, "n_b1") <- sum(grepl("^b1", colnames(nb_emax_model_samples_updatedattr[[1]])))
attr(nb_emax_model_samples_updatedattr, "covariate_names") <- colnames(nb_monotone_incr_x)
attr(nb_emax_model_samples_updatedattr, "formula") <- formula
class(nb_emax_model_samples_updatedattr) <- c(
  "beaver_mcmc_negbin_emax",
  "beaver_mcmc",
  class(nb_emax_model_samples_updatedattr)
)
# nolint end

#>Save----

save(
  priors_nb_emax,
  nb_emax_model_samples,
  nb_emax_model_samples_updatedattr,
  file = test_path("fixtures", "nb_emax_mcmc+_objects.Rdata")
)


#Negative Binomial Exp Model----

#>Set-up model----

priors_nb_exp <- list(
  mu_b1 = 0,
  sigma_b1 = 10,
  mu_b2 = 0,
  sigma_b2 = 10,
  mu_b3 = 0,
  sigma_b3 = 10
)

nb_exp_model <- create_nb_exp_model(priors_list = priors_nb_exp)
cat(nb_exp_model)

parameters_nb_exp <- c("p", "b1", "b2", "b3")

#>Run model----

nb_exp_model_samples <- fit_jags(
  model = nb_exp_model,
  jags_data = nb_monotone_incr_jags,
  parameters = parameters_nb_exp,
  n_chains = n_chains,
  n_adapt = n_adapt,
  n_burn = n_burn,
  n_iter = n_iter,
  thin = thin,
  quiet = quiet
)

for (i in seq_len(length(nb_exp_model_samples))) {

  param_names <- attr(nb_exp_model_samples[[i]], "dimnames")[[2]]
  ind <- grep("^b1", param_names)
  param_names[ind] <- colnames(nb_monotone_incr_x)
  attr(nb_exp_model_samples[[i]], "dimnames")[[2]] <- param_names

}
rm(i, param_names, ind)

# nolint start
nb_exp_model_samples_updatedattr <- nb_exp_model_samples
attr(nb_exp_model_samples_updatedattr, "doses") <- nb_monotone_incr_doses
attr(nb_exp_model_samples_updatedattr, "n_b1") <- sum(grepl("^b1", colnames(nb_exp_model_samples_updatedattr[[1]])))
attr(nb_exp_model_samples_updatedattr, "covariate_names") <- colnames(nb_monotone_incr_x)
attr(nb_exp_model_samples_updatedattr, "formula") <- formula
class(nb_exp_model_samples_updatedattr) <- c(
  "beaver_mcmc_negbin_exp",
  "beaver_mcmc",
  class(nb_exp_model_samples_updatedattr)
)
# nolint end

#>Save----

save(
  priors_nb_exp,
  nb_exp_model_samples,
  nb_exp_model_samples_updatedattr,
  file = test_path("fixtures", "nb_exp_mcmc+_objects.Rdata")
)


#Negative Binomial Sigmoid Emax Model----

#>Set-up model----

priors_nb_sigmoid_emax <- list(
  mu_b1 = 0,
  sigma_b1 = 10,
  mu_b2 = 0,
  sigma_b2 = 10,
  mu_b3 = 0.25,
  sigma_b3 = 0.05,
  mu_b4 = 1,
  sigma_b4 = 5
)

nb_sigmoid_emax_model <- create_nb_sigmoid_emax_model(
  priors_list = priors_nb_sigmoid_emax
)
cat(nb_sigmoid_emax_model)

parameters_nb_sigmoid_emax <- c("p", "b1", "b2", "b3", "b4")

#>Run model----

nb_sigmoid_emax_model_samples <- fit_jags(
  model = nb_sigmoid_emax_model,
  jags_data = nb_monotone_incr_jags,
  parameters = parameters_nb_sigmoid_emax,
  n_chains = n_chains,
  n_adapt = n_adapt,
  n_burn = n_burn,
  n_iter = n_iter,
  thin = thin,
  quiet = quiet
)

for (i in seq_len(length(nb_sigmoid_emax_model_samples))) {

  param_names <- attr(nb_sigmoid_emax_model_samples[[i]], "dimnames")[[2]]
  ind <- grep("^b1", param_names)
  param_names[ind] <- colnames(nb_monotone_incr_x)
  attr(nb_sigmoid_emax_model_samples[[i]], "dimnames")[[2]] <- param_names

}
rm(i, param_names, ind)

# nolint start
nb_sigmoid_emax_model_samples_updatedattr <- nb_sigmoid_emax_model_samples
attr(nb_sigmoid_emax_model_samples_updatedattr, "doses") <- nb_monotone_incr_doses
attr(nb_sigmoid_emax_model_samples_updatedattr, "n_b1") <- sum(grepl("^b1", colnames(nb_sigmoid_emax_model_samples_updatedattr[[1]])))
attr(nb_sigmoid_emax_model_samples_updatedattr, "covariate_names") <- colnames(nb_monotone_incr_x)
attr(nb_sigmoid_emax_model_samples_updatedattr, "formula") <- formula
class(nb_sigmoid_emax_model_samples_updatedattr) <- c(
  "beaver_mcmc_negbin_sigmoid_emax",
  "beaver_mcmc",
  class(nb_sigmoid_emax_model_samples_updatedattr)
)
# nolint end

#>Save----

save(
  priors_nb_sigmoid_emax,
  nb_sigmoid_emax_model_samples,
  nb_sigmoid_emax_model_samples_updatedattr,
  file = test_path("fixtures", "nb_sigmoid_emax_mcmc+_objects.Rdata")
)


#Negative Binomial Log Linear Model----

#>Set-up model----

priors_nb_loglinear <- list(
  mu_b1 = 0,
  sigma_b1 = 10,
  mu_b2 = 0,
  sigma_b2 = 10
)

nb_loglinear_model <- create_nb_loglinear_model(
  priors_list = priors_nb_loglinear
)
cat(nb_loglinear_model)

parameters_nb_loglinear <- c("p", "b1", "b2")

#>Run model----

nb_loglinear_model_samples <- fit_jags(
  model = nb_loglinear_model,
  jags_data = nb_monotone_incr_jags,
  parameters = parameters_nb_loglinear,
  n_chains = n_chains,
  n_adapt = n_adapt,
  n_burn = n_burn,
  n_iter = n_iter,
  thin = thin,
  quiet = quiet
)

for (i in seq_len(length(nb_loglinear_model_samples))) {

  param_names <- attr(nb_loglinear_model_samples[[i]], "dimnames")[[2]]
  ind <- grep("^b1", param_names)
  param_names[ind] <- colnames(nb_monotone_incr_x)
  attr(nb_loglinear_model_samples[[i]], "dimnames")[[2]] <- param_names

}
rm(i, param_names, ind)

# nolint start
nb_loglinear_model_samples_updatedattr <- nb_loglinear_model_samples
attr(nb_loglinear_model_samples_updatedattr, "doses") <- nb_monotone_incr_doses
attr(nb_loglinear_model_samples_updatedattr, "n_b1") <- sum(grepl("^b1", colnames(nb_loglinear_model_samples_updatedattr[[1]])))
attr(nb_loglinear_model_samples_updatedattr, "covariate_names") <- colnames(nb_monotone_incr_x)
attr(nb_loglinear_model_samples_updatedattr, "formula") <- formula
class(nb_loglinear_model_samples_updatedattr) <- c(
  "beaver_mcmc_negbin_loglinear",
  "beaver_mcmc",
  class(nb_loglinear_model_samples_updatedattr)
)
# nolint end

#>Save----

save(
  priors_nb_loglinear,
  nb_loglinear_model_samples,
  nb_loglinear_model_samples_updatedattr,
  file = test_path("fixtures", "nb_loglinear_mcmc+_objects.Rdata")
)


#Negative Binomial Log Quadratic Model----

#>Set-up model----

priors_nb_logquad <- list(
  mu_b1 = 0,
  sigma_b1 = 10,
  mu_b2 = 0,
  sigma_b2 = 10,
  mu_b3 = 0,
  sigma_b3 = 10
)

nb_logquad_model <- create_nb_logquadratic_model(
  priors_list = priors_nb_logquad
)
cat(nb_logquad_model)

parameters_nb_logquad <- c("p", "b1", "b2", "b3")

#>Run model----

nb_logquad_model_samples <- fit_jags(
  model = nb_logquad_model,
  jags_data = nb_monotone_incr_jags,
  parameters = parameters_nb_logquad,
  n_chains = n_chains,
  n_adapt = n_adapt,
  n_burn = n_burn,
  n_iter = n_iter,
  thin = thin,
  quiet = quiet
)

for (i in seq_len(length(nb_logquad_model_samples))) {

  param_names <- attr(nb_logquad_model_samples[[i]], "dimnames")[[2]]
  ind <- grep("^b1", param_names)
  param_names[ind] <- colnames(nb_monotone_incr_x)
  attr(nb_logquad_model_samples[[i]], "dimnames")[[2]] <- param_names

}
rm(i, param_names, ind)

# nolint start
nb_logquad_model_samples_updatedattr <- nb_logquad_model_samples
attr(nb_logquad_model_samples_updatedattr, "doses") <- nb_monotone_incr_doses
attr(nb_logquad_model_samples_updatedattr, "n_b1") <- sum(grepl("^b1", colnames(nb_logquad_model_samples_updatedattr[[1]])))
attr(nb_logquad_model_samples_updatedattr, "covariate_names") <- colnames(nb_monotone_incr_x)
attr(nb_logquad_model_samples_updatedattr, "formula") <- formula
class(nb_logquad_model_samples_updatedattr) <- c(
  "beaver_mcmc_negbin_logquad",
  "beaver_mcmc",
  class(nb_logquad_model_samples_updatedattr)
)
# nolint end

#>Save----

save(
  priors_nb_logquad,
  nb_logquad_model_samples,
  nb_logquad_model_samples_updatedattr,
  file = test_path("fixtures", "nb_logquad_mcmc+_objects.Rdata")
)


#Negative Binomial Emax Model w/covariates----

#>Set-up model----

priors_nb_emax <- list(
  mu_b1 = 0,
  sigma_b1 = 10,
  mu_b2 = 0,
  sigma_b2 = 10,
  mu_b3 = 0.25,
  sigma_b3 = 0.05
)

nb_emax_model <- create_nb_emax_model(priors_list = priors_nb_emax)
cat(nb_emax_model)

parameters_nb_emax <- c("p", "b1", "b2", "b3")

#>Run model----

nb_emax_model_cov_samples <- fit_jags(
  model = nb_emax_model,
  jags_data = nb_monotone_incr_cov_jags,
  parameters = parameters_nb_emax,
  n_chains = n_chains,
  n_adapt = n_adapt,
  n_burn = n_burn,
  n_iter = n_iter,
  thin = thin,
  quiet = quiet
)

for (i in seq_len(length(nb_emax_model_cov_samples))) {

  param_names <- attr(nb_emax_model_cov_samples[[i]], "dimnames")[[2]]
  ind <- grep("^b1", param_names)
  param_names[ind] <- colnames(nb_monotone_incr_cov_x)
  attr(nb_emax_model_cov_samples[[i]], "dimnames")[[2]] <- param_names

}
rm(i, param_names, ind)

# nolint start
nb_emax_model_cov_samples_updatedattr <- nb_emax_model_cov_samples
attr(nb_emax_model_cov_samples_updatedattr, "doses") <- nb_monotone_incr_cov_doses
attr(nb_emax_model_cov_samples_updatedattr, "n_b1") <- sum(grepl("^b1", colnames(nb_emax_model_cov_samples_updatedattr[[1]])))
attr(nb_emax_model_cov_samples_updatedattr, "covariate_names") <- colnames(nb_monotone_incr_cov_x)
attr(nb_emax_model_cov_samples_updatedattr, "formula") <- formula_cov
class(nb_emax_model_cov_samples_updatedattr) <- c(
  "beaver_mcmc_negbin_emax",
  "beaver_mcmc",
  class(nb_emax_model_cov_samples_updatedattr)
)
# nolint end

#>Save----

save(
  priors_nb_emax,
  nb_emax_model_cov_samples,
  nb_emax_model_cov_samples_updatedattr,
  file = test_path("fixtures", "nb_emax_cov_mcmc+_objects.Rdata")
)


#Negative Binomial Exp Model----

#>Set-up model----

priors_nb_exp <- list(
  mu_b1 = 0,
  sigma_b1 = 10,
  mu_b2 = 0,
  sigma_b2 = 10,
  mu_b3 = 0,
  sigma_b3 = 10
)

nb_exp_model <- create_nb_exp_model(priors_list = priors_nb_exp)
cat(nb_exp_model)

parameters_nb_exp <- c("p", "b1", "b2", "b3")

#>Run model----

nb_exp_model_cov_samples <- fit_jags(
  model = nb_exp_model,
  jags_data = nb_monotone_incr_cov_jags,
  parameters = parameters_nb_exp,
  n_chains = n_chains,
  n_adapt = n_adapt,
  n_burn = n_burn,
  n_iter = n_iter,
  thin = thin,
  quiet = quiet
)

for (i in seq_len(length(nb_exp_model_cov_samples))) {

  param_names <- attr(nb_exp_model_cov_samples[[i]], "dimnames")[[2]]
  ind <- grep("^b1", param_names)
  param_names[ind] <- colnames(nb_monotone_incr_cov_x)
  attr(nb_exp_model_cov_samples[[i]], "dimnames")[[2]] <- param_names

}
rm(i, param_names, ind)

# nolint start
nb_exp_model_cov_samples_updatedattr <- nb_exp_model_cov_samples
attr(nb_exp_model_cov_samples_updatedattr, "doses") <- nb_monotone_incr_cov_doses
attr(nb_exp_model_cov_samples_updatedattr, "n_b1") <- sum(grepl("^b1", colnames(nb_exp_model_cov_samples_updatedattr[[1]])))
attr(nb_exp_model_cov_samples_updatedattr, "covariate_names") <- colnames(nb_monotone_incr_cov_x)
attr(nb_exp_model_cov_samples_updatedattr, "formula") <- formula_cov
class(nb_exp_model_cov_samples_updatedattr) <- c(
  "beaver_mcmc_negbin_exp",
  "beaver_mcmc",
  class(nb_exp_model_cov_samples_updatedattr)
)
# nolint end

#>Save----

save(
  priors_nb_exp,
  nb_exp_model_cov_samples,
  nb_exp_model_cov_samples_updatedattr,
  file = test_path("fixtures", "nb_exp_cov_mcmc+_objects.Rdata")
)


#Cleanup----

rm(formula, formula_cov, n_chains, n_adapt, n_burn, n_iter, thin, quiet)
rm(
  create_nb_indep_model,
  create_nb_linear_model,
  create_nb_quadratic_model,
  create_nb_emax_model,
  create_nb_exp_model,
  create_nb_sigmoid_emax_model,
  create_nb_loglinear_model,
  create_nb_logquadratic_model,
  fit_jags
)
rm(list = ls(pattern = "nb_monotone_incr"))
rm(list = ls(pattern = "nb_indep"))
rm(list = ls(pattern = "nb_linear"))
rm(list = ls(pattern = "nb_quad"))
rm(list = ls(pattern = "nb_emax"))
rm(list = ls(pattern = "nb_exp"))
rm(list = ls(pattern = "nb_sigmoid_emax"))
rm(list = ls(pattern = "nb_loglinear"))
rm(list = ls(pattern = "nb_logquad"))
