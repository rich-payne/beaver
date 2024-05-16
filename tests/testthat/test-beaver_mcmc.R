
test_that("beaver_mcmc works, produces an object with correct properties", {

  nb_monotone_incr <- readRDS(test_path("fixtures", "nb_monotone_incr.rds"))

  expect_failure(expect_s3_class(nb_monotone_incr, NA))
  expect_s3_class(nb_monotone_incr, "data.frame")

  nb_monotone_incr_cov <- readRDS(test_path("fixtures", "nb_monotone_incr_cov.rds")) # nolint

  expect_failure(expect_s3_class(nb_monotone_incr_cov, NA))
  expect_s3_class(nb_monotone_incr_cov, "data.frame")

  #no covariates----

  formula <- ~ 1
  model_class <- c("beaver_negbin_linear", "beaver_negbin_quad")

  beaver_mcmc <- beaver_mcmc(
    data = nb_monotone_incr,
    formula = formula,
    linear = model_negbin_linear(
      mu_b1 = 0,
      sigma_b1 = 10,
      mu_b2 = 0,
      sigma_b2 = 10,
      w_prior = 1 / 2
    ),
    quad = model_negbin_quad(
      mu_b1 = 0,
      sigma_b1 = 10,
      mu_b2 = 0,
      sigma_b2 = 10,
      mu_b3 = 0,
      sigma_b3 = 10,
      w_prior = 1 / 2
    ),
    n_adapt = 1e2,
    n_burn = 1e2,
    n_iter = 1e2L,
    n_chains = 2L,
    thin = 1,
    quiet = TRUE
  )

  beaver_mcmc_checks(
    beaver_mcmc = beaver_mcmc,
    model_class = model_class
  )

  #covariates----

  formula_cov <- ~ age
  model_class_cov <- c("beaver_negbin_emax", "beaver_negbin_exp")

  beaver_mcmc_cov <- beaver_mcmc(
    data = nb_monotone_incr_cov,
    formula = formula_cov,
    emax = model_negbin_emax(
      mu_b1 = 0,
      sigma_b1 = 10,
      mu_b2 = 0,
      sigma_b2 = 10,
      mu_b3 = 0.25,
      sigma_b3 = 0.05,
      w_prior = 1 / 2
    ),
    exp = model_negbin_exp(
      mu_b1 = 0,
      sigma_b1 = 10,
      mu_b2 = 0,
      sigma_b2 = 10,
      mu_b3 = 0,
      sigma_b3 = 10,
      w_prior = 1 / 2
    ),
    n_adapt = 1e2,
    n_burn = 1e2,
    n_iter = 1e2L,
    n_chains = 2L,
    thin = 1,
    quiet = TRUE
  )

  beaver_mcmc_checks(
    beaver_mcmc = beaver_mcmc_cov,
    model_class = model_class_cov
  )
})

test_that("run_mcmc works, produces an object with correct properties", {

  nb_monotone_incr <- readRDS(test_path("fixtures", "nb_monotone_incr.rds"))

  expect_failure(expect_s3_class(nb_monotone_incr, NA))
  expect_s3_class(nb_monotone_incr, "data.frame")

  nb_monotone_incr_cov <- readRDS(test_path("fixtures", "nb_monotone_incr_cov.rds")) # nolint

  expect_failure(expect_s3_class(nb_monotone_incr_cov, NA))
  expect_s3_class(nb_monotone_incr_cov, "data.frame")

  n_iter <- 1e2L
  n_chains <- 2L
  jags_modules <- rjags::list.modules()
  load_jags_modules()

  #no covariates----

  formula <- ~ 1
  nb_monotone_incr_x <- model.matrix(formula, data = nb_monotone_incr)
  model_class <- c("beaver_negbin_linear")

  mcmc <- run_mcmc(
    model = model_negbin_linear(
      mu_b1 = 0,
      sigma_b1 = 10,
      mu_b2 = 0,
      sigma_b2 = 10,
      w_prior = 1
    ),
    data = nb_monotone_incr,
    x = nb_monotone_incr_x,
    n_adapt = 1e2,
    n_burn = 1e2,
    n_iter = n_iter,
    n_chains = n_chains,
    thin = 1,
    quiet = TRUE,
    formula = formula,
    n_models = 1
  )

  run_mcmc_checks(
    mcmc = mcmc,
    model_class = model_class,
    n_iter = n_iter,
    n_chains = n_chains
  )

  #covariates----

  formula_cov <- ~ age
  nb_monotone_incr_cov_x <- model.matrix(
    formula_cov,
    data = nb_monotone_incr_cov
  )
  model_class_cov <- c("beaver_negbin_emax")

  mcmc_cov <- run_mcmc(
    model = model_negbin_emax(
      mu_b1 = 0,
      sigma_b1 = 10,
      mu_b2 = 0,
      sigma_b2 = 10,
      mu_b3 = 0.25,
      sigma_b3 = 0.05,
      w_prior = 1
    ),
    data = nb_monotone_incr_cov,
    x = nb_monotone_incr_cov_x,
    n_adapt = 1e2,
    n_burn = 1e2,
    n_iter = n_iter,
    n_chains = n_chains,
    thin = 1,
    quiet = TRUE,
    formula = formula_cov,
    n_models = 2
  )

  run_mcmc_checks(
    mcmc = mcmc_cov,
    model_class = model_class_cov,
    n_iter = n_iter,
    n_chains = n_chains
  )

  #cleanup----

  restore_jags_modules(jags_modules)
})

test_that("restore_jags_modules works", {

  jags_modules <- rjags::list.modules()
  load_jags_modules()
  expect_failure(expect_identical(rjags::list.modules(), jags_modules))
  restore_jags_modules(original_modules = jags_modules)
  expect_success(expect_identical(rjags::list.modules(), jags_modules))
})

test_that("load_jags_modules works", {

  suppressWarnings(rjags::unload.module("glm", quiet = TRUE))
  expect_false("glm" %in% rjags::list.modules())

  load_jags_modules()

  expect_true("glm" %in% rjags::list.modules())

  suppressWarnings(rjags::unload.module("glm", quiet = TRUE))
})

test_that("set_jags_seed returns the appropriate value", {

  #n == 1----

  expect_no_error(set_jags_seed(n_chains = 1))
  seed1 <- set_jags_seed(n_chains = 1)

  set_jags_seed_checks(seed = seed1, n_chains = 1L)

  #n == 2----

  expect_no_error(set_jags_seed(n_chains = 2))
  seed2 <- set_jags_seed(n_chains = 2)

  set_jags_seed_checks(seed = seed2, n_chains = 2L)
})

test_that("set_jags_seed throws an error when warranted", {

  expect_error(set_jags_seed(n_chains = NULL))
})

test_that("get_prog_bar returns the appropriate value", {

  expect_no_error(get_prog_bar(quiet = TRUE))
  expect_identical(get_prog_bar(quiet = TRUE), "none")
  expect_no_error(get_prog_bar(quiet = FALSE))
  expect_identical(get_prog_bar(quiet = FALSE), "text")
})

test_that("get_prog_bar throws an error when warranted", {

  expect_error(get_prog_bar(quiet = "yes"))
})

test_that("rename_b1 works against an S3 object of class mcmc.list, produces an object with correct properties", { # nolint

  nb_monotone_incr <- readRDS(test_path("fixtures", "nb_monotone_incr.rds"))

  expect_failure(expect_s3_class(nb_monotone_incr, NA))
  expect_s3_class(nb_monotone_incr, "data.frame")

  nb_monotone_incr_cov <- readRDS(test_path("fixtures", "nb_monotone_incr_cov.rds")) # nolint

  expect_failure(expect_s3_class(nb_monotone_incr_cov, NA))
  expect_s3_class(nb_monotone_incr_cov, "data.frame")

  n_iter <- 1e2L
  n_chains <- 2L
  jags_modules <- rjags::list.modules()
  load_jags_modules()

  #no covariates----

  formula <- ~ 1
  nb_monotone_incr_x <- model.matrix(formula, data = nb_monotone_incr)

  jags_data <- get_jags_data(
    model = model_negbin_linear(
      mu_b1 = 0,
      sigma_b1 = 10,
      mu_b2 = 0,
      sigma_b2 = 10,
      w_prior = 1
    ),
    data = nb_monotone_incr,
    x = nb_monotone_incr_x
  )
  mod <- rjags::jags.model(
    file = get_jags_model(
      model = model_negbin_linear(
        mu_b1 = 0,
        sigma_b1 = 10,
        mu_b2 = 0,
        sigma_b2 = 10,
        w_prior = 1
      )
    ),
    data = jags_data,
    inits = set_jags_seed(n_chains = n_chains),
    n.chains = n_chains,
    n.adapt = 1e2,
    quiet = TRUE
  )
  stats::update(mod, n.iter = 1e2, progress.bar = get_prog_bar(quiet = TRUE))
  samples <- rjags::coda.samples(
    mod,
    variable.names = get_var_names(
      model = model_negbin_linear(
        mu_b1 = 0,
        sigma_b1 = 10,
        mu_b2 = 0,
        sigma_b2 = 10,
        w_prior = 1
      )
    ),
    n.iter = n_iter,
    thin = 1,
    progress.bar = get_prog_bar(quiet = TRUE)
  )

  expect_failure(expect_s3_class(samples, NA))
  expect_s3_class(samples, "mcmc.list")

  expect_no_error(
    rename_b1(
      samples = samples,
      b1_names = colnames(nb_monotone_incr_x)
    )
  )

  samples_renamed <- rename_b1(
    samples = samples,
    b1_names = colnames(nb_monotone_incr_x)
  )

  rename_b1_checks(
    samples = samples,
    samples_renamed = samples_renamed,
    b1_names = colnames(nb_monotone_incr_x)
  )

  #>b1_names == NULL----

  expect_no_error(
    rename_b1(
      samples = samples,
      b1_names = NULL
    )
  )

  samples_renamed_null <- rename_b1(
    samples = samples,
    b1_names = NULL
  )

  expect_identical(samples_renamed_null, samples)

  #covariates----

  formula_cov <- ~ age
  nb_monotone_incr_cov_x <- model.matrix(
    formula_cov,
    data = nb_monotone_incr_cov
  )

  jags_data_cov <- get_jags_data(
    model = model_negbin_emax(
      mu_b1 = 0,
      sigma_b1 = 10,
      mu_b2 = 0,
      sigma_b2 = 10,
      mu_b3 = 0.25,
      sigma_b3 = 0.05,
      w_prior = 1
    ),
    data = nb_monotone_incr_cov,
    x = nb_monotone_incr_cov_x
  )
  mod_cov <- rjags::jags.model(
    file = get_jags_model(
      model = model_negbin_emax(
        mu_b1 = 0,
        sigma_b1 = 10,
        mu_b2 = 0,
        sigma_b2 = 10,
        mu_b3 = 0.25,
        sigma_b3 = 0.05,
        w_prior = 1
      )
    ),
    data = jags_data_cov,
    inits = set_jags_seed(n_chains = n_chains),
    n.chains = n_chains,
    n.adapt = 1e2,
    quiet = TRUE
  )
  stats::update(
    mod_cov,
    n.iter = 1e2,
    progress.bar = get_prog_bar(quiet = TRUE)
  )
  samples_cov <- rjags::coda.samples(
    mod_cov,
    variable.names = get_var_names(
      model = model_negbin_emax(
        mu_b1 = 0,
        sigma_b1 = 10,
        mu_b2 = 0,
        sigma_b2 = 10,
        mu_b3 = 0.25,
        sigma_b3 = 0.05,
        w_prior = 1
      )
    ),
    n.iter = n_iter,
    thin = 1,
    progress.bar = get_prog_bar(quiet = TRUE)
  )

  expect_failure(expect_s3_class(samples_cov, NA))
  expect_s3_class(samples_cov, "mcmc.list")

  expect_no_error(
    rename_b1(
      samples = samples_cov,
      b1_names = colnames(nb_monotone_incr_cov_x)
    )
  )

  samples_cov_renamed <- rename_b1(
    samples = samples_cov,
    b1_names = colnames(nb_monotone_incr_cov_x)
  )

  rename_b1_checks(
    samples = samples_cov,
    samples_renamed = samples_cov_renamed,
    b1_names = colnames(nb_monotone_incr_cov_x)
  )

  #cleanup----

  restore_jags_modules(jags_modules)
})

test_that("get_bma works, produces an object with correct properties", {

  nb_monotone_incr <- readRDS(test_path("fixtures", "nb_monotone_incr.rds"))

  expect_failure(expect_s3_class(nb_monotone_incr, NA))
  expect_s3_class(nb_monotone_incr, "data.frame")

  nb_monotone_incr_cov <- readRDS(test_path("fixtures", "nb_monotone_incr_cov.rds")) # nolint

  expect_failure(expect_s3_class(nb_monotone_incr_cov, NA))
  expect_s3_class(nb_monotone_incr_cov, "data.frame")

  jags_modules <- rjags::list.modules()
  load_jags_modules()

  #no covariates----

  formula <- ~ 1
  nb_monotone_incr_x <- model.matrix(formula, data = nb_monotone_incr)

  models <- list(
    linear = model_negbin_linear(
      mu_b1 = 0,
      sigma_b1 = 10,
      mu_b2 = 0,
      sigma_b2 = 10,
      w_prior = 1 / 2
    ),
    quad = model_negbin_quad(
      mu_b1 = 0,
      sigma_b1 = 10,
      mu_b2 = 0,
      sigma_b2 = 10,
      mu_b3 = 0,
      sigma_b3 = 10,
      w_prior = 1 / 2
    )
  )

  mcmc <- purrr::map(
    models,
    run_mcmc,
    data = nb_monotone_incr,
    x = nb_monotone_incr_x,
    n_adapt = 1e2,
    n_burn = 1e2,
    n_iter = 1e2L,
    n_chains = 2L,
    thin = 1,
    quiet = TRUE,
    formula = formula,
    n_models = 1
  )

  expect_s3_class(mcmc, NA)
  expect_true(is.list(mcmc))

  expect_no_error(
    get_bma(
      mcmc = mcmc,
      models = models
    )
  )

  bma_fit <- get_bma(
    mcmc = mcmc,
    models = models
  )

  bma_fit_checks(
    bma_fit = bma_fit,
    models = models
  )

  #covariates----

  formula_cov <- ~ age
  nb_monotone_incr_cov_x <- model.matrix(
    formula_cov,
    data = nb_monotone_incr_cov
  )

  models_cov <- list(
    emax = model_negbin_emax(
      mu_b1 = 0,
      sigma_b1 = 10,
      mu_b2 = 0,
      sigma_b2 = 10,
      mu_b3 = 0.25,
      sigma_b3 = 0.05,
      w_prior = 1 / 2
    ),
    exp = model_negbin_exp(
      mu_b1 = 0,
      sigma_b1 = 10,
      mu_b2 = 0,
      sigma_b2 = 10,
      mu_b3 = 0,
      sigma_b3 = 10,
      w_prior = 1 / 2
    )
  )

  mcmc_cov <- purrr::map(
    models_cov,
    run_mcmc,
    data = nb_monotone_incr_cov,
    x = nb_monotone_incr_cov_x,
    n_adapt = 1e2,
    n_burn = 1e2,
    n_iter = 1e2L,
    n_chains = 2L,
    thin = 1,
    quiet = TRUE,
    formula = formula_cov,
    n_models = 1
  )

  expect_s3_class(mcmc_cov, NA)
  expect_true(is.list(mcmc_cov))

  expect_no_error(
    get_bma(
      mcmc = mcmc_cov,
      models = models_cov
    )
  )

  bma_fit_cov <- get_bma(
    mcmc = mcmc_cov,
    models = models_cov
  )

  bma_fit_checks(
    bma_fit = bma_fit_cov,
    models = models_cov
  )

  #cleanup----

  restore_jags_modules(jags_modules)
})

test_that("get_bma_arg works, produces an object with correct properties", {

  nb_monotone_incr <- readRDS(test_path("fixtures", "nb_monotone_incr.rds"))

  expect_failure(expect_s3_class(nb_monotone_incr, NA))
  expect_s3_class(nb_monotone_incr, "data.frame")

  nb_monotone_incr_cov <- readRDS(test_path("fixtures", "nb_monotone_incr_cov.rds")) # nolint

  expect_failure(expect_s3_class(nb_monotone_incr_cov, NA))
  expect_s3_class(nb_monotone_incr_cov, "data.frame")

  jags_modules <- rjags::list.modules()
  load_jags_modules()

  #no covariates----

  formula <- ~ 1
  nb_monotone_incr_x <- model.matrix(formula, data = nb_monotone_incr)

  models <- list(
    linear = model_negbin_linear(
      mu_b1 = 0,
      sigma_b1 = 10,
      mu_b2 = 0,
      sigma_b2 = 10,
      w_prior = 1 / 2
    ),
    quad = model_negbin_quad(
      mu_b1 = 0,
      sigma_b1 = 10,
      mu_b2 = 0,
      sigma_b2 = 10,
      mu_b3 = 0,
      sigma_b3 = 10,
      w_prior = 1 / 2
    )
  )

  mcmc <- purrr::map(
    models,
    run_mcmc,
    data = nb_monotone_incr,
    x = nb_monotone_incr_x,
    n_adapt = 1e2,
    n_burn = 1e2,
    n_iter = 1e2L,
    n_chains = 2L,
    thin = 1,
    quiet = TRUE,
    formula = formula,
    n_models = 1
  )

  for (i in names(mcmc)) {

    expect_s3_class(mcmc[[i]], NA)
    expect_true(is.list(mcmc[[i]]))

    expect_no_error(
      get_bma_arg(
        mcmc = mcmc[[i]],
        model = models[[i]]
      )
    )

    bma_arg <- get_bma_arg(
      mcmc = mcmc[[i]],
      model = models[[i]]
    )

    bma_arg_checks(
      bma_arg = bma_arg,
      mcmc = mcmc[[i]]
    )

  }

  #covariates----

  formula_cov <- ~ age
  nb_monotone_incr_cov_x <- model.matrix(
    formula_cov,
    data = nb_monotone_incr_cov
  )

  models_cov <- list(
    emax = model_negbin_emax(
      mu_b1 = 0,
      sigma_b1 = 10,
      mu_b2 = 0,
      sigma_b2 = 10,
      mu_b3 = 0.25,
      sigma_b3 = 0.05,
      w_prior = 1 / 2
    ),
    exp = model_negbin_exp(
      mu_b1 = 0,
      sigma_b1 = 10,
      mu_b2 = 0,
      sigma_b2 = 10,
      mu_b3 = 0,
      sigma_b3 = 10,
      w_prior = 1 / 2
    )
  )

  mcmc_cov <- purrr::map(
    models_cov,
    run_mcmc,
    data = nb_monotone_incr_cov,
    x = nb_monotone_incr_cov_x,
    n_adapt = 1e2,
    n_burn = 1e2,
    n_iter = 1e2L,
    n_chains = 2L,
    thin = 1,
    quiet = TRUE,
    formula = formula_cov,
    n_models = 1
  )

  for (j in names(mcmc_cov)) {

    expect_s3_class(mcmc_cov[[j]], NA)
    expect_true(is.list(mcmc_cov[[j]]))

    expect_no_error(
      get_bma_arg(
        mcmc = mcmc_cov[[j]],
        model = models_cov[[j]]
      )
    )

    bma_cov_arg <- get_bma_arg(
      mcmc = mcmc_cov[[j]],
      model = models_cov[[j]]
    )

    bma_arg_checks(
      bma_arg = bma_cov_arg,
      mcmc = mcmc_cov[[j]]
    )

  }

  #cleanup----

  restore_jags_modules(jags_modules)
})
