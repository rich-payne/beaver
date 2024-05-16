
test_that("get_mean.beaver_mcmc_negbin_indep works against an S3 object of class beaver_mcmc_negbin_indep, produces an object with correct properties", { # nolint

  nb_monotone_incr <- readRDS(test_path("fixtures", "nb_monotone_incr.rds"))

  expect_failure(expect_named(nb_monotone_incr, NULL))
  expect_no_error(
    checkmate::assertSubset(
      c("dose", "response"),
      names(nb_monotone_incr)
    )
  )

  nb_monotone_incr_doses <- nb_monotone_incr %>%
    dplyr::distinct(dose) %>%
    dplyr::arrange(dose) %>%
    dplyr::pull(dose)

  load(test_path("fixtures", "nb_indep_mcmc+_objects.Rdata"))

  expect_failure(
    expect_s3_class(nb_indep_model_samples_updatedattr, NA)
  )
  expect_s3_class(
    nb_indep_model_samples_updatedattr,
    "beaver_mcmc_negbin_indep"
  )
  expect_no_error(
    checkmate::assertSubset(
      c("covariate_names", "formula"),
      names(attributes(nb_indep_model_samples_updatedattr))
    )
  )

  samples_process(
    samples = nb_indep_model_samples_updatedattr,
    data = nb_monotone_incr
  )

  expect_no_error(
    get_mean.beaver_mcmc_negbin_indep(
      samples = nb_indep_model_samples_updatedattr,
      doses = rep(nb_monotone_incr$dose, each = n_mcmc),
      intercept = intercept,
      u_doses = nb_monotone_incr_doses
    )
  )

  means <- get_mean.beaver_mcmc_negbin_indep(
    samples = nb_indep_model_samples_updatedattr,
    doses = rep(nb_monotone_incr$dose, each = n_mcmc),
    intercept = intercept,
    u_doses = nb_monotone_incr_doses
  )

  mean_checks(
    means = means,
    samples = nb_indep_model_samples_updatedattr,
    data = nb_monotone_incr
  )

  expect_identical(
    get_mean.beaver_mcmc_negbin_indep(
      samples = nb_indep_model_samples_updatedattr,
      doses = rep(nb_monotone_incr$dose, each = n_mcmc),
      intercept = intercept,
      samps = samps,
      u_doses = nb_monotone_incr_doses
    ),
    means
  )
})

test_that("get_mean works identically to get_mean.beaver_mcmc_negbin_indep", {

  nb_monotone_incr <- readRDS(test_path("fixtures", "nb_monotone_incr.rds"))

  nb_monotone_incr_doses <- nb_monotone_incr %>%
    dplyr::distinct(dose) %>%
    dplyr::arrange(dose) %>%
    dplyr::pull(dose)

  load(test_path("fixtures", "nb_indep_mcmc+_objects.Rdata"))

  samples_process(
    samples = nb_indep_model_samples_updatedattr,
    data = nb_monotone_incr
  )

  expect_no_error(
    get_mean(
      samples = nb_indep_model_samples_updatedattr,
      doses = rep(nb_monotone_incr$dose, each = n_mcmc),
      intercept = intercept,
      u_doses = nb_monotone_incr_doses
    )
  )
  expect_equal(
    get_mean(
      samples = nb_indep_model_samples_updatedattr,
      doses = rep(nb_monotone_incr$dose, each = n_mcmc),
      intercept = intercept,
      u_doses = nb_monotone_incr_doses
    ),
    get_mean.beaver_mcmc_negbin_indep(
      samples = nb_indep_model_samples_updatedattr,
      doses = rep(nb_monotone_incr$dose, each = n_mcmc),
      intercept = intercept,
      u_doses = nb_monotone_incr_doses
    )
  )
})

test_that("mean_negbin_linear produces an object with correct properties (small scale)", { # nolint

  dose_vals <- 1:3
  n_samps <- 1L

  set.seed(1234)
  b1 <- runif(n_samps, 2.9, 3.1)
  b2 <- runif(n_samps, 1.4, 1.6)

  expect_no_error(
    mean_negbin_linear(
      x = rep(dose_vals, each = n_samps),
      b1 = matrix(
        rep(b1, each = length(dose_vals)),
        nrow = length(b1),
        byrow = TRUE
      ),
      b2 = b2
    )
  )

  means <-
    mean_negbin_linear(
      x = rep(dose_vals, each = n_samps),
      b1 = matrix(
        rep(b1, each = length(dose_vals)),
        nrow = length(b1),
        byrow = TRUE
      ),
      b2 = b2
    )

  expect_true(is.matrix(means))
  expect_identical(nrow(means), n_samps)
  expect_identical(ncol(means), length(dose_vals))
  expect_identical(as.numeric(means), exp(b1 + b2 * dose_vals))
})

test_that("get_mean.beaver_mcmc_negbin_linear works against an S3 object of class beaver_mcmc_negbin_linear, produces an object with correct properties", { # nolint

  nb_monotone_incr <- readRDS(test_path("fixtures", "nb_monotone_incr.rds"))

  expect_failure(expect_named(nb_monotone_incr, NULL))
  expect_no_error(
    checkmate::assertSubset(
      c("dose", "response"),
      names(nb_monotone_incr)
    )
  )

  load(test_path("fixtures", "nb_linear_mcmc+_objects.Rdata"))

  expect_failure(expect_s3_class(nb_linear_model_samples_updatedattr, NA))
  expect_s3_class(
    nb_linear_model_samples_updatedattr,
    "beaver_mcmc_negbin_linear"
  )
  expect_no_error(
    checkmate::assertSubset(
      c("covariate_names", "formula"),
      names(attributes(nb_linear_model_samples_updatedattr))
    )
  )

  samples_process(
    samples = nb_linear_model_samples_updatedattr,
    data = nb_monotone_incr
  )

  expect_no_error(
    get_mean.beaver_mcmc_negbin_linear(
      samples = nb_linear_model_samples_updatedattr,
      doses = rep(nb_monotone_incr$dose, each = n_mcmc),
      intercept = intercept
    )
  )

  means <- get_mean.beaver_mcmc_negbin_linear(
    samples = nb_linear_model_samples_updatedattr,
    doses = rep(nb_monotone_incr$dose, each = n_mcmc),
    intercept = intercept
  )

  mean_checks(
    means = means,
    samples = nb_linear_model_samples_updatedattr,
    data = nb_monotone_incr
  )

  expect_identical(
    get_mean.beaver_mcmc_negbin_linear(
      samples = nb_linear_model_samples_updatedattr,
      doses = rep(nb_monotone_incr$dose, each = n_mcmc),
      intercept = intercept,
      samps = samps
    ),
    means
  )
})

test_that("get_mean works identically to get_mean.beaver_mcmc_negbin_linear", {

  nb_monotone_incr <- readRDS(test_path("fixtures", "nb_monotone_incr.rds"))
  load(test_path("fixtures", "nb_linear_mcmc+_objects.Rdata"))

  samples_process(
    samples = nb_linear_model_samples_updatedattr,
    data = nb_monotone_incr
  )

  expect_no_error(
    get_mean(
      samples = nb_linear_model_samples_updatedattr,
      doses = rep(nb_monotone_incr$dose, each = n_mcmc),
      intercept = intercept
    )
  )
  expect_equal(
    get_mean(
      samples = nb_linear_model_samples_updatedattr,
      doses = rep(nb_monotone_incr$dose, each = n_mcmc),
      intercept = intercept
    ),
    get_mean.beaver_mcmc_negbin_linear(
      samples = nb_linear_model_samples_updatedattr,
      doses = rep(nb_monotone_incr$dose, each = n_mcmc),
      intercept = intercept
    )
  )
})

test_that("mean_negbin_quad produces an object with correct properties (small scale)", { # nolint

  dose_vals <- 1:3
  n_samps <- 1L

  set.seed(1234)
  b1 <- runif(n_samps, 2.9, 3.1)
  b2 <- runif(n_samps, 1.4, 1.6)
  b3 <- runif(n_samps, -3.1, -2.9) # nolint

  expect_no_error(
    mean_negbin_quad(
      x = rep(dose_vals, each = n_samps),
      b1 = matrix(
        rep(b1, each = length(dose_vals)),
        nrow = length(b1),
        byrow = TRUE
      ),
      b2 = b2,
      b3 = b3
    )
  )

  means <-
    mean_negbin_quad(
      x = rep(dose_vals, each = n_samps),
      b1 = matrix(
        rep(b1, each = length(dose_vals)),
        nrow = length(b1),
        byrow = TRUE
      ),
      b2 = b2,
      b3 = b3
    )

  expect_true(is.matrix(means))
  expect_identical(nrow(means), n_samps)
  expect_identical(ncol(means), length(dose_vals))
  expect_identical(
    as.numeric(means),
    exp(b1 + b2 * dose_vals + b3 * dose_vals ^ 2)
  )
})

test_that("get_mean.beaver_mcmc_negbin_quad works against an S3 object of class beaver_mcmc_negbin_quad, produces an object with correct properties", { # nolint

  nb_monotone_incr <- readRDS(test_path("fixtures", "nb_monotone_incr.rds"))

  expect_failure(expect_named(nb_monotone_incr, NULL))
  expect_no_error(
    checkmate::assertSubset(
      c("dose", "response"),
      names(nb_monotone_incr)
    )
  )

  load(test_path("fixtures", "nb_quad_mcmc+_objects.Rdata"))

  expect_failure(expect_s3_class(nb_quad_model_samples_updatedattr, NA))
  expect_s3_class(nb_quad_model_samples_updatedattr, "beaver_mcmc_negbin_quad")
  expect_no_error(
    checkmate::assertSubset(
      c("covariate_names", "formula"),
      names(attributes(nb_quad_model_samples_updatedattr))
    )
  )

  samples_process(
    samples = nb_quad_model_samples_updatedattr,
    data = nb_monotone_incr
  )

  expect_no_error(
    get_mean.beaver_mcmc_negbin_quad(
      samples = nb_quad_model_samples_updatedattr,
      doses = rep(nb_monotone_incr$dose, each = n_mcmc),
      intercept = intercept
    )
  )

  means <- get_mean.beaver_mcmc_negbin_quad(
    samples = nb_quad_model_samples_updatedattr,
    doses = rep(nb_monotone_incr$dose, each = n_mcmc),
    intercept = intercept
  )

  mean_checks(
    means = means,
    samples = nb_quad_model_samples_updatedattr,
    data = nb_monotone_incr
  )

  expect_identical(
    get_mean.beaver_mcmc_negbin_quad(
      samples = nb_quad_model_samples_updatedattr,
      doses = rep(nb_monotone_incr$dose, each = n_mcmc),
      intercept = intercept,
      samps = samps
    ),
    means
  )
})

test_that("get_mean works identically to get_mean.beaver_mcmc_negbin_quad", {

  nb_monotone_incr <- readRDS(test_path("fixtures", "nb_monotone_incr.rds"))
  load(test_path("fixtures", "nb_quad_mcmc+_objects.Rdata"))

  samples_process(
    samples = nb_quad_model_samples_updatedattr,
    data = nb_monotone_incr
  )

  expect_no_error(
    get_mean(
      samples = nb_quad_model_samples_updatedattr,
      doses = rep(nb_monotone_incr$dose, each = n_mcmc),
      intercept = intercept
    )
  )
  expect_equal(
    get_mean(
      samples = nb_quad_model_samples_updatedattr,
      doses = rep(nb_monotone_incr$dose, each = n_mcmc),
      intercept = intercept
    ),
    get_mean.beaver_mcmc_negbin_quad(
      samples = nb_quad_model_samples_updatedattr,
      doses = rep(nb_monotone_incr$dose, each = n_mcmc),
      intercept = intercept
    )
  )
})

test_that("mean_negbin_emax produces an object with correct properties (small scale)", { # nolint

  dose_vals <- 1:3
  n_samps <- 1L

  set.seed(1234)
  b1 <- runif(n_samps, 1.9, 2.1)
  b2 <- runif(n_samps, 2.9, 3.1)
  b3 <- runif(n_samps, 0.2, 0.4)

  expect_no_error(
    mean_negbin_emax(
      x = rep(dose_vals, each = n_samps),
      b1 = matrix(
        rep(b1, each = length(dose_vals)),
        nrow = length(b1),
        byrow = TRUE
      ),
      b2 = b2,
      b3 = b3
    )
  )

  means <-
    mean_negbin_emax(
      x = rep(dose_vals, each = n_samps),
      b1 = matrix(
        rep(b1, each = length(dose_vals)),
        nrow = length(b1),
        byrow = TRUE
      ),
      b2 = b2,
      b3 = b3
    )

  expect_true(is.matrix(means))
  expect_identical(nrow(means), n_samps)
  expect_identical(ncol(means), length(dose_vals))
  expect_identical(
    as.numeric(means),
    exp(b1 + b2 * dose_vals / (b3 + dose_vals))
  )
})

test_that("get_mean.beaver_mcmc_negbin_emax works against an S3 object of class beaver_mcmc_negbin_emax, produces an object with correct properties", { # nolint

  nb_monotone_incr <- readRDS(test_path("fixtures", "nb_monotone_incr.rds"))

  expect_failure(expect_named(nb_monotone_incr, NULL))
  expect_no_error(
    checkmate::assertSubset(
      c("dose", "response"),
      names(nb_monotone_incr)
    )
  )

  load(test_path("fixtures", "nb_emax_mcmc+_objects.Rdata"))

  expect_failure(expect_s3_class(nb_emax_model_samples_updatedattr, NA))
  expect_s3_class(nb_emax_model_samples_updatedattr, "beaver_mcmc_negbin_emax")
  expect_no_error(
    checkmate::assertSubset(
      c("covariate_names", "formula"),
      names(attributes(nb_emax_model_samples_updatedattr))
    )
  )

  samples_process(
    samples = nb_emax_model_samples_updatedattr,
    data = nb_monotone_incr
  )

  expect_no_error(
    get_mean.beaver_mcmc_negbin_emax(
      samples = nb_emax_model_samples_updatedattr,
      doses = rep(nb_monotone_incr$dose, each = n_mcmc),
      intercept = intercept
    )
  )

  means <- get_mean.beaver_mcmc_negbin_emax(
    samples = nb_emax_model_samples_updatedattr,
    doses = rep(nb_monotone_incr$dose, each = n_mcmc),
    intercept = intercept
  )

  mean_checks(
    means = means,
    samples = nb_emax_model_samples_updatedattr,
    data = nb_monotone_incr
  )

  expect_identical(
    get_mean.beaver_mcmc_negbin_emax(
      samples = nb_emax_model_samples_updatedattr,
      doses = rep(nb_monotone_incr$dose, each = n_mcmc),
      intercept = intercept,
      samps = samps
    ),
    means
  )
})

test_that("get_mean works identically to get_mean.beaver_mcmc_negbin_emax", {

  nb_monotone_incr <- readRDS(test_path("fixtures", "nb_monotone_incr.rds"))
  load(test_path("fixtures", "nb_emax_mcmc+_objects.Rdata"))

  samples_process(
    samples = nb_emax_model_samples_updatedattr,
    data = nb_monotone_incr
  )

  expect_no_error(
    get_mean(
      samples = nb_emax_model_samples_updatedattr,
      doses = rep(nb_monotone_incr$dose, each = n_mcmc),
      intercept = intercept
    )
  )
  expect_equal(
    get_mean(
      samples = nb_emax_model_samples_updatedattr,
      doses = rep(nb_monotone_incr$dose, each = n_mcmc),
      intercept = intercept
    ),
    get_mean.beaver_mcmc_negbin_emax(
      samples = nb_emax_model_samples_updatedattr,
      doses = rep(nb_monotone_incr$dose, each = n_mcmc),
      intercept = intercept
    )
  )
})

test_that("mean_negbin_exp produces an object with correct properties (small scale)", { # nolint

  dose_vals <- 1:3
  n_samps <- 1L

  set.seed(1234)
  b1 <- runif(n_samps, 2.1, 2.3)
  b2 <- runif(n_samps, 2.5, 2.7)
  b3 <- runif(n_samps, 2.9, 3.1)

  expect_no_error(
    mean_negbin_exp(
      x = rep(dose_vals, each = n_samps),
      b1 = matrix(
        rep(b1, each = length(dose_vals)),
        nrow = length(b1),
        byrow = TRUE
      ),
      b2 = b2,
      b3 = b3
    )
  )

  means <-
    mean_negbin_exp(
      x = rep(dose_vals, each = n_samps),
      b1 = matrix(
        rep(b1, each = length(dose_vals)),
        nrow = length(b1),
        byrow = TRUE
      ),
      b2 = b2,
      b3 = b3
    )

  expect_true(is.matrix(means))
  expect_identical(nrow(means), n_samps)
  expect_identical(ncol(means), length(dose_vals))
  expect_identical(
    as.numeric(means),
    exp(b1 + b2 * (1 - exp(- b3 * dose_vals)))
  )
})

test_that("get_mean.beaver_mcmc_negbin_exp works against an S3 object of class beaver_mcmc_negbin_exp, produces an object with correct properties", { # nolint

  nb_monotone_incr <- readRDS(test_path("fixtures", "nb_monotone_incr.rds"))

  expect_failure(expect_named(nb_monotone_incr, NULL))
  expect_no_error(
    checkmate::assertSubset(
      c("dose", "response"),
      names(nb_monotone_incr)
    )
  )

  load(test_path("fixtures", "nb_exp_mcmc+_objects.Rdata"))

  expect_failure(expect_s3_class(nb_exp_model_samples_updatedattr, NA))
  expect_s3_class(nb_exp_model_samples_updatedattr, "beaver_mcmc_negbin_exp")
  expect_no_error(
    checkmate::assertSubset(
      c("covariate_names", "formula"),
      names(attributes(nb_exp_model_samples_updatedattr))
    )
  )

  samples_process(
    samples = nb_exp_model_samples_updatedattr,
    data = nb_monotone_incr
  )

  expect_no_error(
    get_mean.beaver_mcmc_negbin_exp(
      samples = nb_exp_model_samples_updatedattr,
      doses = rep(nb_monotone_incr$dose, each = n_mcmc),
      intercept = intercept
    )
  )

  means <- get_mean.beaver_mcmc_negbin_exp(
    samples = nb_exp_model_samples_updatedattr,
    doses = rep(nb_monotone_incr$dose, each = n_mcmc),
    intercept = intercept
  )

  mean_checks(
    means = means,
    samples = nb_exp_model_samples_updatedattr,
    data = nb_monotone_incr
  )

  expect_identical(
    get_mean.beaver_mcmc_negbin_exp(
      samples = nb_exp_model_samples_updatedattr,
      doses = rep(nb_monotone_incr$dose, each = n_mcmc),
      intercept = intercept,
      samps = samps
    ),
    means
  )
})

test_that("get_mean works identically to get_mean.beaver_mcmc_negbin_exp", {

  nb_monotone_incr <- readRDS(test_path("fixtures", "nb_monotone_incr.rds"))
  load(test_path("fixtures", "nb_exp_mcmc+_objects.Rdata"))

  samples_process(
    samples = nb_exp_model_samples_updatedattr,
    data = nb_monotone_incr
  )

  expect_no_error(
    get_mean(
      samples = nb_exp_model_samples_updatedattr,
      doses = rep(nb_monotone_incr$dose, each = n_mcmc),
      intercept = intercept
    )
  )
  expect_equal(
    get_mean(
      samples = nb_exp_model_samples_updatedattr,
      doses = rep(nb_monotone_incr$dose, each = n_mcmc),
      intercept = intercept
    ),
    get_mean.beaver_mcmc_negbin_exp(
      samples = nb_exp_model_samples_updatedattr,
      doses = rep(nb_monotone_incr$dose, each = n_mcmc),
      intercept = intercept
    )
  )
})

test_that("mean_negbin_sigmoid_emax produces an object with correct properties (small scale)", { # nolint

  dose_vals <- 1:3
  n_samps <- 1L

  set.seed(1234)
  b1 <- runif(n_samps, 1.9, 2.1)
  b2 <- runif(n_samps, 2.9, 3.1)
  b3 <- runif(n_samps, 0.2, 0.4)
  b4 <- runif(n_samps, 1.1, 1.3)

  expect_no_error(
    mean_negbin_sigmoid_emax(
      x = rep(dose_vals, each = n_samps),
      b1 = matrix(
        rep(b1, each = length(dose_vals)),
        nrow = length(b1),
        byrow = TRUE
      ),
      b2 = b2,
      b3 = b3,
      b4 = b4
    )
  )

  means <-
    mean_negbin_sigmoid_emax(
      x = rep(dose_vals, each = n_samps),
      b1 = matrix(
        rep(b1, each = length(dose_vals)),
        nrow = length(b1),
        byrow = TRUE
      ),
      b2 = b2,
      b3 = b3,
      b4 = b4
    )

  expect_true(is.matrix(means))
  expect_identical(nrow(means), n_samps)
  expect_identical(ncol(means), length(dose_vals))
  expect_identical(
    as.numeric(means),
    exp(b1 + b2 * dose_vals ^ b4 / (b3 ^ b4 + dose_vals ^ b4))
  )
})

test_that("get_mean.beaver_mcmc_negbin_sigmoid_emax works against an S3 object of class beaver_mcmc_negbin_sigmoid_emax, produces an object with correct properties", { # nolint

  nb_monotone_incr <- readRDS(test_path("fixtures", "nb_monotone_incr.rds"))

  expect_failure(expect_named(nb_monotone_incr, NULL))
  expect_no_error(
    checkmate::assertSubset(
      c("dose", "response"),
      names(nb_monotone_incr)
    )
  )

  load(test_path("fixtures", "nb_sigmoid_emax_mcmc+_objects.Rdata"))

  expect_failure(
    expect_s3_class(nb_sigmoid_emax_model_samples_updatedattr, NA)
  )
  expect_s3_class(
    nb_sigmoid_emax_model_samples_updatedattr,
    "beaver_mcmc_negbin_sigmoid_emax"
  )
  expect_no_error(
    checkmate::assertSubset(
      c("covariate_names", "formula"),
      names(attributes(nb_sigmoid_emax_model_samples_updatedattr))
    )
  )

  samples_process(
    samples = nb_sigmoid_emax_model_samples_updatedattr,
    data = nb_monotone_incr
  )

  expect_no_error(
    get_mean.beaver_mcmc_negbin_sigmoid_emax(
      samples = nb_sigmoid_emax_model_samples_updatedattr,
      doses = rep(nb_monotone_incr$dose, each = n_mcmc),
      intercept = intercept
    )
  )

  means <- get_mean.beaver_mcmc_negbin_sigmoid_emax(
    samples = nb_sigmoid_emax_model_samples_updatedattr,
    doses = rep(nb_monotone_incr$dose, each = n_mcmc),
    intercept = intercept
  )

  mean_checks(
    means = means,
    samples = nb_sigmoid_emax_model_samples_updatedattr,
    data = nb_monotone_incr
  )

  expect_identical(
    get_mean.beaver_mcmc_negbin_sigmoid_emax(
      samples = nb_sigmoid_emax_model_samples_updatedattr,
      doses = rep(nb_monotone_incr$dose, each = n_mcmc),
      intercept = intercept,
      samps = samps
    ),
    means
  )
})

test_that("get_mean works identically to get_mean.beaver_mcmc_negbin_sigmoid_emax", { # nolint

  nb_monotone_incr <- readRDS(test_path("fixtures", "nb_monotone_incr.rds"))
  load(test_path("fixtures", "nb_sigmoid_emax_mcmc+_objects.Rdata"))

  samples_process(
    samples = nb_sigmoid_emax_model_samples_updatedattr,
    data = nb_monotone_incr
  )

  expect_no_error(
    get_mean(
      samples = nb_sigmoid_emax_model_samples_updatedattr,
      doses = rep(nb_monotone_incr$dose, each = n_mcmc),
      intercept = intercept
    )
  )
  expect_equal(
    get_mean(
      samples = nb_sigmoid_emax_model_samples_updatedattr,
      doses = rep(nb_monotone_incr$dose, each = n_mcmc),
      intercept = intercept
    ),
    get_mean.beaver_mcmc_negbin_sigmoid_emax(
      samples = nb_sigmoid_emax_model_samples_updatedattr,
      doses = rep(nb_monotone_incr$dose, each = n_mcmc),
      intercept = intercept
    )
  )
})

test_that("mean_negbin_loglinear produces an object with correct properties (small scale)", { # nolint

  dose_vals <- 1:3
  n_samps <- 1L

  set.seed(1234)
  b1 <- runif(n_samps, 2.9, 3.1)
  b2 <- runif(n_samps, 1.4, 1.6)

  expect_no_error(
    mean_negbin_loglinear(
      x = rep(dose_vals, each = n_samps),
      b1 = matrix(
        rep(b1, each = length(dose_vals)),
        nrow = length(b1),
        byrow = TRUE
      ),
      b2 = b2
    )
  )

  means <-
    mean_negbin_loglinear(
      x = rep(dose_vals, each = n_samps),
      b1 = matrix(
        rep(b1, each = length(dose_vals)),
        nrow = length(b1),
        byrow = TRUE
      ),
      b2 = b2
    )

  expect_true(is.matrix(means))
  expect_identical(nrow(means), n_samps)
  expect_identical(ncol(means), length(dose_vals))
  expect_identical(as.numeric(means), exp(b1 + b2 * log(1 + dose_vals)))
})

test_that("get_mean.beaver_mcmc_negbin_loglinear works against an S3 object of class beaver_mcmc_negbin_loglinear, produces an object with correct properties", { # nolint

  nb_monotone_incr <- readRDS(test_path("fixtures", "nb_monotone_incr.rds"))

  expect_failure(expect_named(nb_monotone_incr, NULL))
  expect_no_error(
    checkmate::assertSubset(
      c("dose", "response"),
      names(nb_monotone_incr)
    )
  )

  load(test_path("fixtures", "nb_loglinear_mcmc+_objects.Rdata"))

  expect_failure(
    expect_s3_class(nb_loglinear_model_samples_updatedattr, NA)
  )
  expect_s3_class(
    nb_loglinear_model_samples_updatedattr,
    "beaver_mcmc_negbin_loglinear"
  )
  expect_no_error(
    checkmate::assertSubset(
      c("covariate_names", "formula"),
      names(attributes(nb_loglinear_model_samples_updatedattr))
    )
  )

  samples_process(
    samples = nb_loglinear_model_samples_updatedattr,
    data = nb_monotone_incr
  )

  expect_no_error(
    get_mean.beaver_mcmc_negbin_loglinear(
      samples = nb_loglinear_model_samples_updatedattr,
      doses = rep(nb_monotone_incr$dose, each = n_mcmc),
      intercept = intercept
    )
  )

  means <- get_mean.beaver_mcmc_negbin_loglinear(
    samples = nb_loglinear_model_samples_updatedattr,
    doses = rep(nb_monotone_incr$dose, each = n_mcmc),
    intercept = intercept
  )

  mean_checks(
    means = means,
    samples = nb_loglinear_model_samples_updatedattr,
    data = nb_monotone_incr
  )

  expect_identical(
    get_mean.beaver_mcmc_negbin_loglinear(
      samples = nb_loglinear_model_samples_updatedattr,
      doses = rep(nb_monotone_incr$dose, each = n_mcmc),
      intercept = intercept,
      samps = samps
    ),
    means
  )
})

test_that("get_mean works identically to get_mean.beaver_mcmc_negbin_loglinear", { # nolint

  nb_monotone_incr <- readRDS(test_path("fixtures", "nb_monotone_incr.rds"))
  load(test_path("fixtures", "nb_loglinear_mcmc+_objects.Rdata"))

  samples_process(
    samples = nb_loglinear_model_samples_updatedattr,
    data = nb_monotone_incr
  )

  expect_no_error(
    get_mean(
      samples = nb_loglinear_model_samples_updatedattr,
      doses = rep(nb_monotone_incr$dose, each = n_mcmc),
      intercept = intercept
    )
  )
  expect_equal(
    get_mean(
      samples = nb_loglinear_model_samples_updatedattr,
      doses = rep(nb_monotone_incr$dose, each = n_mcmc),
      intercept = intercept
    ),
    get_mean.beaver_mcmc_negbin_loglinear(
      samples = nb_loglinear_model_samples_updatedattr,
      doses = rep(nb_monotone_incr$dose, each = n_mcmc),
      intercept = intercept
    )
  )
})

test_that("mean_negbin_logquad produces an object with correct properties (small scale)", { # nolint

  dose_vals <- 1:3
  n_samps <- 1L

  set.seed(1234)
  b1 <- runif(n_samps, 2.9, 3.1)
  b2 <- runif(n_samps, 1.4, 1.6)
  b3 <- runif(n_samps, -3.1, -2.9) # nolint

  expect_no_error(
    mean_negbin_logquad(
      x = rep(dose_vals, each = n_samps),
      b1 = matrix(
        rep(b1, each = length(dose_vals)),
        nrow = length(b1),
        byrow = TRUE
      ),
      b2 = b2,
      b3 = b3
    )
  )

  means <-
    mean_negbin_logquad(
      x = rep(dose_vals, each = n_samps),
      b1 = matrix(
        rep(b1, each = length(dose_vals)),
        nrow = length(b1),
        byrow = TRUE
      ),
      b2 = b2,
      b3 = b3
    )

  expect_true(is.matrix(means))
  expect_identical(nrow(means), n_samps)
  expect_identical(ncol(means), length(dose_vals))
  expect_identical(
    as.numeric(means),
    exp(b1 + b2 * log(1 + dose_vals) + b3 * log(1 + dose_vals) ^ 2)
  )
})

test_that("get_mean.beaver_mcmc_negbin_logquad works against an S3 object of class beaver_mcmc_negbin_logquad, produces an object with correct properties", { # nolint

  nb_monotone_incr <- readRDS(test_path("fixtures", "nb_monotone_incr.rds"))

  expect_failure(expect_named(nb_monotone_incr, NULL))
  expect_no_error(
    checkmate::assertSubset(
      c("dose", "response"),
      names(nb_monotone_incr)
    )
  )

  load(test_path("fixtures", "nb_logquad_mcmc+_objects.Rdata"))

  expect_failure(
    expect_s3_class(nb_logquad_model_samples_updatedattr, NA)
  )
  expect_s3_class(
    nb_logquad_model_samples_updatedattr,
    "beaver_mcmc_negbin_logquad"
  )
  expect_no_error(
    checkmate::assertSubset(
      c("covariate_names", "formula"),
      names(attributes(nb_logquad_model_samples_updatedattr))
    )
  )

  samples_process(
    samples = nb_logquad_model_samples_updatedattr,
    data = nb_monotone_incr
  )

  expect_no_error(
    get_mean.beaver_mcmc_negbin_logquad(
      samples = nb_logquad_model_samples_updatedattr,
      doses = rep(nb_monotone_incr$dose, each = n_mcmc),
      intercept = intercept
    )
  )

  means <- get_mean.beaver_mcmc_negbin_logquad(
    samples = nb_logquad_model_samples_updatedattr,
    doses = rep(nb_monotone_incr$dose, each = n_mcmc),
    intercept = intercept
  )

  mean_checks(
    means = means,
    samples = nb_logquad_model_samples_updatedattr,
    data = nb_monotone_incr
  )

  expect_identical(
    get_mean.beaver_mcmc_negbin_logquad(
      samples = nb_logquad_model_samples_updatedattr,
      doses = rep(nb_monotone_incr$dose, each = n_mcmc),
      intercept = intercept,
      samps = samps
    ),
    means
  )
})

test_that("get_mean works identically to get_mean.beaver_mcmc_negbin_logquad", { # nolint

  nb_monotone_incr <- readRDS(test_path("fixtures", "nb_monotone_incr.rds"))
  load(test_path("fixtures", "nb_logquad_mcmc+_objects.Rdata"))

  samples_process(
    samples = nb_logquad_model_samples_updatedattr,
    data = nb_monotone_incr
  )

  expect_no_error(
    get_mean(
      samples = nb_logquad_model_samples_updatedattr,
      doses = rep(nb_monotone_incr$dose, each = n_mcmc),
      intercept = intercept
    )
  )
  expect_equal(
    get_mean(
      samples = nb_logquad_model_samples_updatedattr,
      doses = rep(nb_monotone_incr$dose, each = n_mcmc),
      intercept = intercept
    ),
    get_mean.beaver_mcmc_negbin_logquad(
      samples = nb_logquad_model_samples_updatedattr,
      doses = rep(nb_monotone_incr$dose, each = n_mcmc),
      intercept = intercept
    )
  )
})
