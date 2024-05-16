
test_that("draws.beaver_mcmc works against an S3 object of class beaver_mcmc, produces an object with correct properties", { # nolint

  load(test_path("fixtures", "nb_indep_mcmc+_objects.Rdata"))

  expect_failure(expect_s3_class(nb_indep_model_samples_updatedattr, NA))
  expect_s3_class(nb_indep_model_samples_updatedattr, "beaver_mcmc")

  expect_no_error(draws.beaver_mcmc(nb_indep_model_samples_updatedattr))

  draws <- draws.beaver_mcmc(nb_indep_model_samples_updatedattr)

  expect_s3_class(draws, "data.frame")
  expect_named(
    draws,
    c(colnames(nb_indep_model_samples_updatedattr[[1]]), "iter")
  )
  expect_identical(
    nrow(draws),
    sum(sapply(nb_indep_model_samples_updatedattr, nrow))
  )
  expect_type(draws$iter, "integer")
  expect_equal(
    draws$iter,
    1:sum(sapply(nb_indep_model_samples_updatedattr, nrow))
  )
})

test_that("draws works identically to draws.beaver_mcmc", {

  load(test_path("fixtures", "nb_indep_mcmc+_objects.Rdata"))

  expect_no_error(draws(nb_indep_model_samples_updatedattr))
  expect_equal(
    draws(nb_indep_model_samples_updatedattr),
    draws.beaver_mcmc(nb_indep_model_samples_updatedattr)
  )
})

test_that("draws.beaver_mcmc works against an S3 object of class mcmc.list (but not of class beaver_mcmc)", { # nolint

  load(test_path("fixtures", "nb_indep_mcmc+_objects.Rdata"))

  expect_failure(expect_s3_class(nb_indep_model_samples, NA))
  expect_failure(expect_s3_class(nb_indep_model_samples, "beaver_mcmc"))
  expect_s3_class(nb_indep_model_samples, "mcmc.list")

  expect_no_error(draws.beaver_mcmc(nb_indep_model_samples))
})

test_that("draws does not work against an S3 object of class mcmc.list (but not of class beaver_mcmc)", { # nolint

  load(test_path("fixtures", "nb_indep_mcmc+_objects.Rdata"))

  expect_failure(expect_s3_class(nb_indep_model_samples, NA))
  expect_failure(expect_s3_class(nb_indep_model_samples, "beaver_mcmc"))
  expect_s3_class(nb_indep_model_samples, "mcmc.list")

  expect_error(draws(nb_indep_model_samples))
})

test_that("draws.beaver_mcmc does not work against an S3 object of class beaver_mcmc_bma (even if of class beaver_mcmc)", { # nolint

  load(test_path("fixtures", "nb_bma_objects.Rdata"))

  expect_failure(expect_s3_class(nb_bma, NA))
  expect_s3_class(
    nb_bma,
    c("beaver_mcmc_bma", "yodel_bma", "beaver_mcmc"),
    exact = TRUE
  )

  expect_error(draws.beaver_mcmc(nb_bma))
})

test_that("draws does not work against an S3 object of class beaver_mcmc_bma (even if of class beaver_mcmc)", { # nolint

  load(test_path("fixtures", "nb_bma_objects.Rdata"))

  expect_failure(expect_s3_class(nb_bma, NA))
  expect_s3_class(
    nb_bma,
    c("beaver_mcmc_bma", "yodel_bma", "beaver_mcmc"),
    exact = TRUE
  )

  expect_error(draws(nb_bma), "draws is intended for single model fits only")
})
