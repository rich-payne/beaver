
test_that("get_n_params.beaver_mcmc_negbin_indep works against an S3 object of class beaver_mcmc_negbin_indep, produces an object with correct properties", { # nolint

  load(test_path("fixtures", "nb_indep_mcmc+_objects.Rdata"))

  expect_failure(expect_s3_class(nb_indep_model_samples_updatedattr, NA))
  expect_s3_class(
    nb_indep_model_samples_updatedattr,
    "beaver_mcmc_negbin_indep"
  )
  expect_no_error(
    checkmate::assertSubset(
      c("doses", "n_b1"),
      names(attributes(nb_indep_model_samples_updatedattr))
    )
  )

  expect_no_error(
    get_n_params.beaver_mcmc_negbin_indep(
      model = nb_indep_model_samples_updatedattr
    )
  )

  n_params <- get_n_params.beaver_mcmc_negbin_indep(
    model = nb_indep_model_samples_updatedattr
  )

  expect_true(is.numeric(n_params))
  expect_identical(length(n_params), 1L)
})

test_that("get_n_params works identically to get_n_params.beaver_mcmc_negbin_indep", { # nolint

  load(test_path("fixtures", "nb_indep_mcmc+_objects.Rdata"))

  expect_no_error(get_n_params(model = nb_indep_model_samples_updatedattr))
  expect_identical(
    get_n_params(model = nb_indep_model_samples_updatedattr),
    get_n_params.beaver_mcmc_negbin_indep(
      model = nb_indep_model_samples_updatedattr
    )
  )
})

test_that("get_n_params.beaver_mcmc_negbin_emax works against an S3 object of class beaver_mcmc_negbin_emax, produces an object with correct properties", { # nolint

  load(test_path("fixtures", "nb_emax_mcmc+_objects.Rdata"))

  expect_failure(expect_s3_class(nb_emax_model_samples_updatedattr, NA))
  expect_s3_class(nb_emax_model_samples_updatedattr, "beaver_mcmc_negbin_emax")
  expect_no_error(
    checkmate::assertSubset(
      c("doses", "n_b1"),
      names(attributes(nb_emax_model_samples_updatedattr))
    )
  )

  expect_no_error(
    get_n_params.beaver_mcmc_negbin_emax(
      model = nb_emax_model_samples_updatedattr
    )
  )

  n_params <- get_n_params.beaver_mcmc_negbin_emax(
    model = nb_emax_model_samples_updatedattr
  )

  expect_true(is.numeric(n_params))
  expect_identical(length(n_params), 1L)
})

test_that("get_n_params works identically to get_n_params.beaver_mcmc_negbin_emax", { # nolint

  load(test_path("fixtures", "nb_emax_mcmc+_objects.Rdata"))

  expect_no_error(get_n_params(model = nb_emax_model_samples_updatedattr))
  expect_identical(
    get_n_params(model = nb_emax_model_samples_updatedattr),
    get_n_params.beaver_mcmc_negbin_emax(
      model = nb_emax_model_samples_updatedattr
    )
  )
})

test_that("get_n_params.beaver_mcmc_negbin_sigmoid_emax works against an S3 object of class beaver_mcmc_negbin_sigmoid_emax, produces an object with correct properties", { # nolint

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
      c("doses", "n_b1"),
      names(attributes(nb_sigmoid_emax_model_samples_updatedattr))
    )
  )

  expect_no_error(
    get_n_params.beaver_mcmc_negbin_sigmoid_emax(
      model = nb_sigmoid_emax_model_samples_updatedattr
    )
  )

  n_params <- get_n_params.beaver_mcmc_negbin_sigmoid_emax(
    model = nb_sigmoid_emax_model_samples_updatedattr
  )

  expect_true(is.numeric(n_params))
  expect_identical(length(n_params), 1L)
})

test_that("get_n_params works identically to get_n_params.beaver_mcmc_negbin_sigmoid_emax", { # nolint

  load(test_path("fixtures", "nb_sigmoid_emax_mcmc+_objects.Rdata"))

  expect_no_error(
    get_n_params(model = nb_sigmoid_emax_model_samples_updatedattr)
  )
  expect_identical(
    get_n_params(model = nb_sigmoid_emax_model_samples_updatedattr),
    get_n_params.beaver_mcmc_negbin_sigmoid_emax(
      model = nb_sigmoid_emax_model_samples_updatedattr
    )
  )
})

test_that("get_n_params.beaver_mcmc_negbin_linear works against an S3 object of class beaver_mcmc_negbin_linear, produces an object with correct properties", { # nolint

  load(test_path("fixtures", "nb_linear_mcmc+_objects.Rdata"))

  expect_failure(expect_s3_class(nb_linear_model_samples_updatedattr, NA))
  expect_s3_class(
    nb_linear_model_samples_updatedattr,
    "beaver_mcmc_negbin_linear"
  )
  expect_no_error(
    checkmate::assertSubset(
      c("doses", "n_b1"),
      names(attributes(nb_linear_model_samples_updatedattr))
    )
  )

  expect_no_error(
    get_n_params.beaver_mcmc_negbin_linear(
      model = nb_linear_model_samples_updatedattr
    )
  )

  n_params <- get_n_params.beaver_mcmc_negbin_linear(
    model = nb_linear_model_samples_updatedattr
  )

  expect_true(is.numeric(n_params))
  expect_identical(length(n_params), 1L)
})

test_that("get_n_params works identically to get_n_params.beaver_mcmc_negbin_linear", { # nolint

  load(test_path("fixtures", "nb_linear_mcmc+_objects.Rdata"))

  expect_no_error(get_n_params(model = nb_linear_model_samples_updatedattr))
  expect_identical(
    get_n_params(model = nb_linear_model_samples_updatedattr),
    get_n_params.beaver_mcmc_negbin_linear(
      model = nb_linear_model_samples_updatedattr
    )
  )
})

test_that("get_n_params.beaver_mcmc_negbin_loglinear works against an S3 object of class beaver_mcmc_negbin_loglinear, produces an object with correct properties", { # nolint

  load(test_path("fixtures", "nb_loglinear_mcmc+_objects.Rdata"))

  expect_failure(expect_s3_class(nb_loglinear_model_samples_updatedattr, NA))
  expect_s3_class(
    nb_loglinear_model_samples_updatedattr,
    "beaver_mcmc_negbin_loglinear"
  )
  expect_no_error(
    checkmate::assertSubset(
      c("doses", "n_b1"),
      names(attributes(nb_loglinear_model_samples_updatedattr))
    )
  )

  expect_no_error(
    get_n_params.beaver_mcmc_negbin_loglinear(
      model = nb_loglinear_model_samples_updatedattr
    )
  )

  n_params <- get_n_params.beaver_mcmc_negbin_loglinear(
    model = nb_loglinear_model_samples_updatedattr
  )

  expect_true(is.numeric(n_params))
  expect_identical(length(n_params), 1L)
})

test_that("get_n_params works identically to get_n_params.beaver_mcmc_negbin_loglinear", { # nolint

  load(test_path("fixtures", "nb_loglinear_mcmc+_objects.Rdata"))

  expect_no_error(get_n_params(model = nb_loglinear_model_samples_updatedattr))
  expect_identical(
    get_n_params(model = nb_loglinear_model_samples_updatedattr),
    get_n_params.beaver_mcmc_negbin_loglinear(
      model = nb_loglinear_model_samples_updatedattr
    )
  )
})

test_that("get_n_params.beaver_mcmc_negbin_quad works against an S3 object of class beaver_mcmc_negbin_quad, produces an object with correct properties", { # nolint

  load(test_path("fixtures", "nb_quad_mcmc+_objects.Rdata"))

  expect_failure(expect_s3_class(nb_quad_model_samples_updatedattr, NA))
  expect_s3_class(nb_quad_model_samples_updatedattr, "beaver_mcmc_negbin_quad")
  expect_no_error(
    checkmate::assertSubset(
      c("doses", "n_b1"),
      names(attributes(nb_quad_model_samples_updatedattr))
    )
  )

  expect_no_error(
    get_n_params.beaver_mcmc_negbin_quad(
      model = nb_quad_model_samples_updatedattr
    )
  )

  n_params <- get_n_params.beaver_mcmc_negbin_quad(
    model = nb_quad_model_samples_updatedattr
  )

  expect_true(is.numeric(n_params))
  expect_identical(length(n_params), 1L)
})

test_that("get_n_params works identically to get_n_params.beaver_mcmc_negbin_quad", { # nolint

  load(test_path("fixtures", "nb_quad_mcmc+_objects.Rdata"))

  expect_no_error(get_n_params(model = nb_quad_model_samples_updatedattr))
  expect_identical(
    get_n_params(model = nb_quad_model_samples_updatedattr),
    get_n_params.beaver_mcmc_negbin_quad(
      model = nb_quad_model_samples_updatedattr
    )
  )
})

test_that("get_n_params.beaver_mcmc_negbin_logquad works against an S3 object of class beaver_mcmc_negbin_logquad, produces an object with correct properties", { # nolint

  load(test_path("fixtures", "nb_logquad_mcmc+_objects.Rdata"))

  expect_failure(expect_s3_class(nb_logquad_model_samples_updatedattr, NA))
  expect_s3_class(
    nb_logquad_model_samples_updatedattr,
    "beaver_mcmc_negbin_logquad"
  )
  expect_no_error(
    checkmate::assertSubset(
      c("doses", "n_b1"),
      names(attributes(nb_logquad_model_samples_updatedattr))
    )
  )

  expect_no_error(
    get_n_params.beaver_mcmc_negbin_logquad(
      model = nb_logquad_model_samples_updatedattr
    )
  )

  n_params <- get_n_params.beaver_mcmc_negbin_logquad(
    model = nb_logquad_model_samples_updatedattr
  )

  expect_true(is.numeric(n_params))
  expect_identical(length(n_params), 1L)
})

test_that("get_n_params works identically to get_n_params.beaver_mcmc_negbin_logquad", { # nolint

  load(test_path("fixtures", "nb_logquad_mcmc+_objects.Rdata"))

  expect_no_error(get_n_params(model = nb_logquad_model_samples_updatedattr))
  expect_identical(
    get_n_params(model = nb_logquad_model_samples_updatedattr),
    get_n_params.beaver_mcmc_negbin_logquad(
      model = nb_logquad_model_samples_updatedattr
    )
  )
})

test_that("get_n_params.beaver_mcmc_negbin_exp works against an S3 object of class beaver_mcmc_negbin_exp, produces an object with correct properties", { # nolint

  load(test_path("fixtures", "nb_exp_mcmc+_objects.Rdata"))

  expect_failure(expect_s3_class(nb_exp_model_samples_updatedattr, NA))
  expect_s3_class(nb_exp_model_samples_updatedattr, "beaver_mcmc_negbin_exp")
  expect_no_error(
    checkmate::assertSubset(
      c("doses", "n_b1"),
      names(attributes(nb_exp_model_samples_updatedattr))
    )
  )

  expect_no_error(
    get_n_params.beaver_mcmc_negbin_exp(
      model = nb_exp_model_samples_updatedattr
    )
  )

  n_params <- get_n_params.beaver_mcmc_negbin_exp(
    model = nb_exp_model_samples_updatedattr
  )

  expect_true(is.numeric(n_params))
  expect_identical(length(n_params), 1L)
})

test_that("get_n_params works identically to get_n_params.beaver_mcmc_negbin_exp", { # nolint

  load(test_path("fixtures", "nb_exp_mcmc+_objects.Rdata"))

  expect_no_error(get_n_params(model = nb_exp_model_samples_updatedattr))
  expect_identical(
    get_n_params(model = nb_exp_model_samples_updatedattr),
    get_n_params.beaver_mcmc_negbin_exp(
      model = nb_exp_model_samples_updatedattr
    )
  )
})

test_that("get_n_params does not work against an S3 object of class mcmc.list (but not of class beaver_mcmc_negbin_METHOD)", { # nolint

  load(test_path("fixtures", "nb_indep_mcmc+_objects.Rdata"))

  expect_failure(expect_s3_class(nb_indep_model_samples, NA))
  expect_false(any(class(nb_indep_model_samples) %in% paste0(
    "beaver_mcmc_negbin_",
    c(
      "indep",
      "emax",
      "sigmoid_emax",
      "linear",
      "loglinear",
      "quad",
      "logquad",
      "exp"
    )
  )))
  expect_s3_class(nb_indep_model_samples_updatedattr, "mcmc.list")

  expect_error(get_n_params(model = nb_indep_model_samples))
})
