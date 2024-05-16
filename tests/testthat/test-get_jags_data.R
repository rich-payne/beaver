
test_that("get_jags_data.default works against an S3 object of class data.frame with numeric variables dose and response, produces an object with correct properties", { # nolint

  nb_monotone_incr <- readRDS(test_path("fixtures", "nb_monotone_incr.rds"))

  expect_failure(expect_s3_class(nb_monotone_incr, NA))
  expect_s3_class(nb_monotone_incr, "data.frame")
  expect_failure(expect_named(nb_monotone_incr, NULL))
  expect_no_error(
    checkmate::assertSubset(
      c("dose", "response"),
      names(nb_monotone_incr)
    )
  )
  expect_true(is.numeric(nb_monotone_incr$dose))
  expect_true(is.numeric(nb_monotone_incr$response))

  nb_monotone_incr_cov <- readRDS(test_path("fixtures", "nb_monotone_incr_cov.rds")) # nolint

  expect_failure(expect_s3_class(nb_monotone_incr_cov, NA))
  expect_s3_class(nb_monotone_incr_cov, "data.frame")
  expect_failure(expect_named(nb_monotone_incr_cov, NULL))
  expect_no_error(
    checkmate::assertSubset(
      c("dose", "response", "age"),
      names(nb_monotone_incr_cov)
    )
  )
  expect_true(is.numeric(nb_monotone_incr_cov$dose))
  expect_true(is.numeric(nb_monotone_incr_cov$response))
  expect_true(is.numeric(nb_monotone_incr_cov$age))

  model <- list()

  #no covariates----

  formula <- ~ 1
  nb_monotone_incr_x <- model.matrix(formula, data = nb_monotone_incr)

  expect_no_error(
    get_jags_data.default(
      model = model,
      data = nb_monotone_incr,
      x = nb_monotone_incr_x
    )
  )

  expect_invisible(
    get_jags_data.default(
      model = model,
      data = nb_monotone_incr,
      x = nb_monotone_incr_x
    )
  )

  jags_data <- get_jags_data.default(
    model = model,
    data = nb_monotone_incr,
    x = nb_monotone_incr_x
  )

  get_jags_data_checks(
    jags_data = jags_data,
    data = nb_monotone_incr,
    x = nb_monotone_incr_x
  )

  #covariates----

  formula_cov <- ~ age
  nb_monotone_incr_cov_x <- model.matrix(
    formula_cov,
    data = nb_monotone_incr_cov
  )

  expect_no_error(
    get_jags_data.default(
      model = model,
      data = nb_monotone_incr_cov,
      x = nb_monotone_incr_cov_x
    )
  )

  expect_invisible(
    get_jags_data.default(
      model = model,
      data = nb_monotone_incr_cov,
      x = nb_monotone_incr_cov_x
    )
  )

  jags_data_cov <- get_jags_data.default(
    model = model,
    data = nb_monotone_incr_cov,
    x = nb_monotone_incr_cov_x
  )

  get_jags_data_checks(
    jags_data = jags_data_cov,
    data = nb_monotone_incr_cov,
    x = nb_monotone_incr_cov_x
  )
})

test_that("get_jags_data works identically to get_jags_data.default", {

  nb_monotone_incr <- readRDS(test_path("fixtures", "nb_monotone_incr.rds")) # nolint
  nb_monotone_incr_cov <- readRDS(test_path("fixtures", "nb_monotone_incr_cov.rds")) # nolint

  model <- list()

  #no covariates----

  formula <- ~ 1
  nb_monotone_incr_x <- model.matrix(formula, data = nb_monotone_incr)

  expect_no_error(
    get_jags_data(
      model = model,
      data = nb_monotone_incr,
      x = nb_monotone_incr_x
    )
  )

  expect_equal(
    get_jags_data(
      model = model,
      data = nb_monotone_incr,
      x = nb_monotone_incr_x
    ),
    get_jags_data.default(
      model = model,
      data = nb_monotone_incr,
      x = nb_monotone_incr_x
    )
  )

  #covariates----

  formula_cov <- ~ age
  nb_monotone_incr_cov_x <- model.matrix(
    formula_cov,
    data = nb_monotone_incr_cov
  )

  expect_no_error(
    get_jags_data(
      model = model,
      data = nb_monotone_incr_cov,
      x = nb_monotone_incr_cov_x
    )
  )

  expect_equal(
    get_jags_data(
      model = model,
      data = nb_monotone_incr_cov,
      x = nb_monotone_incr_cov_x
    ),
    get_jags_data.default(
      model = model,
      data = nb_monotone_incr_cov,
      x = nb_monotone_incr_cov_x
    )
  )
})
