
test_that("get_jags_model.beaver_negbin_indep works against an S3 object of class beaver_negbin_indep, produces an object with correct properties", { # nolint

  model_class <- c("beaver_negbin_indep")

  model <- list()
  class(model) <- model_class

  expect_failure(expect_s3_class(model, NA))
  expect_s3_class(model, "beaver_negbin_indep")

  expect_no_error(get_jags_model.beaver_negbin_indep(model = model))

  jags_model <- get_jags_model.beaver_negbin_indep(model = model)

  jags_model_checks(model = jags_model, model_class = model_class)
})

test_that("get_jags_model works identically to get_jags_model.beaver_negbin_indep", { # nolint

  model_class <- c("beaver_negbin_indep")

  model <- list()
  class(model) <- model_class

  expect_no_error(get_jags_model(model = model))
  expect_identical(
    expect_no_error(get_jags_model(model = model)),
    get_jags_model.beaver_negbin_indep(model = model)
  )
})

test_that("get_jags_model.beaver_negbin_emax works against an S3 object of class beaver_negbin_emax, produces an object with correct properties", { # nolint

  model_class <- c("beaver_negbin_emax")

  model <- list()
  class(model) <- model_class

  expect_failure(expect_s3_class(model, NA))
  expect_s3_class(model, "beaver_negbin_emax")

  expect_no_error(get_jags_model.beaver_negbin_emax(model = model))

  jags_model <- get_jags_model.beaver_negbin_emax(model = model)

  jags_model_checks(model = jags_model, model_class = model_class)
})

test_that("get_jags_model works identically to get_jags_model.beaver_negbin_emax", { # nolint

  model_class <- c("beaver_negbin_emax")

  model <- list()
  class(model) <- model_class

  expect_no_error(get_jags_model(model = model))
  expect_identical(
    expect_no_error(get_jags_model(model = model)),
    get_jags_model.beaver_negbin_emax(model = model)
  )
})

test_that("get_jags_model.beaver_negbin_sigmoid_emax works against an S3 object of class beaver_negbin_sigmoid_emax, produces an object with correct properties", { # nolint

  model_class <- c("beaver_negbin_sigmoid_emax")

  model <- list()
  class(model) <- model_class

  expect_failure(expect_s3_class(model, NA))
  expect_s3_class(model, "beaver_negbin_sigmoid_emax")

  expect_no_error(get_jags_model.beaver_negbin_sigmoid_emax(model = model))

  jags_model <- get_jags_model.beaver_negbin_sigmoid_emax(model = model)

  jags_model_checks(model = jags_model, model_class = model_class)
})

test_that("get_jags_model works identically to get_jags_model.beaver_negbin_sigmoid_emax", { # nolint

  model_class <- c("beaver_negbin_sigmoid_emax")

  model <- list()
  class(model) <- model_class

  expect_no_error(get_jags_model(model = model))
  expect_identical(
    expect_no_error(get_jags_model(model = model)),
    get_jags_model.beaver_negbin_sigmoid_emax(model = model)
  )
})

test_that("get_jags_model.beaver_negbin_linear works against an S3 object of class beaver_negbin_linear, produces an object with correct properties", { # nolint

  model_class <- c("beaver_negbin_linear")

  model <- list()
  class(model) <- model_class

  expect_failure(expect_s3_class(model, NA))
  expect_s3_class(model, "beaver_negbin_linear")

  expect_no_error(get_jags_model.beaver_negbin_linear(model = model))

  jags_model <- get_jags_model.beaver_negbin_linear(model = model)

  jags_model_checks(model = jags_model, model_class = model_class)
})

test_that("get_jags_model works identically to get_jags_model.beaver_negbin_linear", { # nolint

  model_class <- c("beaver_negbin_linear")

  model <- list()
  class(model) <- model_class

  expect_no_error(get_jags_model(model = model))
  expect_identical(
    expect_no_error(get_jags_model(model = model)),
    get_jags_model.beaver_negbin_linear(model = model)
  )
})

test_that("get_jags_model.beaver_negbin_loglinear works against an S3 object of class beaver_negbin_loglinear, produces an object with correct properties", { # nolint

  model_class <- c("beaver_negbin_loglinear")

  model <- list()
  class(model) <- model_class

  expect_failure(expect_s3_class(model, NA))
  expect_s3_class(model, "beaver_negbin_loglinear")

  expect_no_error(get_jags_model.beaver_negbin_loglinear(model = model))

  jags_model <- get_jags_model.beaver_negbin_loglinear(model = model)

  jags_model_checks(model = jags_model, model_class = model_class)
})

test_that("get_jags_model works identically to get_jags_model.beaver_negbin_loglinear", { # nolint

  model_class <- c("beaver_negbin_loglinear")

  model <- list()
  class(model) <- model_class

  expect_no_error(get_jags_model(model = model))
  expect_identical(
    expect_no_error(get_jags_model(model = model)),
    get_jags_model.beaver_negbin_loglinear(model = model)
  )
})

test_that("get_jags_model.beaver_negbin_quad works against an S3 object of class beaver_negbin_quad, produces an object with correct properties", { # nolint

  model_class <- c("beaver_negbin_quad")

  model <- list()
  class(model) <- model_class

  expect_failure(expect_s3_class(model, NA))
  expect_s3_class(model, "beaver_negbin_quad")

  expect_no_error(get_jags_model.beaver_negbin_quad(model = model))

  jags_model <- get_jags_model.beaver_negbin_quad(model = model)

  jags_model_checks(model = jags_model, model_class = model_class)
})

test_that("get_jags_model works identically to get_jags_model.beaver_negbin_quad", { # nolint

  model_class <- c("beaver_negbin_quad")

  model <- list()
  class(model) <- model_class

  expect_no_error(get_jags_model(model = model))
  expect_identical(
    expect_no_error(get_jags_model(model = model)),
    get_jags_model.beaver_negbin_quad(model = model)
  )
})

test_that("get_jags_model.beaver_negbin_logquad works against an S3 object of class beaver_negbin_logquad, produces an object with correct properties", { # nolint

  model_class <- c("beaver_negbin_logquad")

  model <- list()
  class(model) <- model_class

  expect_failure(expect_s3_class(model, NA))
  expect_s3_class(model, "beaver_negbin_logquad")

  expect_no_error(get_jags_model.beaver_negbin_logquad(model = model))

  jags_model <- get_jags_model.beaver_negbin_logquad(model = model)

  jags_model_checks(model = jags_model, model_class = model_class)
})

test_that("get_jags_model works identically to get_jags_model.beaver_negbin_logquad", { # nolint

  model_class <- c("beaver_negbin_logquad")

  model <- list()
  class(model) <- model_class

  expect_no_error(get_jags_model(model = model))
  expect_identical(
    expect_no_error(get_jags_model(model = model)),
    get_jags_model.beaver_negbin_logquad(model = model)
  )
})

test_that("get_jags_model.beaver_negbin_exp works against an S3 object of class beaver_negbin_exp, produces an object with correct properties", { # nolint

  model_class <- c("beaver_negbin_exp")

  model <- list()
  class(model) <- model_class

  expect_failure(expect_s3_class(model, NA))
  expect_s3_class(model, "beaver_negbin_exp")

  expect_no_error(get_jags_model.beaver_negbin_exp(model = model))

  jags_model <- get_jags_model.beaver_negbin_exp(model = model)

  jags_model_checks(model = jags_model, model_class = model_class)
})

test_that("get_jags_model works identically to get_jags_model.beaver_negbin_exp", { # nolint

  model_class <- c("beaver_negbin_exp")

  model <- list()
  class(model) <- model_class

  expect_no_error(get_jags_model(model = model))
  expect_identical(
    expect_no_error(get_jags_model(model = model)),
    get_jags_model.beaver_negbin_exp(model = model)
  )
})
