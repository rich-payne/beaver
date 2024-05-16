
test_that("get_var_names.beaver_negbin_indep works against an S3 object of class beaver_negbin_indep, produces an object with correct properties", { # nolint

  model_class <- c("beaver_negbin_indep")

  model <- list()
  class(model) <- model_class

  expect_failure(expect_s3_class(model, NA))
  expect_s3_class(model, "beaver_negbin_indep")

  expect_no_error(get_var_names.beaver_negbin_indep(model = model))

  var_names <- get_var_names.beaver_negbin_indep(model = model)

  expect_true(is.character(var_names))
  expect_gte(length(var_names), 3L)
})

test_that("get_var_names works identically to get_var_names.beaver_negbin_indep", { # nolint

  model_class <- c("beaver_negbin_indep")

  model <- list()
  class(model) <- model_class

  expect_failure(expect_s3_class(model, NA))
  expect_s3_class(model, "beaver_negbin_indep")

  expect_no_error(get_var_names(model = model))
  expect_identical(
    get_var_names(model = model),
    get_var_names.beaver_negbin_indep(model = model)
  )
})

test_that("get_var_names.beaver_negbin_indep works against an S3 object of class xyz (but not of class beaver_negbin_indep)", { # nolint

  model_class <- c("xyz")

  model <- list()
  class(model) <- model_class

  expect_failure(expect_s3_class(model, NA))
  expect_failure(expect_s3_class(model, "beaver_negbin_indep"))
  expect_s3_class(model, "xyz")

  expect_no_error(get_var_names.beaver_negbin_indep(model = model))
})

test_that("get_var_names.beaver_negbin_emax works against an S3 object of class beaver_negbin_emax, produces an object with correct properties", { # nolint

  model_class <- c("beaver_negbin_emax")

  model <- list()
  class(model) <- model_class

  expect_failure(expect_s3_class(model, NA))
  expect_s3_class(model, "beaver_negbin_emax")

  expect_no_error(get_var_names.beaver_negbin_emax(model = model))

  var_names <- get_var_names.beaver_negbin_emax(model = model)

  expect_true(is.character(var_names))
  expect_gte(length(var_names), 3L)
})

test_that("get_var_names works identically to get_var_names.beaver_negbin_emax", { # nolint

  model_class <- c("beaver_negbin_emax")

  model <- list()
  class(model) <- model_class

  expect_failure(expect_s3_class(model, NA))
  expect_s3_class(model, "beaver_negbin_emax")

  expect_no_error(get_var_names(model = model))
  expect_identical(
    get_var_names(model = model),
    get_var_names.beaver_negbin_emax(model = model)
  )
})

test_that("get_var_names.beaver_negbin_emax works against an S3 object of class xyz (but not of class beaver_negbin_emax)", { # nolint

  model_class <- c("xyz")

  model <- list()
  class(model) <- model_class

  expect_failure(expect_s3_class(model, NA))
  expect_failure(expect_s3_class(model, "beaver_negbin_emax"))
  expect_s3_class(model, "xyz")

  expect_no_error(get_var_names.beaver_negbin_emax(model = model))
})

test_that("get_var_names.beaver_negbin_sigmoid_emax works against an S3 object of class beaver_negbin_sigmoid_emax, produces an object with correct properties", { # nolint

  model_class <- c("beaver_negbin_sigmoid_emax")

  model <- list()
  class(model) <- model_class

  expect_failure(expect_s3_class(model, NA))
  expect_s3_class(model, "beaver_negbin_sigmoid_emax")

  expect_no_error(get_var_names.beaver_negbin_sigmoid_emax(model = model))

  var_names <- get_var_names.beaver_negbin_sigmoid_emax(model = model)

  expect_true(is.character(var_names))
  expect_gte(length(var_names), 3L)
})

test_that("get_var_names works identically to get_var_names.beaver_negbin_sigmoid_emax", { # nolint

  model_class <- c("beaver_negbin_sigmoid_emax")

  model <- list()
  class(model) <- model_class

  expect_failure(expect_s3_class(model, NA))
  expect_s3_class(model, "beaver_negbin_sigmoid_emax")

  expect_no_error(get_var_names(model = model))
  expect_identical(
    get_var_names(model = model),
    get_var_names.beaver_negbin_sigmoid_emax(model = model)
  )
})

test_that("get_var_names.beaver_negbin_sigmoid_emax works against an S3 object of class xyz (but not of class beaver_negbin_sigmoid_emax)", { # nolint

  model_class <- c("xyz")

  model <- list()
  class(model) <- model_class

  expect_failure(expect_s3_class(model, NA))
  expect_failure(expect_s3_class(model, "beaver_negbin_sigmoid_emax"))
  expect_s3_class(model, "xyz")

  expect_no_error(get_var_names.beaver_negbin_sigmoid_emax(model = model))
})

test_that("get_var_names.beaver_negbin_linear works against an S3 object of class beaver_negbin_linear, produces an object with correct properties", { # nolint

  model_class <- c("beaver_negbin_linear")

  model <- list()
  class(model) <- model_class

  expect_failure(expect_s3_class(model, NA))
  expect_s3_class(model, "beaver_negbin_linear")

  expect_no_error(get_var_names.beaver_negbin_linear(model = model))

  var_names <- get_var_names.beaver_negbin_linear(model = model)

  expect_true(is.character(var_names))
  expect_gte(length(var_names), 3L)
})

test_that("get_var_names works identically to get_var_names.beaver_negbin_linear", { # nolint

  model_class <- c("beaver_negbin_linear")

  model <- list()
  class(model) <- model_class

  expect_failure(expect_s3_class(model, NA))
  expect_s3_class(model, "beaver_negbin_linear")

  expect_no_error(get_var_names(model = model))
  expect_identical(
    get_var_names(model = model),
    get_var_names.beaver_negbin_linear(model = model)
  )
})

test_that("get_var_names.beaver_negbin_linear works against an S3 object of class xyz (but not of class beaver_negbin_linear)", { # nolint

  model_class <- c("xyz")

  model <- list()
  class(model) <- model_class

  expect_failure(expect_s3_class(model, NA))
  expect_failure(expect_s3_class(model, "beaver_negbin_linear"))
  expect_s3_class(model, "xyz")

  expect_no_error(get_var_names.beaver_negbin_linear(model = model))
})

test_that("get_var_names.beaver_negbin_loglinear works against an S3 object of class beaver_negbin_loglinear, produces an object with correct properties", { # nolint

  model_class <- c("beaver_negbin_loglinear")

  model <- list()
  class(model) <- model_class

  expect_failure(expect_s3_class(model, NA))
  expect_s3_class(model, "beaver_negbin_loglinear")

  expect_no_error(get_var_names.beaver_negbin_loglinear(model = model))

  var_names <- get_var_names.beaver_negbin_loglinear(model = model)

  expect_true(is.character(var_names))
  expect_gte(length(var_names), 3L)
})

test_that("get_var_names works identically to get_var_names.beaver_negbin_loglinear", { # nolint

  model_class <- c("beaver_negbin_loglinear")

  model <- list()
  class(model) <- model_class

  expect_failure(expect_s3_class(model, NA))
  expect_s3_class(model, "beaver_negbin_loglinear")

  expect_no_error(get_var_names(model = model))
  expect_identical(
    get_var_names(model = model),
    get_var_names.beaver_negbin_loglinear(model = model)
  )
})

test_that("get_var_names.beaver_negbin_loglinear works against an S3 object of class xyz (but not of class beaver_negbin_loglinear)", { # nolint

  model_class <- c("xyz")

  model <- list()
  class(model) <- model_class

  expect_failure(expect_s3_class(model, NA))
  expect_failure(expect_s3_class(model, "beaver_negbin_loglinear"))
  expect_s3_class(model, "xyz")

  expect_no_error(get_var_names.beaver_negbin_loglinear(model = model))
})

test_that("get_var_names.beaver_negbin_quad works against an S3 object of class beaver_negbin_quad, produces an object with correct properties", { # nolint

  model_class <- c("beaver_negbin_quad")

  model <- list()
  class(model) <- model_class

  expect_failure(expect_s3_class(model, NA))
  expect_s3_class(model, "beaver_negbin_quad")

  expect_no_error(get_var_names.beaver_negbin_quad(model = model))

  var_names <- get_var_names.beaver_negbin_quad(model = model)

  expect_true(is.character(var_names))
  expect_gte(length(var_names), 3L)
})

test_that("get_var_names works identically to get_var_names.beaver_negbin_quad", { # nolint

  model_class <- c("beaver_negbin_quad")

  model <- list()
  class(model) <- model_class

  expect_failure(expect_s3_class(model, NA))
  expect_s3_class(model, "beaver_negbin_quad")

  expect_no_error(get_var_names(model = model))
  expect_identical(
    get_var_names(model = model),
    get_var_names.beaver_negbin_quad(model = model)
  )
})

test_that("get_var_names.beaver_negbin_quad works against an S3 object of class xyz (but not of class beaver_negbin_quad)", { # nolint

  model_class <- c("xyz")

  model <- list()
  class(model) <- model_class

  expect_failure(expect_s3_class(model, NA))
  expect_failure(expect_s3_class(model, "beaver_negbin_quad"))
  expect_s3_class(model, "xyz")

  expect_no_error(get_var_names.beaver_negbin_quad(model = model))
})

test_that("get_var_names.beaver_negbin_logquad works against an S3 object of class beaver_negbin_logquad, produces an object with correct properties", { # nolint

  model_class <- c("beaver_negbin_logquad")

  model <- list()
  class(model) <- model_class

  expect_failure(expect_s3_class(model, NA))
  expect_s3_class(model, "beaver_negbin_logquad")

  expect_no_error(get_var_names.beaver_negbin_logquad(model = model))

  var_names <- get_var_names.beaver_negbin_logquad(model = model)

  expect_true(is.character(var_names))
  expect_gte(length(var_names), 3L)
})

test_that("get_var_names works identically to get_var_names.beaver_negbin_logquad", { # nolint

  model_class <- c("beaver_negbin_logquad")

  model <- list()
  class(model) <- model_class

  expect_failure(expect_s3_class(model, NA))
  expect_s3_class(model, "beaver_negbin_logquad")

  expect_no_error(get_var_names(model = model))
  expect_identical(
    get_var_names(model = model),
    get_var_names.beaver_negbin_logquad(model = model)
  )
})

test_that("get_var_names.beaver_negbin_logquad works against an S3 object of class xyz (but not of class beaver_negbin_logquad)", { # nolint

  model_class <- c("xyz")

  model <- list()
  class(model) <- model_class

  expect_failure(expect_s3_class(model, NA))
  expect_failure(expect_s3_class(model, "beaver_negbin_logquad"))
  expect_s3_class(model, "xyz")

  expect_no_error(get_var_names.beaver_negbin_logquad(model = model))
})

test_that("get_var_names.beaver_negbin_exp works against an S3 object of class beaver_negbin_exp, produces an object with correct properties", { # nolint

  model_class <- c("beaver_negbin_exp")

  model <- list()
  class(model) <- model_class

  expect_failure(expect_s3_class(model, NA))
  expect_s3_class(model, "beaver_negbin_exp")

  expect_no_error(get_var_names.beaver_negbin_exp(model = model))

  var_names <- get_var_names.beaver_negbin_exp(model = model)

  expect_true(is.character(var_names))
  expect_gte(length(var_names), 3L)
})

test_that("get_var_names works identically to get_var_names.beaver_negbin_exp", { # nolint

  model_class <- c("beaver_negbin_exp")

  model <- list()
  class(model) <- model_class

  expect_failure(expect_s3_class(model, NA))
  expect_s3_class(model, "beaver_negbin_exp")

  expect_no_error(get_var_names(model = model))
  expect_identical(
    get_var_names(model = model),
    get_var_names.beaver_negbin_exp(model = model)
  )
})

test_that("get_var_names.beaver_negbin_exp works against an S3 object of class xyz (but not of class beaver_negbin_exp)", { # nolint

  model_class <- c("xyz")

  model <- list()
  class(model) <- model_class

  expect_failure(expect_s3_class(model, NA))
  expect_failure(expect_s3_class(model, "beaver_negbin_exp"))
  expect_s3_class(model, "xyz")

  expect_no_error(get_var_names.beaver_negbin_exp(model = model))
})

test_that("get_var_names does not work against an S3 object of class xyz (but not of class beaver_negbin_METHOD)", { # nolint

  model_class <- c("xyz")

  model <- list()
  class(model) <- model_class

  expect_failure(expect_s3_class(model, NA))
  expect_false(any(class(model) %in% paste0(
    "beaver_negbin_",
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
  expect_s3_class(model, "xyz")

  expect_error(get_var_names(model = model))
})
