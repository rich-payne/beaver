
test_that("model_negbin_indep produces an S3 list object with correct properties", { # nolint

  function_args <- formalArgs(model_negbin_indep)

  expect_no_error(
    do.call(
      what = "model_negbin_indep",
      args = purrr::map(rlang::set_names(function_args), ~ 1)
    )
  )

  model <- do.call(
    what = "model_negbin_indep",
    args = purrr::map(rlang::set_names(function_args), ~ 1)
  )

  model_checks(
    model = model,
    model_class = c("beaver_negbin_indep", "beaver_model"),
    function_args = function_args
  )

  expect_identical(
    do.call(
      what = "model_negbin_indep",
      args = purrr::map(
        rlang::set_names(function_args[!function_args == "w_prior"]),
        ~ 1
      )
    ),
    model
  )
})

test_that("model_negbin_emax produces an S3 list object with correct properties", { # nolint

  function_args <- formalArgs(model_negbin_emax)

  expect_no_error(
    do.call(
      what = "model_negbin_emax",
      args = purrr::map(rlang::set_names(function_args), ~ 1)
    )
  )

  model <- do.call(
    what = "model_negbin_emax",
    args = purrr::map(rlang::set_names(function_args), ~ 1)
  )

  model_checks(
    model = model,
    model_class = c("beaver_negbin_emax", "beaver_model"),
    function_args = function_args
  )

  expect_identical(
    do.call(
      what = "model_negbin_emax",
      args = purrr::map(
        rlang::set_names(function_args[!function_args == "w_prior"]),
        ~ 1
      )
    ),
    model
  )
})

test_that("model_negbin_sigmoid_emax produces an S3 list object with correct properties", { # nolint

  function_args <- formalArgs(model_negbin_sigmoid_emax)

  expect_no_error(
    do.call(
      what = "model_negbin_sigmoid_emax",
      args = purrr::map(rlang::set_names(function_args), ~ 1)
    )
  )

  model <- do.call(
    what = "model_negbin_sigmoid_emax",
    args = purrr::map(rlang::set_names(function_args), ~ 1)
  )

  model_checks(
    model = model,
    model_class = c("beaver_negbin_sigmoid_emax", "beaver_model"),
    function_args = function_args
  )

  expect_identical(
    do.call(
      what = "model_negbin_sigmoid_emax",
      args = purrr::map(
        rlang::set_names(function_args[!function_args == "w_prior"]),
        ~ 1
      )
    ),
    model
  )
})

test_that("model_negbin_linear produces an S3 list object with correct properties", { # nolint

  function_args <- formalArgs(model_negbin_linear)

  expect_no_error(
    do.call(
      what = "model_negbin_linear",
      args = purrr::map(rlang::set_names(function_args), ~ 1)
    )
  )

  model <- do.call(
    what = "model_negbin_linear",
    args = purrr::map(rlang::set_names(function_args), ~ 1)
  )

  model_checks(
    model = model,
    model_class = c("beaver_negbin_linear", "beaver_model"),
    function_args = function_args
  )

  expect_identical(
    do.call(
      what = "model_negbin_linear",
      args = purrr::map(
        rlang::set_names(function_args[!function_args == "w_prior"]),
        ~ 1
      )
    ),
    model
  )
})

test_that("model_negbin_loglinear produces an S3 list object with correct properties", { # nolint

  function_args <- formalArgs(model_negbin_loglinear)

  expect_no_error(
    do.call(
      what = "model_negbin_loglinear",
      args = purrr::map(rlang::set_names(function_args), ~ 1)
    )
  )

  model <- do.call(
    what = "model_negbin_loglinear",
    args = purrr::map(rlang::set_names(function_args), ~ 1)
  )

  model_checks(
    model = model,
    model_class = c("beaver_negbin_loglinear", "beaver_model"),
    function_args = function_args
  )

  expect_identical(
    do.call(
      what = "model_negbin_loglinear",
      args = purrr::map(
        rlang::set_names(function_args[!function_args == "w_prior"]),
        ~ 1
      )
    ),
    model
  )
})

test_that("model_negbin_quad produces an S3 list object with correct properties", { # nolint

  function_args <- formalArgs(model_negbin_quad)

  expect_no_error(
    do.call(
      what = "model_negbin_quad",
      args = purrr::map(rlang::set_names(function_args), ~ 1)
    )
  )

  model <- do.call(
    what = "model_negbin_quad",
    args = purrr::map(rlang::set_names(function_args), ~ 1)
  )

  model_checks(
    model = model,
    model_class = c("beaver_negbin_quad", "beaver_model"),
    function_args = function_args
  )

  expect_identical(
    do.call(
      what = "model_negbin_quad",
      args = purrr::map(
        rlang::set_names(function_args[!function_args == "w_prior"]),
        ~ 1
      )
    ),
    model
  )
})

test_that("model_negbin_logquad produces an S3 list object with correct properties", { # nolint

  function_args <- formalArgs(model_negbin_logquad)

  expect_no_error(
    do.call(
      what = "model_negbin_logquad",
      args = purrr::map(rlang::set_names(function_args), ~ 1)
    )
  )

  model <- do.call(
    what = "model_negbin_logquad",
    args = purrr::map(rlang::set_names(function_args), ~ 1)
  )

  model_checks(
    model = model,
    model_class = c("beaver_negbin_logquad", "beaver_model"),
    function_args = function_args
  )

  expect_identical(
    do.call(
      what = "model_negbin_logquad",
      args = purrr::map(
        rlang::set_names(function_args[!function_args == "w_prior"]),
        ~ 1
      )
    ),
    model
  )
})

test_that("model_negbin_exp produces an S3 list object with correct properties", { # nolint

  function_args <- formalArgs(model_negbin_exp)

  expect_no_error(
    do.call(
      what = "model_negbin_exp",
      args = purrr::map(rlang::set_names(function_args), ~ 1)
    )
  )

  model <- do.call(
    what = "model_negbin_exp",
    args = purrr::map(rlang::set_names(function_args), ~ 1)
  )

  model_checks(
    model = model,
    model_class = c("beaver_negbin_exp", "beaver_model"),
    function_args = function_args
  )

  expect_identical(
    do.call(
      what = "model_negbin_exp",
      args = purrr::map(
        rlang::set_names(function_args[!function_args == "w_prior"]),
        ~ 1
      )
    ),
    model
  )
})
