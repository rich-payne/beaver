
test_that("add_common_attrs works against an S3 object of class mcmc.list, produces an object with correct properties", { # nolint

  nb_monotone_incr <- readRDS(test_path("fixtures", "nb_monotone_incr.rds"))

  expect_failure(expect_s3_class(nb_monotone_incr, NA))
  expect_s3_class(nb_monotone_incr, "data.frame")

  load(test_path("fixtures", "nb_indep_mcmc+_objects.Rdata"))

  expect_failure(expect_s3_class(nb_indep_model_samples, NA))
  expect_s3_class(nb_indep_model_samples, "mcmc.list")

  formula <- ~ 1
  nb_monotone_incr_x <- model.matrix(formula, data = nb_monotone_incr)

  expect_no_error(
    add_common_attrs(
      samples = nb_indep_model_samples,
      data = nb_monotone_incr,
      x = nb_monotone_incr_x,
      formula = formula
    )
  )

  samples <- add_common_attrs(
    samples = nb_indep_model_samples,
    data = nb_monotone_incr,
    x = nb_monotone_incr_x,
    formula = formula
  )

  expect_failure(expect_s3_class(samples, NA))
  expect_s3_class(samples, "mcmc.list")
  expect_identical(
    names(attributes(samples)),
    c("class", "doses", "n_b1", "covariate_names", "formula")
  )
})

test_that("add_class_and_attrs.beaver_negbin_indep works against an S3 object of class mcmc.list, produces an object with correct properties", { # nolint

  nb_monotone_incr <- readRDS(test_path("fixtures", "nb_monotone_incr.rds"))

  expect_failure(expect_s3_class(nb_monotone_incr, NA))
  expect_s3_class(nb_monotone_incr, "data.frame")

  load(test_path("fixtures", "nb_indep_mcmc+_objects.Rdata"))

  expect_failure(expect_s3_class(nb_indep_model_samples, NA))
  expect_s3_class(nb_indep_model_samples, "mcmc.list")

  formula <- ~ 1
  nb_monotone_incr_x <- model.matrix(formula, data = nb_monotone_incr)

  expect_no_error(
    add_class_and_attrs.beaver_negbin_indep(
      samples = nb_indep_model_samples,
      data = nb_monotone_incr,
      x = nb_monotone_incr_x,
      formula = formula
    )
  )

  samples <- add_class_and_attrs.beaver_negbin_indep(
    samples = nb_indep_model_samples,
    data = nb_monotone_incr,
    x = nb_monotone_incr_x,
    formula = formula
  )

  expect_failure(expect_s3_class(samples, NA))
  expect_s3_class(samples, "mcmc.list")
  expect_identical(
    names(attributes(samples)),
    c("class", "doses", "n_b1", "covariate_names", "formula")
  )
  expect_identical(
    class(samples),
    c("beaver_mcmc_negbin_indep", "beaver_mcmc", "mcmc.list")
  )
})

test_that("add_class_and_attrs works identically to add_class_and_attrs.beaver_negbin_indep", { # nolint

  nb_monotone_incr <- readRDS(test_path("fixtures", "nb_monotone_incr.rds"))

  expect_failure(expect_s3_class(nb_monotone_incr, NA))
  expect_s3_class(nb_monotone_incr, "data.frame")

  load(test_path("fixtures", "nb_indep_mcmc+_objects.Rdata"))

  expect_failure(expect_s3_class(nb_indep_model_samples, NA))
  expect_s3_class(nb_indep_model_samples, "mcmc.list")

  formula <- ~ 1
  nb_monotone_incr_x <- model.matrix(formula, data = nb_monotone_incr)

  model_class <- c("beaver_negbin_indep")

  model <- list()
  class(model) <- model_class

  expect_no_error(
    add_class_and_attrs(
      model = model,
      samples = nb_indep_model_samples,
      data = nb_monotone_incr,
      x = nb_monotone_incr_x,
      formula = formula
    )
  )

  expect_identical(
    add_class_and_attrs(
      model = model,
      samples = nb_indep_model_samples,
      data = nb_monotone_incr,
      x = nb_monotone_incr_x,
      formula = formula
    ),
    add_class_and_attrs.beaver_negbin_indep(
      samples = nb_indep_model_samples,
      data = nb_monotone_incr,
      x = nb_monotone_incr_x,
      formula = formula
    )
  )
})

test_that("add_class_and_attrs.beaver_negbin_emax works against an S3 object of class mcmc.list, produces an object with correct properties", { # nolint

  nb_monotone_incr <- readRDS(test_path("fixtures", "nb_monotone_incr.rds"))

  expect_failure(expect_s3_class(nb_monotone_incr, NA))
  expect_s3_class(nb_monotone_incr, "data.frame")

  load(test_path("fixtures", "nb_emax_mcmc+_objects.Rdata"))

  expect_failure(expect_s3_class(nb_emax_model_samples, NA))
  expect_s3_class(nb_emax_model_samples, "mcmc.list")

  formula <- ~ 1
  nb_monotone_incr_x <- model.matrix(formula, data = nb_monotone_incr)

  expect_no_error(
    add_class_and_attrs.beaver_negbin_emax(
      samples = nb_emax_model_samples,
      data = nb_monotone_incr,
      x = nb_monotone_incr_x,
      formula = formula
    )
  )

  samples <- add_class_and_attrs.beaver_negbin_emax(
    samples = nb_emax_model_samples,
    data = nb_monotone_incr,
    x = nb_monotone_incr_x,
    formula = formula
  )

  expect_failure(expect_s3_class(samples, NA))
  expect_s3_class(samples, "mcmc.list")
  expect_identical(
    names(attributes(samples)),
    c("class", "doses", "n_b1", "covariate_names", "formula")
  )
  expect_identical(
    class(samples),
    c("beaver_mcmc_negbin_emax", "beaver_mcmc", "mcmc.list")
  )
})

test_that("add_class_and_attrs works identically to add_class_and_attrs.beaver_negbin_emax", { # nolint

  nb_monotone_incr <- readRDS(test_path("fixtures", "nb_monotone_incr.rds"))

  expect_failure(expect_s3_class(nb_monotone_incr, NA))
  expect_s3_class(nb_monotone_incr, "data.frame")

  load(test_path("fixtures", "nb_emax_mcmc+_objects.Rdata"))

  expect_failure(expect_s3_class(nb_emax_model_samples, NA))
  expect_s3_class(nb_emax_model_samples, "mcmc.list")

  formula <- ~ 1
  nb_monotone_incr_x <- model.matrix(formula, data = nb_monotone_incr)

  model_class <- c("beaver_negbin_emax")

  model <- list()
  class(model) <- model_class

  expect_no_error(
    add_class_and_attrs(
      model = model,
      samples = nb_emax_model_samples,
      data = nb_monotone_incr,
      x = nb_monotone_incr_x,
      formula = formula
    )
  )

  expect_identical(
    add_class_and_attrs(
      model = model,
      samples = nb_emax_model_samples,
      data = nb_monotone_incr,
      x = nb_monotone_incr_x,
      formula = formula
    ),
    add_class_and_attrs.beaver_negbin_emax(
      samples = nb_emax_model_samples,
      data = nb_monotone_incr,
      x = nb_monotone_incr_x,
      formula = formula
    )
  )
})

test_that("add_class_and_attrs.beaver_negbin_sigmoid_emax works against an S3 object of class mcmc.list, produces an object with correct properties", { # nolint

  nb_monotone_incr <- readRDS(test_path("fixtures", "nb_monotone_incr.rds"))

  expect_failure(expect_s3_class(nb_monotone_incr, NA))
  expect_s3_class(nb_monotone_incr, "data.frame")

  load(test_path("fixtures", "nb_sigmoid_emax_mcmc+_objects.Rdata"))

  expect_failure(expect_s3_class(nb_sigmoid_emax_model_samples, NA))
  expect_s3_class(nb_sigmoid_emax_model_samples, "mcmc.list")

  formula <- ~ 1
  nb_monotone_incr_x <- model.matrix(formula, data = nb_monotone_incr)

  expect_no_error(
    add_class_and_attrs.beaver_negbin_sigmoid_emax(
      samples = nb_sigmoid_emax_model_samples,
      data = nb_monotone_incr,
      x = nb_monotone_incr_x,
      formula = formula
    )
  )

  samples <- add_class_and_attrs.beaver_negbin_sigmoid_emax(
    samples = nb_sigmoid_emax_model_samples,
    data = nb_monotone_incr,
    x = nb_monotone_incr_x,
    formula = formula
  )

  expect_failure(expect_s3_class(samples, NA))
  expect_s3_class(samples, "mcmc.list")
  expect_identical(
    names(attributes(samples)),
    c("class", "doses", "n_b1", "covariate_names", "formula")
  )
  expect_identical(
    class(samples),
    c("beaver_mcmc_negbin_sigmoid_emax", "beaver_mcmc", "mcmc.list")
  )
})

test_that("add_class_and_attrs works identically to add_class_and_attrs.beaver_negbin_sigmoid_emax", { # nolint

  nb_monotone_incr <- readRDS(test_path("fixtures", "nb_monotone_incr.rds"))

  expect_failure(expect_s3_class(nb_monotone_incr, NA))
  expect_s3_class(nb_monotone_incr, "data.frame")

  load(test_path("fixtures", "nb_sigmoid_emax_mcmc+_objects.Rdata"))

  expect_failure(expect_s3_class(nb_sigmoid_emax_model_samples, NA))
  expect_s3_class(nb_sigmoid_emax_model_samples, "mcmc.list")

  formula <- ~ 1
  nb_monotone_incr_x <- model.matrix(formula, data = nb_monotone_incr)

  model_class <- c("beaver_negbin_sigmoid_emax")

  model <- list()
  class(model) <- model_class

  expect_no_error(
    add_class_and_attrs(
      model = model,
      samples = nb_sigmoid_emax_model_samples,
      data = nb_monotone_incr,
      x = nb_monotone_incr_x,
      formula = formula
    )
  )

  expect_identical(
    add_class_and_attrs(
      model = model,
      samples = nb_sigmoid_emax_model_samples,
      data = nb_monotone_incr,
      x = nb_monotone_incr_x,
      formula = formula
    ),
    add_class_and_attrs.beaver_negbin_sigmoid_emax(
      samples = nb_sigmoid_emax_model_samples,
      data = nb_monotone_incr,
      x = nb_monotone_incr_x,
      formula = formula
    )
  )
})

test_that("add_class_and_attrs.beaver_negbin_linear works against an S3 object of class mcmc.list, produces an object with correct properties", { # nolint

  nb_monotone_incr <- readRDS(test_path("fixtures", "nb_monotone_incr.rds"))

  expect_failure(expect_s3_class(nb_monotone_incr, NA))
  expect_s3_class(nb_monotone_incr, "data.frame")

  load(test_path("fixtures", "nb_linear_mcmc+_objects.Rdata"))

  expect_failure(expect_s3_class(nb_linear_model_samples, NA))
  expect_s3_class(nb_linear_model_samples, "mcmc.list")

  formula <- ~ 1
  nb_monotone_incr_x <- model.matrix(formula, data = nb_monotone_incr)

  expect_no_error(
    add_class_and_attrs.beaver_negbin_linear(
      samples = nb_linear_model_samples,
      data = nb_monotone_incr,
      x = nb_monotone_incr_x,
      formula = formula
    )
  )

  samples <- add_class_and_attrs.beaver_negbin_linear(
    samples = nb_linear_model_samples,
    data = nb_monotone_incr,
    x = nb_monotone_incr_x,
    formula = formula
  )

  expect_failure(expect_s3_class(samples, NA))
  expect_s3_class(samples, "mcmc.list")
  expect_identical(
    names(attributes(samples)),
    c("class", "doses", "n_b1", "covariate_names", "formula")
  )
  expect_identical(
    class(samples),
    c("beaver_mcmc_negbin_linear", "beaver_mcmc", "mcmc.list")
  )
})

test_that("add_class_and_attrs works identically to add_class_and_attrs.beaver_negbin_linear", { # nolint

  nb_monotone_incr <- readRDS(test_path("fixtures", "nb_monotone_incr.rds"))

  expect_failure(expect_s3_class(nb_monotone_incr, NA))
  expect_s3_class(nb_monotone_incr, "data.frame")

  load(test_path("fixtures", "nb_linear_mcmc+_objects.Rdata"))

  expect_failure(expect_s3_class(nb_linear_model_samples, NA))
  expect_s3_class(nb_linear_model_samples, "mcmc.list")

  formula <- ~ 1
  nb_monotone_incr_x <- model.matrix(formula, data = nb_monotone_incr)

  model_class <- c("beaver_negbin_linear")

  model <- list()
  class(model) <- model_class

  expect_no_error(
    add_class_and_attrs(
      model = model,
      samples = nb_linear_model_samples,
      data = nb_monotone_incr,
      x = nb_monotone_incr_x,
      formula = formula
    )
  )

  expect_identical(
    add_class_and_attrs(
      model = model,
      samples = nb_linear_model_samples,
      data = nb_monotone_incr,
      x = nb_monotone_incr_x,
      formula = formula
    ),
    add_class_and_attrs.beaver_negbin_linear(
      samples = nb_linear_model_samples,
      data = nb_monotone_incr,
      x = nb_monotone_incr_x,
      formula = formula
    )
  )
})

test_that("add_class_and_attrs.beaver_negbin_loglinear works against an S3 object of class mcmc.list, produces an object with correct properties", { # nolint

  nb_monotone_incr <- readRDS(test_path("fixtures", "nb_monotone_incr.rds"))

  expect_failure(expect_s3_class(nb_monotone_incr, NA))
  expect_s3_class(nb_monotone_incr, "data.frame")

  load(test_path("fixtures", "nb_loglinear_mcmc+_objects.Rdata"))

  expect_failure(expect_s3_class(nb_loglinear_model_samples, NA))
  expect_s3_class(nb_loglinear_model_samples, "mcmc.list")

  formula <- ~ 1
  nb_monotone_incr_x <- model.matrix(formula, data = nb_monotone_incr)

  expect_no_error(
    add_class_and_attrs.beaver_negbin_loglinear(
      samples = nb_loglinear_model_samples,
      data = nb_monotone_incr,
      x = nb_monotone_incr_x,
      formula = formula
    )
  )

  samples <- add_class_and_attrs.beaver_negbin_loglinear(
    samples = nb_loglinear_model_samples,
    data = nb_monotone_incr,
    x = nb_monotone_incr_x,
    formula = formula
  )

  expect_failure(expect_s3_class(samples, NA))
  expect_s3_class(samples, "mcmc.list")
  expect_identical(
    names(attributes(samples)),
    c("class", "doses", "n_b1", "covariate_names", "formula")
  )
  expect_identical(
    class(samples),
    c("beaver_mcmc_negbin_loglinear", "beaver_mcmc", "mcmc.list")
  )
})

test_that("add_class_and_attrs works identically to add_class_and_attrs.beaver_negbin_loglinear", { # nolint

  nb_monotone_incr <- readRDS(test_path("fixtures", "nb_monotone_incr.rds"))

  expect_failure(expect_s3_class(nb_monotone_incr, NA))
  expect_s3_class(nb_monotone_incr, "data.frame")

  load(test_path("fixtures", "nb_loglinear_mcmc+_objects.Rdata"))

  expect_failure(expect_s3_class(nb_loglinear_model_samples, NA))
  expect_s3_class(nb_loglinear_model_samples, "mcmc.list")

  formula <- ~ 1
  nb_monotone_incr_x <- model.matrix(formula, data = nb_monotone_incr)

  model_class <- c("beaver_negbin_loglinear")

  model <- list()
  class(model) <- model_class

  expect_no_error(
    add_class_and_attrs(
      model = model,
      samples = nb_loglinear_model_samples,
      data = nb_monotone_incr,
      x = nb_monotone_incr_x,
      formula = formula
    )
  )

  expect_identical(
    add_class_and_attrs(
      model = model,
      samples = nb_loglinear_model_samples,
      data = nb_monotone_incr,
      x = nb_monotone_incr_x,
      formula = formula
    ),
    add_class_and_attrs.beaver_negbin_loglinear(
      samples = nb_loglinear_model_samples,
      data = nb_monotone_incr,
      x = nb_monotone_incr_x,
      formula = formula
    )
  )
})

test_that("add_class_and_attrs.beaver_negbin_quad works against an S3 object of class mcmc.list, produces an object with correct properties", { # nolint

  nb_monotone_incr <- readRDS(test_path("fixtures", "nb_monotone_incr.rds"))

  expect_failure(expect_s3_class(nb_monotone_incr, NA))
  expect_s3_class(nb_monotone_incr, "data.frame")

  load(test_path("fixtures", "nb_quad_mcmc+_objects.Rdata"))

  expect_failure(expect_s3_class(nb_quad_model_samples, NA))
  expect_s3_class(nb_quad_model_samples, "mcmc.list")

  formula <- ~ 1
  nb_monotone_incr_x <- model.matrix(formula, data = nb_monotone_incr)

  expect_no_error(
    add_class_and_attrs.beaver_negbin_quad(
      samples = nb_quad_model_samples,
      data = nb_monotone_incr,
      x = nb_monotone_incr_x,
      formula = formula
    )
  )

  samples <- add_class_and_attrs.beaver_negbin_quad(
    samples = nb_quad_model_samples,
    data = nb_monotone_incr,
    x = nb_monotone_incr_x,
    formula = formula
  )

  expect_failure(expect_s3_class(samples, NA))
  expect_s3_class(samples, "mcmc.list")
  expect_identical(
    names(attributes(samples)),
    c("class", "doses", "n_b1", "covariate_names", "formula")
  )
  expect_identical(
    class(samples),
    c("beaver_mcmc_negbin_quad", "beaver_mcmc", "mcmc.list")
  )
})

test_that("add_class_and_attrs works identically to add_class_and_attrs.beaver_negbin_quad", { # nolint

  nb_monotone_incr <- readRDS(test_path("fixtures", "nb_monotone_incr.rds"))

  expect_failure(expect_s3_class(nb_monotone_incr, NA))
  expect_s3_class(nb_monotone_incr, "data.frame")

  load(test_path("fixtures", "nb_quad_mcmc+_objects.Rdata"))

  expect_failure(expect_s3_class(nb_quad_model_samples, NA))
  expect_s3_class(nb_quad_model_samples, "mcmc.list")

  formula <- ~ 1
  nb_monotone_incr_x <- model.matrix(formula, data = nb_monotone_incr)

  model_class <- c("beaver_negbin_quad")

  model <- list()
  class(model) <- model_class

  expect_no_error(
    add_class_and_attrs(
      model = model,
      samples = nb_quad_model_samples,
      data = nb_monotone_incr,
      x = nb_monotone_incr_x,
      formula = formula
    )
  )

  expect_identical(
    add_class_and_attrs(
      model = model,
      samples = nb_quad_model_samples,
      data = nb_monotone_incr,
      x = nb_monotone_incr_x,
      formula = formula
    ),
    add_class_and_attrs.beaver_negbin_quad(
      samples = nb_quad_model_samples,
      data = nb_monotone_incr,
      x = nb_monotone_incr_x,
      formula = formula
    )
  )
})

test_that("add_class_and_attrs.beaver_negbin_logquad works against an S3 object of class mcmc.list, produces an object with correct properties", { # nolint

  nb_monotone_incr <- readRDS(test_path("fixtures", "nb_monotone_incr.rds"))

  expect_failure(expect_s3_class(nb_monotone_incr, NA))
  expect_s3_class(nb_monotone_incr, "data.frame")

  load(test_path("fixtures", "nb_logquad_mcmc+_objects.Rdata"))

  expect_failure(expect_s3_class(nb_logquad_model_samples, NA))
  expect_s3_class(nb_logquad_model_samples, "mcmc.list")

  formula <- ~ 1
  nb_monotone_incr_x <- model.matrix(formula, data = nb_monotone_incr)

  expect_no_error(
    add_class_and_attrs.beaver_negbin_logquad(
      samples = nb_logquad_model_samples,
      data = nb_monotone_incr,
      x = nb_monotone_incr_x,
      formula = formula
    )
  )

  samples <- add_class_and_attrs.beaver_negbin_logquad(
    samples = nb_logquad_model_samples,
    data = nb_monotone_incr,
    x = nb_monotone_incr_x,
    formula = formula
  )

  expect_failure(expect_s3_class(samples, NA))
  expect_s3_class(samples, "mcmc.list")
  expect_identical(
    names(attributes(samples)),
    c("class", "doses", "n_b1", "covariate_names", "formula")
  )
  expect_identical(
    class(samples),
    c("beaver_mcmc_negbin_logquad", "beaver_mcmc", "mcmc.list")
  )
})

test_that("add_class_and_attrs works identically to add_class_and_attrs.beaver_negbin_logquad", { # nolint

  nb_monotone_incr <- readRDS(test_path("fixtures", "nb_monotone_incr.rds"))

  expect_failure(expect_s3_class(nb_monotone_incr, NA))
  expect_s3_class(nb_monotone_incr, "data.frame")

  load(test_path("fixtures", "nb_logquad_mcmc+_objects.Rdata"))

  expect_failure(expect_s3_class(nb_logquad_model_samples, NA))
  expect_s3_class(nb_logquad_model_samples, "mcmc.list")

  formula <- ~ 1
  nb_monotone_incr_x <- model.matrix(formula, data = nb_monotone_incr)

  model_class <- c("beaver_negbin_logquad")

  model <- list()
  class(model) <- model_class

  expect_no_error(
    add_class_and_attrs(
      model = model,
      samples = nb_logquad_model_samples,
      data = nb_monotone_incr,
      x = nb_monotone_incr_x,
      formula = formula
    )
  )

  expect_identical(
    add_class_and_attrs(
      model = model,
      samples = nb_logquad_model_samples,
      data = nb_monotone_incr,
      x = nb_monotone_incr_x,
      formula = formula
    ),
    add_class_and_attrs.beaver_negbin_logquad(
      samples = nb_logquad_model_samples,
      data = nb_monotone_incr,
      x = nb_monotone_incr_x,
      formula = formula
    )
  )
})

test_that("add_class_and_attrs.beaver_negbin_exp works against an S3 object of class mcmc.list, produces an object with correct properties", { # nolint

  nb_monotone_incr <- readRDS(test_path("fixtures", "nb_monotone_incr.rds"))

  expect_failure(expect_s3_class(nb_monotone_incr, NA))
  expect_s3_class(nb_monotone_incr, "data.frame")

  load(test_path("fixtures", "nb_exp_mcmc+_objects.Rdata"))

  expect_failure(expect_s3_class(nb_exp_model_samples, NA))
  expect_s3_class(nb_exp_model_samples, "mcmc.list")

  formula <- ~ 1
  nb_monotone_incr_x <- model.matrix(formula, data = nb_monotone_incr)

  expect_no_error(
    add_class_and_attrs.beaver_negbin_exp(
      samples = nb_exp_model_samples,
      data = nb_monotone_incr,
      x = nb_monotone_incr_x,
      formula = formula
    )
  )

  samples <- add_class_and_attrs.beaver_negbin_exp(
    samples = nb_exp_model_samples,
    data = nb_monotone_incr,
    x = nb_monotone_incr_x,
    formula = formula
  )

  expect_failure(expect_s3_class(samples, NA))
  expect_s3_class(samples, "mcmc.list")
  expect_identical(
    names(attributes(samples)),
    c("class", "doses", "n_b1", "covariate_names", "formula")
  )
  expect_identical(
    class(samples),
    c("beaver_mcmc_negbin_exp", "beaver_mcmc", "mcmc.list")
  )
})

test_that("add_class_and_attrs works identically to add_class_and_attrs.beaver_negbin_exp", { # nolint

  nb_monotone_incr <- readRDS(test_path("fixtures", "nb_monotone_incr.rds"))

  expect_failure(expect_s3_class(nb_monotone_incr, NA))
  expect_s3_class(nb_monotone_incr, "data.frame")

  load(test_path("fixtures", "nb_exp_mcmc+_objects.Rdata"))

  expect_failure(expect_s3_class(nb_exp_model_samples, NA))
  expect_s3_class(nb_exp_model_samples, "mcmc.list")

  formula <- ~ 1
  nb_monotone_incr_x <- model.matrix(formula, data = nb_monotone_incr)

  model_class <- c("beaver_negbin_exp")

  model <- list()
  class(model) <- model_class

  expect_no_error(
    add_class_and_attrs(
      model = model,
      samples = nb_exp_model_samples,
      data = nb_monotone_incr,
      x = nb_monotone_incr_x,
      formula = formula
    )
  )

  expect_identical(
    add_class_and_attrs(
      model = model,
      samples = nb_exp_model_samples,
      data = nb_monotone_incr,
      x = nb_monotone_incr_x,
      formula = formula
    ),
    add_class_and_attrs.beaver_negbin_exp(
      samples = nb_exp_model_samples,
      data = nb_monotone_incr,
      x = nb_monotone_incr_x,
      formula = formula
    )
  )
})
