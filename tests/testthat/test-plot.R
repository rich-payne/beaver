
test_that("plot.beaver_mcmc works against an S3 object of class beaver_mcmc_bma, produces an object with correct properties", { # nolint

  nb_monotone_incr <- readRDS(test_path("fixtures", "nb_monotone_incr.rds"))

  expect_failure(expect_s3_class(nb_monotone_incr, NA))
  expect_s3_class(nb_monotone_incr, "data.frame")

  load(test_path("fixtures", "nb_bma_objects.Rdata"))

  expect_failure(expect_s3_class(nb_bma, NA))
  expect_s3_class(
    nb_bma,
    c("beaver_mcmc_bma", "yodel_bma", "beaver_mcmc"),
    exact = TRUE
  )

  #new_data----

  #>reference_dose == NULL----

  plot1 <- plot.beaver_mcmc(
    x = nb_bma,
    doses = attr(nb_bma, "doses"),
    prob = c(.025, .975),
    new_data = nb_monotone_incr[1, ],
  )

  expect_failure(expect_s3_class(plot1, NA))
  expect_s3_class(plot1, "ggplot")

  #>reference_dose == [first dose], reference_type == "difference"----

  plot1a <- plot.beaver_mcmc(
    x = nb_bma,
    doses = attr(nb_bma, "doses"),
    prob = c(.025, .975),
    new_data = nb_monotone_incr[1, ],
    reference_dose = attr(nb_bma, "doses")[1],
    reference_type = "difference"
  )

  expect_failure(expect_s3_class(plot1a, NA))
  expect_s3_class(plot1a, "ggplot")

  #>reference_dose == [first dose], reference_type == "ratio"----

  plot1b <- plot.beaver_mcmc(
    x = nb_bma,
    doses = attr(nb_bma, "doses"),
    prob = c(.025, .975),
    new_data = nb_monotone_incr[1, ],
    reference_dose = attr(nb_bma, "doses")[1],
    reference_type = "ratio"
  )

  expect_failure(expect_s3_class(plot1b, NA))
  expect_s3_class(plot1b, "ggplot")

  #contrast----

  #>reference_dose == NULL----

  plot2 <- plot.beaver_mcmc(
    x = nb_bma,
    doses = attr(nb_bma, "doses"),
    prob = c(.025, .975),
    contrast = matrix(1, 1, 1)
  )

  expect_failure(expect_s3_class(plot2, NA))
  expect_s3_class(plot2, "ggplot")

  #>reference_dose == [first dose], reference_type == "difference"----

  plot2a <- plot.beaver_mcmc(
    x = nb_bma,
    doses = attr(nb_bma, "doses"),
    prob = c(.025, .975),
    contrast = matrix(1, 1, 1),
    reference_dose = attr(nb_bma, "doses")[1],
    reference_type = "difference"
  )

  expect_failure(expect_s3_class(plot2a, NA))
  expect_s3_class(plot2a, "ggplot")

  #>reference_dose == [first dose], reference_type == "ratio"----

  plot2b <- plot.beaver_mcmc(
    x = nb_bma,
    doses = attr(nb_bma, "doses"),
    prob = c(.025, .975),
    contrast = matrix(1, 1, 1),
    reference_dose = attr(nb_bma, "doses")[1],
    reference_type = "ratio"
  )

  expect_failure(expect_s3_class(plot2b, NA))
  expect_s3_class(plot2b, "ggplot")
})

test_that("plot.beaver_mcmc works against an S3 object of class beaver_mcmc_bma, with covariates, produces an object with correct properties", { # nolint

  nb_monotone_incr_cov <- readRDS(test_path("fixtures", "nb_monotone_incr_cov.rds")) # nolint

  expect_failure(expect_s3_class(nb_monotone_incr_cov, NA))
  expect_s3_class(nb_monotone_incr_cov, "data.frame")

  load(test_path("fixtures", "nb_bma_cov_objects.Rdata"))

  expect_failure(expect_s3_class(nb_bma_cov, NA))
  expect_s3_class(
    nb_bma_cov,
    c("beaver_mcmc_bma", "yodel_bma", "beaver_mcmc"),
    exact = TRUE
  )

  #new_data----

  #>reference_dose == NULL----

  plot1 <- plot.beaver_mcmc(
    x = nb_bma_cov,
    doses = attr(nb_bma_cov, "doses"),
    prob = c(.025, .975),
    new_data = nb_monotone_incr_cov[1, ],
  )

  expect_failure(expect_s3_class(plot1, NA))
  expect_s3_class(plot1, "ggplot")

  #>reference_dose == [first dose], reference_type == "difference"----

  plot1a <- plot.beaver_mcmc(
    x = nb_bma_cov,
    doses = attr(nb_bma_cov, "doses"),
    prob = c(.025, .975),
    new_data = nb_monotone_incr_cov[1, ],
    reference_dose = attr(nb_bma_cov, "doses")[1],
    reference_type = "difference"
  )

  expect_failure(expect_s3_class(plot1a, NA))
  expect_s3_class(plot1a, "ggplot")

  #>reference_dose == [first dose], reference_type == "ratio"----

  plot1b <- plot.beaver_mcmc(
    x = nb_bma_cov,
    doses = attr(nb_bma_cov, "doses"),
    prob = c(.025, .975),
    new_data = nb_monotone_incr_cov[1, ],
    reference_dose = attr(nb_bma_cov, "doses")[1],
    reference_type = "ratio"
  )

  expect_failure(expect_s3_class(plot1b, NA))
  expect_s3_class(plot1b, "ggplot")

  #contrast----

  #>reference_dose == NULL----

  plot2 <- plot.beaver_mcmc(
    x = nb_bma_cov,
    doses = attr(nb_bma_cov, "doses"),
    prob = c(.025, .975),
    contrast = matrix(c(1, 40), 1, 2)
  )

  expect_failure(expect_s3_class(plot2, NA))
  expect_s3_class(plot2, "ggplot")

  #>reference_dose == [first dose], reference_type == "difference"----

  plot2a <- plot.beaver_mcmc(
    x = nb_bma_cov,
    doses = attr(nb_bma_cov, "doses"),
    prob = c(.025, .975),
    contrast = matrix(c(1, 40), 1, 2),
    reference_dose = attr(nb_bma_cov, "doses")[1],
    reference_type = "difference"
  )

  expect_failure(expect_s3_class(plot2a, NA))
  expect_s3_class(plot2a, "ggplot")

  #>reference_dose == [first dose], reference_type == "ratio"----

  plot2b <- plot.beaver_mcmc(
    x = nb_bma_cov,
    doses = attr(nb_bma_cov, "doses"),
    prob = c(.025, .975),
    contrast = matrix(c(1, 40), 1, 2),
    reference_dose = attr(nb_bma_cov, "doses")[1],
    reference_type = "ratio"
  )

  expect_failure(expect_s3_class(plot2b, NA))
  expect_s3_class(plot2b, "ggplot")
})

test_that("plot.beaver_mcmc works against an S3 object of class beaver_mcmc_bma, with covariates & type == \"g-comp\", produces an object with correct properties", { # nolint

  nb_monotone_incr_cov <- readRDS(test_path("fixtures", "nb_monotone_incr_cov.rds")) # nolint

  expect_failure(expect_s3_class(nb_monotone_incr_cov, NA))
  expect_s3_class(nb_monotone_incr_cov, "data.frame")

  load(test_path("fixtures", "nb_bma_cov_objects.Rdata"))

  expect_failure(expect_s3_class(nb_bma_cov, NA))
  expect_s3_class(
    nb_bma_cov,
    c("beaver_mcmc_bma", "yodel_bma", "beaver_mcmc"),
    exact = TRUE
  )

  #new_data----

  #>reference_dose == NULL----

  plot1 <- plot.beaver_mcmc(
    x = nb_bma_cov,
    doses = attr(nb_bma_cov, "doses"),
    prob = c(.025, .975),
    new_data = nb_monotone_incr_cov,
    type = "g-comp"
  )

  expect_failure(expect_s3_class(plot1, NA))
  expect_s3_class(plot1, "ggplot")

  # nolint start
  # #>reference_dose == [first dose], reference_type == "difference"----
  #
  # plot1a <- plot.beaver_mcmc(
  #   x = nb_bma_cov,
  #   doses = attr(nb_bma_cov, "doses"),
  #   prob = c(.025, .975),
  #   new_data = nb_monotone_incr_cov,
  #   type = "g-comp",
  #   reference_dose = attr(nb_bma_cov, "doses")[1],
  #   reference_type = "difference"
  # )
  #
  # expect_failure(expect_s3_class(plot1a, NA))
  # expect_s3_class(plot1a, "ggplot")
  #
  # #>reference_dose == [first dose], reference_type == "ratio"----
  #
  # plot1b <- plot.beaver_mcmc(
  #   x = nb_bma_cov,
  #   doses = attr(nb_bma_cov, "doses"),
  #   prob = c(.025, .975),
  #   new_data = nb_monotone_incr_cov,
  #   type = "g-comp",
  #   reference_dose = attr(nb_bma_cov, "doses")[1],
  #   reference_type = "ratio"
  # )
  #
  # expect_failure(expect_s3_class(plot1b, NA))
  # expect_s3_class(plot1b, "ggplot")
  #
  # #contrast----
  #
  # #>reference_dose == NULL----
  #
  # plot2 <- plot.beaver_mcmc(
  #   x = nb_bma_cov,
  #   doses = attr(nb_bma_cov, "doses"),
  #   prob = c(.025, .975),
  #   contrast = matrix(c(1, 40), 1, 2),
  #   type = "g-comp"
  # )
  #
  # expect_failure(expect_s3_class(plot2, NA))
  # expect_s3_class(plot2, "ggplot")
  #
  # #>reference_dose == [first dose], reference_type == "difference"----
  #
  # plot2a <- plot.beaver_mcmc(
  #   x = nb_bma_cov,
  #   doses = attr(nb_bma_cov, "doses"),
  #   prob = c(.025, .975),
  #   contrast = matrix(c(1, 40), 1, 2),
  #   type = "g-comp",
  #   reference_dose = attr(nb_bma_cov, "doses")[1],
  #   reference_type = "difference"
  # )
  #
  # expect_failure(expect_s3_class(plot2a, NA))
  # expect_s3_class(plot2a, "ggplot")
  #
  # #>reference_dose == [first dose], reference_type == "ratio"----
  #
  # plot2b <- plot.beaver_mcmc(
  #   x = nb_bma_cov,
  #   doses = attr(nb_bma_cov, "doses"),
  #   prob = c(.025, .975),
  #   contrast = matrix(c(1, 40), 1, 2),
  #   type = "g-comp",
  #   reference_dose = attr(nb_bma_cov, "doses")[1],
  #   reference_type = "ratio"
  # )
  #
  # expect_failure(expect_s3_class(plot2b, NA))
  # expect_s3_class(plot2b, "ggplot")
  # nolint end
})

test_that("plot works against an S3 object of class beaver_mcmc_bma", {

  skip_on_cran()

  nb_monotone_incr <- readRDS(test_path("fixtures", "nb_monotone_incr.rds"))

  expect_failure(expect_s3_class(nb_monotone_incr, NA))
  expect_s3_class(nb_monotone_incr, "data.frame")

  load(test_path("fixtures", "nb_bma_objects.Rdata"))

  expect_failure(expect_s3_class(nb_bma, NA))
  expect_s3_class(
    nb_bma,
    c("beaver_mcmc_bma", "yodel_bma", "beaver_mcmc"),
    exact = TRUE
  )

  #new_data----

  #>reference_dose == NULL----

  expect_no_error(
    plot(
      x = nb_bma,
      doses = attr(nb_bma, "doses"),
      prob = c(.025, .975),
      new_data = nb_monotone_incr[1, ]
    )
  )

  #>reference_dose == [first dose], reference_type == "difference"----

  expect_no_error(
    plot(
      x = nb_bma,
      doses = attr(nb_bma, "doses"),
      prob = c(.025, .975),
      new_data = nb_monotone_incr[1, ],
      reference_dose = attr(nb_bma, "doses")[1],
      reference_type = "difference"
    )
  )

  #>reference_dose == [first dose], reference_type == "ratio"----

  expect_no_error(
    plot(
      x = nb_bma,
      doses = attr(nb_bma, "doses"),
      prob = c(.025, .975),
      new_data = nb_monotone_incr[1, ],
      reference_dose = attr(nb_bma, "doses")[1],
      reference_type = "ratio"
    )
  )

  #contrast----

  #>reference_dose == NULL----

  expect_no_error(
    plot(
      x = nb_bma,
      doses = attr(nb_bma, "doses"),
      prob = c(.025, .975),
      contrast = matrix(1, 1, 1)
    )
  )

  #>reference_dose == [first dose], reference_type == "difference"----

  expect_no_error(
    plot(
      x = nb_bma,
      doses = attr(nb_bma, "doses"),
      prob = c(.025, .975),
      contrast = matrix(1, 1, 1),
      reference_dose = attr(nb_bma, "doses")[1],
      reference_type = "difference"
    )
  )

  #>reference_dose == [first dose], reference_type == "ratio"----

  expect_no_error(
    plot(
      x = nb_bma,
      doses = attr(nb_bma, "doses"),
      prob = c(.025, .975),
      contrast = matrix(1, 1, 1),
      reference_dose = attr(nb_bma, "doses")[1],
      reference_type = "ratio"
    )
  )
})

test_that("plot.beaver_mcmc works against an S3 object of class beaver_mcmc, produces an object with correct properties", { # nolint

  skip_on_cran()

  nb_monotone_incr <- readRDS(test_path("fixtures", "nb_monotone_incr.rds"))

  expect_failure(expect_s3_class(nb_monotone_incr, NA))
  expect_s3_class(nb_monotone_incr, "data.frame")

  load(test_path("fixtures", "nb_indep_mcmc+_objects.Rdata"))

  expect_failure(expect_s3_class(nb_indep_model_samples_updatedattr, NA))
  expect_s3_class(nb_indep_model_samples_updatedattr, "beaver_mcmc")
  expect_no_error(
    checkmate::assertSubset(
      c("covariate_names", "formula", "doses"),
      names(attributes(nb_indep_model_samples_updatedattr))
    )
  )

  #new_data----

  #>reference_dose == NULL----

  plot1 <- plot.beaver_mcmc(
    x = nb_indep_model_samples_updatedattr,
    doses = attr(nb_indep_model_samples_updatedattr, "doses"),
    prob = c(.025, .975),
    new_data = nb_monotone_incr[1, ]
  )

  expect_failure(expect_s3_class(plot1, NA))
  expect_s3_class(plot1, "ggplot")

  #>reference_dose == [first dose], reference_type == "difference"----

  plot1a <- plot.beaver_mcmc(
    x = nb_indep_model_samples_updatedattr,
    doses = attr(nb_indep_model_samples_updatedattr, "doses"),
    prob = c(.025, .975),
    new_data = nb_monotone_incr[1, ],
    reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
    reference_type = "difference"
  )

  expect_failure(expect_s3_class(plot1a, NA))
  expect_s3_class(plot1a, "ggplot")

  #>reference_dose == [first dose], reference_type == "ratio"----

  plot1b <- plot.beaver_mcmc(
    x = nb_indep_model_samples_updatedattr,
    doses = attr(nb_indep_model_samples_updatedattr, "doses"),
    prob = c(.025, .975),
    new_data = nb_monotone_incr[1, ],
    reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
    reference_type = "ratio"
  )

  expect_failure(expect_s3_class(plot1b, NA))
  expect_s3_class(plot1b, "ggplot")

  #contrast----

  #>reference_dose == NULL----

  plot2 <- plot.beaver_mcmc(
    x = nb_indep_model_samples_updatedattr,
    doses = attr(nb_indep_model_samples_updatedattr, "doses"),
    prob = c(.025, .975),
    contrast = matrix(1, 1, 1)
  )

  expect_failure(expect_s3_class(plot2, NA))
  expect_s3_class(plot2, "ggplot")

  #>reference_dose == [first dose], reference_type == "difference"----

  plot2a <- plot.beaver_mcmc(
    x = nb_indep_model_samples_updatedattr,
    doses = attr(nb_indep_model_samples_updatedattr, "doses"),
    prob = c(.025, .975),
    contrast = matrix(1, 1, 1),
    reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
    reference_type = "difference"
  )

  expect_failure(expect_s3_class(plot2a, NA))
  expect_s3_class(plot2a, "ggplot")

  #>reference_dose == [first dose], reference_type == "ratio"----

  plot2b <- plot.beaver_mcmc(
    x = nb_indep_model_samples_updatedattr,
    doses = attr(nb_indep_model_samples_updatedattr, "doses"),
    prob = c(.025, .975),
    contrast = matrix(1, 1, 1),
    reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
    reference_type = "ratio"
  )

  expect_failure(expect_s3_class(plot2b, NA))
  expect_s3_class(plot2b, "ggplot")
})

test_that("plot.beaver_mcmc works against an S3 object of class beaver_mcmc, with covariates, produces an object with correct properties", { # nolint

  skip_on_cran()

  nb_monotone_incr_cov <- readRDS(test_path("fixtures", "nb_monotone_incr_cov.rds")) # nolint

  expect_failure(expect_s3_class(nb_monotone_incr_cov, NA))
  expect_s3_class(nb_monotone_incr_cov, "data.frame")

  load(test_path("fixtures", "nb_emax_cov_mcmc+_objects.Rdata"))

  expect_failure(expect_s3_class(nb_emax_model_cov_samples_updatedattr, NA))
  expect_s3_class(nb_emax_model_cov_samples_updatedattr, "beaver_mcmc")
  expect_no_error(
    checkmate::assertSubset(
      c("covariate_names", "formula", "doses"),
      names(attributes(nb_emax_model_cov_samples_updatedattr))
    )
  )

  #new_data----

  #>reference_dose == NULL----

  plot1 <- plot.beaver_mcmc(
    x = nb_emax_model_cov_samples_updatedattr,
    doses = attr(nb_emax_model_cov_samples_updatedattr, "doses"),
    prob = c(.025, .975),
    new_data = nb_monotone_incr_cov[1, ]
  )

  expect_failure(expect_s3_class(plot1, NA))
  expect_s3_class(plot1, "ggplot")

  #>reference_dose == [first dose], reference_type == "difference"----

  plot1a <- plot.beaver_mcmc(
    x = nb_emax_model_cov_samples_updatedattr,
    doses = attr(nb_emax_model_cov_samples_updatedattr, "doses"),
    prob = c(.025, .975),
    new_data = nb_monotone_incr_cov[1, ],
    reference_dose = attr(nb_emax_model_cov_samples_updatedattr, "doses")[1],
    reference_type = "difference"
  )

  expect_failure(expect_s3_class(plot1a, NA))
  expect_s3_class(plot1a, "ggplot")

  #>reference_dose == [first dose], reference_type == "ratio"----

  plot1b <- plot.beaver_mcmc(
    x = nb_emax_model_cov_samples_updatedattr,
    doses = attr(nb_emax_model_cov_samples_updatedattr, "doses"),
    prob = c(.025, .975),
    new_data = nb_monotone_incr_cov[1, ],
    reference_dose = attr(nb_emax_model_cov_samples_updatedattr, "doses")[1],
    reference_type = "ratio"
  )

  expect_failure(expect_s3_class(plot1b, NA))
  expect_s3_class(plot1b, "ggplot")

  #contrast----

  #>reference_dose == NULL----

  plot2 <- plot.beaver_mcmc(
    x = nb_emax_model_cov_samples_updatedattr,
    doses = attr(nb_emax_model_cov_samples_updatedattr, "doses"),
    prob = c(.025, .975),
    contrast = matrix(c(1, 40), 1, 2)
  )

  expect_failure(expect_s3_class(plot2, NA))
  expect_s3_class(plot2, "ggplot")

  #>reference_dose == [first dose], reference_type == "difference"----

  plot2a <- plot.beaver_mcmc(
    x = nb_emax_model_cov_samples_updatedattr,
    doses = attr(nb_emax_model_cov_samples_updatedattr, "doses"),
    prob = c(.025, .975),
    contrast = matrix(c(1, 40), 1, 2),
    reference_dose = attr(nb_emax_model_cov_samples_updatedattr, "doses")[1],
    reference_type = "difference"
  )

  expect_failure(expect_s3_class(plot2a, NA))
  expect_s3_class(plot2a, "ggplot")

  #>reference_dose == [first dose], reference_type == "ratio"----

  plot2b <- plot.beaver_mcmc(
    x = nb_emax_model_cov_samples_updatedattr,
    doses = attr(nb_emax_model_cov_samples_updatedattr, "doses"),
    prob = c(.025, .975),
    contrast = matrix(c(1, 40), 1, 2),
    reference_dose = attr(nb_emax_model_cov_samples_updatedattr, "doses")[1],
    reference_type = "ratio"
  )

  expect_failure(expect_s3_class(plot2b, NA))
  expect_s3_class(plot2b, "ggplot")
})

test_that("plot.beaver_mcmc works against an S3 object of class beaver_mcmc, with covariates & type == \"g-comp\", produces an object with correct properties", { # nolint

  skip_on_cran()

  nb_monotone_incr_cov <- readRDS(test_path("fixtures", "nb_monotone_incr_cov.rds")) # nolint

  expect_failure(expect_s3_class(nb_monotone_incr_cov, NA))
  expect_s3_class(nb_monotone_incr_cov, "data.frame")

  load(test_path("fixtures", "nb_emax_cov_mcmc+_objects.Rdata"))

  expect_failure(expect_s3_class(nb_emax_model_cov_samples_updatedattr, NA))
  expect_s3_class(nb_emax_model_cov_samples_updatedattr, "beaver_mcmc")
  expect_no_error(
    checkmate::assertSubset(
      c("covariate_names", "formula", "doses"),
      names(attributes(nb_emax_model_cov_samples_updatedattr))
    )
  )

  #new_data----

  #>reference_dose == NULL----

  plot1 <- plot.beaver_mcmc(
    x = nb_emax_model_cov_samples_updatedattr,
    doses = attr(nb_emax_model_cov_samples_updatedattr, "doses"),
    prob = c(.025, .975),
    new_data = nb_monotone_incr_cov,
    type = "g-comp"
  )

  expect_failure(expect_s3_class(plot1, NA))
  expect_s3_class(plot1, "ggplot")

  # nolint start
  # #>reference_dose == [first dose], reference_type == "difference"----
  #
  # plot1a <- plot.beaver_mcmc(
  #   x = nb_emax_model_cov_samples_updatedattr,
  #   doses = attr(nb_emax_model_cov_samples_updatedattr, "doses"),
  #   prob = c(.025, .975),
  #   new_data = nb_monotone_incr_cov,
  #   type = "g-comp",
  #   reference_dose = attr(nb_emax_model_cov_samples_updatedattr, "doses")[1],
  #   reference_type = "difference"
  # )
  #
  # expect_failure(expect_s3_class(plot1a, NA))
  # expect_s3_class(plot1a, "ggplot")
  #
  # #>reference_dose == [first dose], reference_type == "ratio"----
  #
  # plot1b <- plot.beaver_mcmc(
  #   x = nb_emax_model_cov_samples_updatedattr,
  #   doses = attr(nb_emax_model_cov_samples_updatedattr, "doses"),
  #   prob = c(.025, .975),
  #   new_data = nb_monotone_incr_cov,
  #   type = "g-comp",
  #   reference_dose = attr(nb_emax_model_cov_samples_updatedattr, "doses")[1],
  #   reference_type = "ratio"
  # )
  #
  # expect_failure(expect_s3_class(plot1b, NA))
  # expect_s3_class(plot1b, "ggplot")
  #
  # #contrast----
  #
  # #>reference_dose == NULL----
  #
  # plot2 <- plot.beaver_mcmc(
  #   x = nb_emax_model_cov_samples_updatedattr,
  #   doses = attr(nb_emax_model_cov_samples_updatedattr, "doses"),
  #   prob = c(.025, .975),
  #   data = nb_monotone_incr_cov,
  #   contrast = matrix(c(1, 40), 1, 2),
  #   type = "g-comp"
  # )
  #
  # expect_failure(expect_s3_class(plot2, NA))
  # expect_s3_class(plot2, "ggplot")
  #
  # #>reference_dose == [first dose], reference_type == "difference"----
  #
  # plot2a <- plot.beaver_mcmc(
  #   x = nb_emax_model_cov_samples_updatedattr,
  #   doses = attr(nb_emax_model_cov_samples_updatedattr, "doses"),
  #   prob = c(.025, .975),
  #   contrast = matrix(c(1, 40), 1, 2),
  #   type = "g-comp",
  #   reference_dose = attr(nb_emax_model_cov_samples_updatedattr, "doses")[1],
  #   reference_type = "difference"
  # )
  #
  # expect_failure(expect_s3_class(plot2a, NA))
  # expect_s3_class(plot2a, "ggplot")
  #
  # #>reference_dose == [first dose], reference_type == "ratio"----
  #
  # plot2b <- plot.beaver_mcmc(
  #   x = nb_emax_model_cov_samples_updatedattr,
  #   doses = attr(nb_emax_model_cov_samples_updatedattr, "doses"),
  #   prob = c(.025, .975),
  #   contrast = matrix(c(1, 40), 1, 2),
  #   type = "g-comp",
  #   reference_dose = attr(nb_emax_model_cov_samples_updatedattr, "doses")[1],
  #   reference_type = "ratio"
  # )
  #
  # expect_failure(expect_s3_class(plot2b, NA))
  # expect_s3_class(plot2b, "ggplot")
  # nolint end
})

test_that("plot works against an S3 object of class beaver_mcmc", {

  skip_on_cran()

  nb_monotone_incr <- readRDS(test_path("fixtures", "nb_monotone_incr.rds"))

  expect_failure(expect_s3_class(nb_monotone_incr, NA))
  expect_s3_class(nb_monotone_incr, "data.frame")

  load(test_path("fixtures", "nb_indep_mcmc+_objects.Rdata"))

  expect_failure(expect_s3_class(nb_indep_model_samples_updatedattr, NA))
  expect_s3_class(nb_indep_model_samples_updatedattr, "beaver_mcmc")
  expect_no_error(
    checkmate::assertSubset(
      c("covariate_names", "formula", "doses"),
      names(attributes(nb_indep_model_samples_updatedattr))
    )
  )

  #new_data----

  #>reference_dose == NULL----

  expect_no_error(
    plot(
      x = nb_indep_model_samples_updatedattr,
      doses = attr(nb_indep_model_samples_updatedattr, "doses"),
      prob = c(.025, .975),
      new_data = nb_monotone_incr[1, ]
    )
  )

  #>reference_dose == [first dose], reference_type == "difference"----

  expect_no_error(
    plot(
      x = nb_indep_model_samples_updatedattr,
      doses = attr(nb_indep_model_samples_updatedattr, "doses"),
      prob = c(.025, .975),
      new_data = nb_monotone_incr[1, ],
      reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
      reference_type = "difference"
    )
  )

  #>reference_dose == [first dose], reference_type == "ratio"----

  expect_no_error(
    plot(
      x = nb_indep_model_samples_updatedattr,
      doses = attr(nb_indep_model_samples_updatedattr, "doses"),
      prob = c(.025, .975),
      new_data = nb_monotone_incr[1, ],
      reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
      reference_type = "ratio"
    )
  )

  #contrast----

  #>reference_dose == NULL----

  expect_no_error(
    plot(
      x = nb_indep_model_samples_updatedattr,
      doses = attr(nb_indep_model_samples_updatedattr, "doses"),
      prob = c(.025, .975),
      contrast = matrix(1, 1, 1)
    )
  )

  #>reference_dose == [first dose], reference_type == "difference"----

  expect_no_error(
    plot(
      x = nb_indep_model_samples_updatedattr,
      doses = attr(nb_indep_model_samples_updatedattr, "doses"),
      prob = c(.025, .975),
      contrast = matrix(1, 1, 1),
      reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
      reference_type = "difference"
    )
  )

  #>reference_dose == [first dose], reference_type == "ratio"----

  expect_no_error(
    plot(
      x = nb_indep_model_samples_updatedattr,
      doses = attr(nb_indep_model_samples_updatedattr, "doses"),
      prob = c(.025, .975),
      contrast = matrix(1, 1, 1),
      reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
      reference_type = "ratio"
    )
  )
})

test_that("plot.beaver_mcmc throws an error when warranted", {

  nb_monotone_incr <- readRDS(test_path("fixtures", "nb_monotone_incr.rds"))

  expect_failure(expect_s3_class(nb_monotone_incr, NA))
  expect_s3_class(nb_monotone_incr, "data.frame")

  nb_monotone_incr_new <- readRDS(test_path("fixtures", "nb_monotone_incr_new.rds")) # nolint

  expect_failure(expect_s3_class(nb_monotone_incr_new, NA))
  expect_s3_class(nb_monotone_incr_new, "data.frame")

  load(test_path("fixtures", "nb_indep_mcmc+_objects.Rdata"))

  expect_failure(expect_s3_class(nb_indep_model_samples_updatedattr, NA))
  expect_s3_class(nb_indep_model_samples_updatedattr, "beaver_mcmc")
  expect_no_error(
    checkmate::assertSubset(
      c("covariate_names", "formula", "doses"),
      names(attributes(nb_indep_model_samples_updatedattr))
    )
  )

  expect_error(
    plot.beaver_mcmc(
      x = nb_indep_model_samples_updatedattr,
      doses = attr(nb_indep_model_samples_updatedattr, "doses"),
      prob = c(.025),
      data = nb_monotone_incr,
      contrast = matrix(1, 1, 1)
    ),
    "\"prob\" must have length 2."
  )

  expect_error(
    plot.beaver_mcmc(
      x = nb_indep_model_samples_updatedattr,
      doses = attr(nb_indep_model_samples_updatedattr, "doses"),
      prob = c(.025, .975),
      data = nb_monotone_incr,
      new_data = nb_monotone_incr
    ),
    "\"new_data\" must have only one row for plotting."
  )

  expect_error(
    plot.beaver_mcmc(
      x = nb_indep_model_samples_updatedattr,
      doses = attr(nb_indep_model_samples_updatedattr, "doses"),
      prob = c(.025, .975),
      data = nb_monotone_incr,
      contrast = matrix(c(1, 1), 2, 1)
    ),
    "\"contrast\" must have only one row for plotting."
  )
})
