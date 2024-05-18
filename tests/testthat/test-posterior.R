
#Unlike other unit tests for this package, the following will not unit test the
#method directly. This is because the method, posterior.beaver_mcmc_bma,
# utilizes NextMethod(), and as per documentation "NextMethod should not be
#called except in methods called by UseMethod...", which places special objects
#in the evaluation frame (.Class, .Generic, and .Method) to direct the
#dispatching. Calling posterior.beaver_mcmc_bma directly results in the absence
#of these special objects, and any attempt to re-establish them for unit
#testing would effectively just re-create the generic.
test_that("posterior works against an S3 object of class beaver_mcmc_bma, produces an object with correct properties", { # nolint

  nb_monotone_incr_new <- readRDS(test_path("fixtures", "nb_monotone_incr_new.rds")) # nolint

  expect_failure(expect_s3_class(nb_monotone_incr_new, NA))
  expect_s3_class(nb_monotone_incr_new, "data.frame")

  load(test_path("fixtures", "nb_bma_objects.Rdata"))

  expect_failure(expect_s3_class(nb_bma, NA))
  expect_s3_class(
    nb_bma,
    c("beaver_mcmc_bma", "yodel_bma", "beaver_mcmc"),
    exact = TRUE
  )

  #new_data----

  #>reference_dose == NULL----

  posterior1 <- posterior(
    x = nb_bma,
    doses = attr(nb_bma, "doses"),
    reference_dose = NULL,
    prob = c(.025, .975),
    return_stats = TRUE,
    return_samples = TRUE,
    new_data = nb_monotone_incr_new
  )

  expect_true(is.list(posterior1))
  expect_named(posterior1, c("stats", "samples"))
  expect_failure(expect_s3_class(posterior1$stats, NA))
  expect_s3_class(posterior1$stats, "data.frame")
  expect_failure(expect_s3_class(posterior1$samples, NA))
  expect_s3_class(posterior1$samples, "data.frame")

  #>>stats only----

  posterior1_stats <- posterior(
    x = nb_bma,
    doses = attr(nb_bma, "doses"),
    reference_dose = NULL,
    prob = c(.025, .975),
    return_stats = TRUE,
    return_samples = FALSE,
    new_data = nb_monotone_incr_new
  )

  expect_true(is.list(posterior1_stats))
  expect_named(posterior1_stats, c("stats", "samples"))
  expect_identical(posterior1_stats$stats, posterior1$stats)
  expect_true(is.null(posterior1_stats$samples))

  #>>samples only----

  posterior1_samples <- posterior(
    x = nb_bma,
    doses = attr(nb_bma, "doses"),
    reference_dose = NULL,
    prob = c(.025, .975),
    return_stats = FALSE,
    return_samples = TRUE,
    new_data = nb_monotone_incr_new
  )

  expect_true(is.list(posterior1_samples))
  expect_named(posterior1_samples, c("stats", "samples"))
  expect_true(is.null(posterior1_samples$stats))
  expect_identical(posterior1_samples$samples, posterior1$samples)

  #>reference_dose == [first dose], reference_type == "difference"----

  posterior1a <- posterior(
    x = nb_bma,
    doses = attr(nb_bma, "doses"),
    reference_dose = attr(nb_bma, "doses")[1],
    prob = c(.025, .975),
    return_stats = TRUE,
    return_samples = TRUE,
    new_data = nb_monotone_incr_new,
    reference_type = "difference"
  )

  expect_true(is.list(posterior1a))
  expect_named(posterior1a, c("stats", "samples"))
  expect_failure(expect_s3_class(posterior1a$stats, NA))
  expect_s3_class(posterior1a$stats, "data.frame")
  expect_failure(expect_s3_class(posterior1a$samples, NA))
  expect_s3_class(posterior1a$samples, "data.frame")

  #>reference_dose == [first dose], reference_type == "ratio"----

  posterior1b <- posterior(
    x = nb_bma,
    doses = attr(nb_bma, "doses"),
    reference_dose = attr(nb_bma, "doses")[1],
    prob = c(.025, .975),
    return_stats = TRUE,
    return_samples = TRUE,
    new_data = nb_monotone_incr_new,
    reference_type = "ratio"
  )

  expect_true(is.list(posterior1b))
  expect_named(posterior1b, c("stats", "samples"))
  expect_failure(expect_s3_class(posterior1b$stats, NA))
  expect_s3_class(posterior1b$stats, "data.frame")
  expect_failure(expect_s3_class(posterior1b$samples, NA))
  expect_s3_class(posterior1b$samples, "data.frame")

  #contrast----

  #>reference_dose == NULL----

  posterior2 <- posterior(
    x = nb_bma,
    doses = attr(nb_bma, "doses"),
    reference_dose = NULL,
    prob = c(.025, .975),
    return_stats = TRUE,
    return_samples = TRUE,
    contrast = matrix(1, 1, 1)
  )

  expect_true(is.list(posterior2))
  expect_named(posterior2, c("stats", "samples"))
  expect_failure(expect_s3_class(posterior2$stats, NA))
  expect_s3_class(posterior2$stats, "data.frame")
  expect_failure(expect_s3_class(posterior2$samples, NA))
  expect_s3_class(posterior2$samples, "data.frame")

  #>>stats only----

  posterior2_stats <- posterior(
    x = nb_bma,
    doses = attr(nb_bma, "doses"),
    reference_dose = NULL,
    prob = c(.025, .975),
    return_stats = TRUE,
    return_samples = FALSE,
    contrast = matrix(1, 1, 1)
  )

  expect_true(is.list(posterior2_stats))
  expect_named(posterior2_stats, c("stats", "samples"))
  expect_identical(posterior2_stats$stats, posterior2$stats)
  expect_true(is.null(posterior2_stats$samples))

  #>>samples only----

  posterior2_samples <- posterior(
    x = nb_bma,
    doses = attr(nb_bma, "doses"),
    reference_dose = NULL,
    prob = c(.025, .975),
    return_stats = FALSE,
    return_samples = TRUE,
    contrast = matrix(1, 1, 1)
  )

  expect_true(is.list(posterior2_samples))
  expect_named(posterior2_samples, c("stats", "samples"))
  expect_true(is.null(posterior2_samples$stats))
  expect_identical(posterior2_samples$samples, posterior2$samples)

  #>reference_dose == [first dose], reference_type == "difference"----

  posterior2a <- posterior(
    x = nb_bma,
    doses = attr(nb_bma, "doses"),
    reference_dose = attr(nb_bma, "doses")[1],
    prob = c(.025, .975),
    return_stats = TRUE,
    return_samples = TRUE,
    contrast = matrix(1, 1, 1),
    reference_type = "difference"
  )

  expect_true(is.list(posterior2a))
  expect_named(posterior2a, c("stats", "samples"))
  expect_failure(expect_s3_class(posterior2a$stats, NA))
  expect_s3_class(posterior2a$stats, "data.frame")
  expect_failure(expect_s3_class(posterior2a$samples, NA))
  expect_s3_class(posterior2a$samples, "data.frame")

  #>reference_dose == [first dose], reference_type == "ratio"----

  posterior2b <- posterior(
    x = nb_bma,
    doses = attr(nb_bma, "doses"),
    reference_dose = attr(nb_bma, "doses")[1],
    prob = c(.025, .975),
    return_stats = TRUE,
    return_samples = TRUE,
    contrast = matrix(1, 1, 1),
    reference_type = "ratio"
  )

  expect_true(is.list(posterior2b))
  expect_named(posterior2b, c("stats", "samples"))
  expect_failure(expect_s3_class(posterior2b$stats, NA))
  expect_s3_class(posterior2b$stats, "data.frame")
  expect_failure(expect_s3_class(posterior2b$samples, NA))
  expect_s3_class(posterior2b$samples, "data.frame")
})

test_that("posterior.beaver_mcmc works against an S3 object of class beaver_mcmc, produces an object with correct properties", { # nolint

  skip_on_cran()

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

  #new_data----

  #>reference_dose == NULL----

  posterior1 <- posterior.beaver_mcmc(
    x = nb_indep_model_samples_updatedattr,
    doses = attr(nb_indep_model_samples_updatedattr, "doses"),
    reference_dose = NULL,
    prob = c(.025, .975),
    return_stats = TRUE,
    return_samples = TRUE,
    new_data = nb_monotone_incr_new
  )

  expect_true(is.list(posterior1))
  expect_named(posterior1, c("stats", "samples"))
  expect_failure(expect_s3_class(posterior1$stats, NA))
  expect_s3_class(posterior1$stats, "data.frame")
  expect_failure(expect_s3_class(posterior1$samples, NA))
  expect_s3_class(posterior1$samples, "data.frame")

  #>>stats only----

  posterior1_stats <- posterior.beaver_mcmc(
    x = nb_indep_model_samples_updatedattr,
    doses = attr(nb_indep_model_samples_updatedattr, "doses"),
    reference_dose = NULL,
    prob = c(.025, .975),
    return_stats = TRUE,
    return_samples = FALSE,
    new_data = nb_monotone_incr_new
  )

  expect_true(is.list(posterior1_stats))
  expect_named(posterior1_stats, c("stats", "samples"))
  expect_identical(posterior1_stats$stats, posterior1$stats)
  expect_true(is.null(posterior1_stats$samples))

  #>>samples only----

  posterior1_samples <- posterior.beaver_mcmc(
    x = nb_indep_model_samples_updatedattr,
    doses = attr(nb_indep_model_samples_updatedattr, "doses"),
    reference_dose = NULL,
    prob = c(.025, .975),
    return_stats = FALSE,
    return_samples = TRUE,
    new_data = nb_monotone_incr_new
  )

  expect_true(is.list(posterior1_samples))
  expect_named(posterior1_samples, c("stats", "samples"))
  expect_true(is.null(posterior1_samples$stats))
  expect_identical(posterior1_samples$samples, posterior1$samples)

  #>reference_dose == [first dose], reference_type == "difference"----

  posterior1a <- posterior.beaver_mcmc(
    x = nb_indep_model_samples_updatedattr,
    doses = attr(nb_indep_model_samples_updatedattr, "doses"),
    reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
    prob = c(.025, .975),
    return_stats = TRUE,
    return_samples = TRUE,
    new_data = nb_monotone_incr_new,
    reference_type = "difference"
  )

  expect_true(is.list(posterior1a))
  expect_named(posterior1a, c("stats", "samples"))
  expect_failure(expect_s3_class(posterior1a$stats, NA))
  expect_s3_class(posterior1a$stats, "data.frame")
  expect_failure(expect_s3_class(posterior1a$samples, NA))
  expect_s3_class(posterior1a$samples, "data.frame")

  #>reference_dose == [first dose], reference_type == "ratio"----

  posterior1b <- posterior.beaver_mcmc(
    x = nb_indep_model_samples_updatedattr,
    doses = attr(nb_indep_model_samples_updatedattr, "doses"),
    reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
    prob = c(.025, .975),
    return_stats = TRUE,
    return_samples = TRUE,
    new_data = nb_monotone_incr_new,
    reference_type = "ratio"
  )

  expect_true(is.list(posterior1b))
  expect_named(posterior1b, c("stats", "samples"))
  expect_failure(expect_s3_class(posterior1b$stats, NA))
  expect_s3_class(posterior1b$stats, "data.frame")
  expect_failure(expect_s3_class(posterior1b$samples, NA))
  expect_s3_class(posterior1b$samples, "data.frame")

  #contrast----

  #>reference_dose == NULL----

  posterior2 <- posterior.beaver_mcmc(
    x = nb_indep_model_samples_updatedattr,
    doses = attr(nb_indep_model_samples_updatedattr, "doses"),
    reference_dose = NULL,
    prob = c(.025, .975),
    return_stats = TRUE,
    return_samples = TRUE,
    contrast = matrix(1, 1, 1)
  )

  expect_true(is.list(posterior2))
  expect_named(posterior2, c("stats", "samples"))
  expect_failure(expect_s3_class(posterior2$stats, NA))
  expect_s3_class(posterior2$stats, "data.frame")
  expect_failure(expect_s3_class(posterior2$samples, NA))
  expect_s3_class(posterior2$samples, "data.frame")

  #>>stats only----

  posterior2_stats <- posterior.beaver_mcmc(
    x = nb_indep_model_samples_updatedattr,
    doses = attr(nb_indep_model_samples_updatedattr, "doses"),
    reference_dose = NULL,
    prob = c(.025, .975),
    return_stats = TRUE,
    return_samples = FALSE,
    contrast = matrix(1, 1, 1)
  )

  expect_true(is.list(posterior2_stats))
  expect_named(posterior2_stats, c("stats", "samples"))
  expect_identical(posterior2_stats$stats, posterior2$stats)
  expect_true(is.null(posterior2_stats$samples))

  #>>samples only----

  posterior2_samples <- posterior.beaver_mcmc(
    x = nb_indep_model_samples_updatedattr,
    doses = attr(nb_indep_model_samples_updatedattr, "doses"),
    reference_dose = NULL,
    prob = c(.025, .975),
    return_stats = FALSE,
    return_samples = TRUE,
    contrast = matrix(1, 1, 1)
  )

  expect_true(is.list(posterior2_samples))
  expect_named(posterior2_samples, c("stats", "samples"))
  expect_true(is.null(posterior2_samples$stats))
  expect_identical(posterior2_samples$samples, posterior2$samples)

  #>reference_dose == [first dose], reference_type == "difference"----

  posterior2a <- posterior.beaver_mcmc(
    x = nb_indep_model_samples_updatedattr,
    doses = attr(nb_indep_model_samples_updatedattr, "doses"),
    reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
    prob = c(.025, .975),
    return_stats = TRUE,
    return_samples = TRUE,
    contrast = matrix(1, 1, 1),
    reference_type = "difference"
  )

  expect_true(is.list(posterior2a))
  expect_named(posterior2a, c("stats", "samples"))
  expect_failure(expect_s3_class(posterior2a$stats, NA))
  expect_s3_class(posterior2a$stats, "data.frame")
  expect_failure(expect_s3_class(posterior2a$samples, NA))
  expect_s3_class(posterior2a$samples, "data.frame")

  #>reference_dose == [first dose], reference_type == "ratio"----

  posterior2b <- posterior.beaver_mcmc(
    x = nb_indep_model_samples_updatedattr,
    doses = attr(nb_indep_model_samples_updatedattr, "doses"),
    reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
    prob = c(.025, .975),
    return_stats = TRUE,
    return_samples = TRUE,
    contrast = matrix(1, 1, 1),
    reference_type = "ratio"
  )

  expect_true(is.list(posterior2b))
  expect_named(posterior2b, c("stats", "samples"))
  expect_failure(expect_s3_class(posterior2b$stats, NA))
  expect_s3_class(posterior2b$stats, "data.frame")
  expect_failure(expect_s3_class(posterior2b$samples, NA))
  expect_s3_class(posterior2b$samples, "data.frame")
})

test_that("posterior.beaver_mcmc works against an S3 object of class beaver_mcmc, with covariates, produces an object with correct properties", { # nolint

  skip_on_cran()

  nb_monotone_incr_cov_new <- readRDS(test_path("fixtures", "nb_monotone_incr_cov_new.rds")) # nolint

  expect_failure(expect_s3_class(nb_monotone_incr_cov_new, NA))
  expect_s3_class(nb_monotone_incr_cov_new, "data.frame")

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

  posterior1 <- posterior.beaver_mcmc(
    x = nb_emax_model_cov_samples_updatedattr,
    doses = attr(nb_emax_model_cov_samples_updatedattr, "doses"),
    reference_dose = NULL,
    prob = c(.025, .975),
    return_stats = TRUE,
    return_samples = TRUE,
    new_data = nb_monotone_incr_cov_new
  )

  expect_true(is.list(posterior1))
  expect_named(posterior1, c("stats", "samples"))
  expect_failure(expect_s3_class(posterior1$stats, NA))
  expect_s3_class(posterior1$stats, "data.frame")
  expect_failure(expect_s3_class(posterior1$samples, NA))
  expect_s3_class(posterior1$samples, "data.frame")

  #>>stats only----

  posterior1_stats <- posterior.beaver_mcmc(
    x = nb_emax_model_cov_samples_updatedattr,
    doses = attr(nb_emax_model_cov_samples_updatedattr, "doses"),
    reference_dose = NULL,
    prob = c(.025, .975),
    return_stats = TRUE,
    return_samples = FALSE,
    new_data = nb_monotone_incr_cov_new
  )

  expect_true(is.list(posterior1_stats))
  expect_named(posterior1_stats, c("stats", "samples"))
  expect_identical(posterior1_stats$stats, posterior1$stats)
  expect_true(is.null(posterior1_stats$samples))

  #>>samples only----

  posterior1_samples <- posterior.beaver_mcmc(
    x = nb_emax_model_cov_samples_updatedattr,
    doses = attr(nb_emax_model_cov_samples_updatedattr, "doses"),
    reference_dose = NULL,
    prob = c(.025, .975),
    return_stats = FALSE,
    return_samples = TRUE,
    new_data = nb_monotone_incr_cov_new
  )

  expect_true(is.list(posterior1_samples))
  expect_named(posterior1_samples, c("stats", "samples"))
  expect_true(is.null(posterior1_samples$stats))
  expect_identical(posterior1_samples$samples, posterior1$samples)

  #>reference_dose == [first dose], reference_type == "difference"----

  posterior1a <- posterior.beaver_mcmc(
    x = nb_emax_model_cov_samples_updatedattr,
    doses = attr(nb_emax_model_cov_samples_updatedattr, "doses"),
    reference_dose = attr(nb_emax_model_cov_samples_updatedattr, "doses")[1],
    prob = c(.025, .975),
    return_stats = TRUE,
    return_samples = TRUE,
    new_data = nb_monotone_incr_cov_new,
    reference_type = "difference"
  )

  expect_true(is.list(posterior1a))
  expect_named(posterior1a, c("stats", "samples"))
  expect_failure(expect_s3_class(posterior1a$stats, NA))
  expect_s3_class(posterior1a$stats, "data.frame")
  expect_failure(expect_s3_class(posterior1a$samples, NA))
  expect_s3_class(posterior1a$samples, "data.frame")

  #>reference_dose == [first dose], reference_type == "ratio"----

  posterior1b <- posterior.beaver_mcmc(
    x = nb_emax_model_cov_samples_updatedattr,
    doses = attr(nb_emax_model_cov_samples_updatedattr, "doses"),
    reference_dose = attr(nb_emax_model_cov_samples_updatedattr, "doses")[1],
    prob = c(.025, .975),
    return_stats = TRUE,
    return_samples = TRUE,
    new_data = nb_monotone_incr_cov_new,
    reference_type = "ratio"
  )

  expect_true(is.list(posterior1b))
  expect_named(posterior1b, c("stats", "samples"))
  expect_failure(expect_s3_class(posterior1b$stats, NA))
  expect_s3_class(posterior1b$stats, "data.frame")
  expect_failure(expect_s3_class(posterior1b$samples, NA))
  expect_s3_class(posterior1b$samples, "data.frame")

  #contrast----

  #>reference_dose == NULL----

  posterior2 <- posterior.beaver_mcmc(
    x = nb_emax_model_cov_samples_updatedattr,
    doses = attr(nb_emax_model_cov_samples_updatedattr, "doses"),
    reference_dose = NULL,
    prob = c(.025, .975),
    return_stats = TRUE,
    return_samples = TRUE,
    contrast = matrix(c(1, 40), 1, 2)
  )

  expect_true(is.list(posterior2))
  expect_named(posterior2, c("stats", "samples"))
  expect_failure(expect_s3_class(posterior2$stats, NA))
  expect_s3_class(posterior2$stats, "data.frame")
  expect_failure(expect_s3_class(posterior2$samples, NA))
  expect_s3_class(posterior2$samples, "data.frame")

  #>>stats only----

  posterior2_stats <- posterior.beaver_mcmc(
    x = nb_emax_model_cov_samples_updatedattr,
    doses = attr(nb_emax_model_cov_samples_updatedattr, "doses"),
    reference_dose = NULL,
    prob = c(.025, .975),
    return_stats = TRUE,
    return_samples = FALSE,
    contrast = matrix(c(1, 40), 1, 2)
  )

  expect_true(is.list(posterior2_stats))
  expect_named(posterior2_stats, c("stats", "samples"))
  expect_identical(posterior2_stats$stats, posterior2$stats)
  expect_true(is.null(posterior2_stats$samples))

  #>>samples only----

  posterior2_samples <- posterior.beaver_mcmc(
    x = nb_emax_model_cov_samples_updatedattr,
    doses = attr(nb_emax_model_cov_samples_updatedattr, "doses"),
    reference_dose = NULL,
    prob = c(.025, .975),
    return_stats = FALSE,
    return_samples = TRUE,
    contrast = matrix(c(1, 40), 1, 2)
  )

  expect_true(is.list(posterior2_samples))
  expect_named(posterior2_samples, c("stats", "samples"))
  expect_true(is.null(posterior2_samples$stats))
  expect_identical(posterior2_samples$samples, posterior2$samples)

  #>reference_dose == [first dose], reference_type == "difference"----

  posterior2a <- posterior.beaver_mcmc(
    x = nb_emax_model_cov_samples_updatedattr,
    doses = attr(nb_emax_model_cov_samples_updatedattr, "doses"),
    reference_dose = attr(nb_emax_model_cov_samples_updatedattr, "doses")[1],
    prob = c(.025, .975),
    return_stats = TRUE,
    return_samples = TRUE,
    contrast = matrix(c(1, 40), 1, 2),
    reference_type = "difference"
  )

  expect_true(is.list(posterior2a))
  expect_named(posterior2a, c("stats", "samples"))
  expect_failure(expect_s3_class(posterior2a$stats, NA))
  expect_s3_class(posterior2a$stats, "data.frame")
  expect_failure(expect_s3_class(posterior2a$samples, NA))
  expect_s3_class(posterior2a$samples, "data.frame")

  #>reference_dose == [first dose], reference_type == "ratio"----

  posterior2b <- posterior.beaver_mcmc(
    x = nb_emax_model_cov_samples_updatedattr,
    doses = attr(nb_emax_model_cov_samples_updatedattr, "doses"),
    reference_dose = attr(nb_emax_model_cov_samples_updatedattr, "doses")[1],
    prob = c(.025, .975),
    return_stats = TRUE,
    return_samples = TRUE,
    contrast = matrix(c(1, 40), 1, 2),
    reference_type = "ratio"
  )

  expect_true(is.list(posterior2b))
  expect_named(posterior2b, c("stats", "samples"))
  expect_failure(expect_s3_class(posterior2b$stats, NA))
  expect_s3_class(posterior2b$stats, "data.frame")
  expect_failure(expect_s3_class(posterior2b$samples, NA))
  expect_s3_class(posterior2b$samples, "data.frame")
})

test_that("posterior works identically to posterior.beaver_mcmc", {

  skip_on_cran()

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

  #new_data----

  #>reference_dose == NULL----

  expect_identical(
    yodel::posterior(
      x = nb_indep_model_samples_updatedattr,
      doses = attr(nb_indep_model_samples_updatedattr, "doses"),
      reference_dose = NULL,
      prob = c(.025, .975),
      return_stats = TRUE,
      return_samples = TRUE,
      new_data = nb_monotone_incr_new
    ),
    posterior.beaver_mcmc(
      x = nb_indep_model_samples_updatedattr,
      doses = attr(nb_indep_model_samples_updatedattr, "doses"),
      reference_dose = NULL,
      prob = c(.025, .975),
      return_stats = TRUE,
      return_samples = TRUE,
      new_data = nb_monotone_incr_new
    )
  )

  #>reference_dose == [first dose], reference_type == "difference"----

  expect_identical(
    yodel::posterior(
      x = nb_indep_model_samples_updatedattr,
      doses = attr(nb_indep_model_samples_updatedattr, "doses"),
      reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
      prob = c(.025, .975),
      return_stats = TRUE,
      return_samples = TRUE,
      new_data = nb_monotone_incr_new,
      reference_type = "difference"
    ),
    posterior.beaver_mcmc(
      x = nb_indep_model_samples_updatedattr,
      doses = attr(nb_indep_model_samples_updatedattr, "doses"),
      reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
      prob = c(.025, .975),
      return_stats = TRUE,
      return_samples = TRUE,
      new_data = nb_monotone_incr_new,
      reference_type = "difference"
    )
  )

  #>reference_dose == [first dose], reference_type == "ratio"----

  expect_identical(
    yodel::posterior(
      x = nb_indep_model_samples_updatedattr,
      doses = attr(nb_indep_model_samples_updatedattr, "doses"),
      reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
      prob = c(.025, .975),
      return_stats = TRUE,
      return_samples = TRUE,
      new_data = nb_monotone_incr_new,
      reference_type = "ratio"
    ),
    posterior.beaver_mcmc(
      x = nb_indep_model_samples_updatedattr,
      doses = attr(nb_indep_model_samples_updatedattr, "doses"),
      reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
      prob = c(.025, .975),
      return_stats = TRUE,
      return_samples = TRUE,
      new_data = nb_monotone_incr_new,
      reference_type = "ratio"
    )
  )

  #contrast----

  #>reference_dose == NULL----

  expect_identical(
    yodel::posterior(
      x = nb_indep_model_samples_updatedattr,
      doses = attr(nb_indep_model_samples_updatedattr, "doses"),
      reference_dose = NULL,
      prob = c(.025, .975),
      return_stats = TRUE,
      return_samples = TRUE,
      contrast = matrix(1, 1, 1)
    ),
    posterior.beaver_mcmc(
      x = nb_indep_model_samples_updatedattr,
      doses = attr(nb_indep_model_samples_updatedattr, "doses"),
      reference_dose = NULL,
      prob = c(.025, .975),
      return_stats = TRUE,
      return_samples = TRUE,
      contrast = matrix(1, 1, 1)
    )
  )

  #>reference_dose == [first dose], reference_type == "difference"----

  expect_identical(
    yodel::posterior(
      x = nb_indep_model_samples_updatedattr,
      doses = attr(nb_indep_model_samples_updatedattr, "doses"),
      reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
      prob = c(.025, .975),
      return_stats = TRUE,
      return_samples = TRUE,
      contrast = matrix(1, 1, 1),
      reference_type = "difference"
    ),
    posterior.beaver_mcmc(
      x = nb_indep_model_samples_updatedattr,
      doses = attr(nb_indep_model_samples_updatedattr, "doses"),
      reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
      prob = c(.025, .975),
      return_stats = TRUE,
      return_samples = TRUE,
      contrast = matrix(1, 1, 1),
      reference_type = "difference"
    )
  )

  #>reference_dose == [first dose], reference_type == "ratio"----

  expect_identical(
    yodel::posterior(
      x = nb_indep_model_samples_updatedattr,
      doses = attr(nb_indep_model_samples_updatedattr, "doses"),
      reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
      prob = c(.025, .975),
      return_stats = TRUE,
      return_samples = TRUE,
      contrast = matrix(1, 1, 1),
      reference_type = "ratio"
    ),
    posterior.beaver_mcmc(
      x = nb_indep_model_samples_updatedattr,
      doses = attr(nb_indep_model_samples_updatedattr, "doses"),
      reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
      prob = c(.025, .975),
      return_stats = TRUE,
      return_samples = TRUE,
      contrast = matrix(1, 1, 1),
      reference_type = "ratio"
    )
  )
})

test_that("posterior_bma works against an S3 object of class beaver_mcmc, produces an object with correct properties", { # nolint

  skip_on_cran()

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

  #new_data----

  #>reference_dose == NULL----

  posterior1 <- posterior_bma(
    x = nb_indep_model_samples_updatedattr,
    doses = attr(nb_indep_model_samples_updatedattr, "doses"),
    reference_dose = NULL,
    prob = c(.025, .975),
    new_data = nb_monotone_incr_new
  )

  expect_failure(expect_s3_class(posterior1, NA))
  expect_s3_class(posterior1, "data.frame")

  #>reference_dose == [first dose], reference_type == "difference"----

  posterior1a <- posterior_bma(
    x = nb_indep_model_samples_updatedattr,
    doses = attr(nb_indep_model_samples_updatedattr, "doses"),
    reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
    prob = c(.025, .975),
    new_data = nb_monotone_incr_new,
    reference_type = "difference"
  )

  expect_failure(expect_s3_class(posterior1a, NA))
  expect_s3_class(posterior1a, "data.frame")

  #>reference_dose == [first dose], reference_type == "ratio"----

  posterior1b <- posterior_bma(
    x = nb_indep_model_samples_updatedattr,
    doses = attr(nb_indep_model_samples_updatedattr, "doses"),
    reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
    prob = c(.025, .975),
    new_data = nb_monotone_incr_new,
    reference_type = "ratio"
  )

  expect_failure(expect_s3_class(posterior1b, NA))
  expect_s3_class(posterior1b, "data.frame")

  #contrast----

  #>reference_dose == NULL----

  posterior2 <- posterior_bma(
    x = nb_indep_model_samples_updatedattr,
    doses = attr(nb_indep_model_samples_updatedattr, "doses"),
    reference_dose = NULL,
    prob = c(.025, .975),
    contrast = matrix(1, 1, 1)
  )

  expect_failure(expect_s3_class(posterior2, NA))
  expect_s3_class(posterior2, "data.frame")

  #>reference_dose == [first dose], reference_type == "difference"----

  posterior2a <- posterior_bma(
    x = nb_indep_model_samples_updatedattr,
    doses = attr(nb_indep_model_samples_updatedattr, "doses"),
    reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
    prob = c(.025, .975),
    contrast = matrix(1, 1, 1),
    reference_type = "difference"
  )

  expect_failure(expect_s3_class(posterior2a, NA))
  expect_s3_class(posterior2a, "data.frame")

  #>reference_dose == [first dose], reference_type == "ratio"----

  posterior2b <- posterior_bma(
    x = nb_indep_model_samples_updatedattr,
    doses = attr(nb_indep_model_samples_updatedattr, "doses"),
    reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
    prob = c(.025, .975),
    contrast = matrix(1, 1, 1),
    reference_type = "ratio"
  )

  expect_failure(expect_s3_class(posterior2b, NA))
  expect_s3_class(posterior2b, "data.frame")
})

test_that("get_samps works against an S3 object of class beaver_mcmc, produces an object with correct properties", { # nolint

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

  draws <- draws(nb_indep_model_samples_updatedattr)

  #new_data----

  contrast1 <- get_contrast(
    x = nb_indep_model_samples_updatedattr,
    new_data = nb_monotone_incr_new,
    contrast = NULL
  )

  expect_no_error(
    get_samps(
      x = nb_indep_model_samples_updatedattr,
      draws = draws,
      doses = attr(nb_indep_model_samples_updatedattr, "doses"),
      contrast = contrast1
    )
  )
  expect_invisible(
    get_samps(
      x = nb_indep_model_samples_updatedattr,
      draws = draws,
      doses = attr(nb_indep_model_samples_updatedattr, "doses"),
      contrast = contrast1
    )
  )

  samps1 <- get_samps(
    x = nb_indep_model_samples_updatedattr,
    draws = draws,
    doses = attr(nb_indep_model_samples_updatedattr, "doses"),
    contrast = contrast1
  )

  get_samps_checks(
    samps = samps1,
    samples = nb_indep_model_samples_updatedattr,
    draws = draws,
    contrast = contrast1
  )

  #contrast----

  contrast2 <- get_contrast(
    x = nb_indep_model_samples_updatedattr,
    new_data = NULL,
    contrast = matrix(1, 1, 1)
  )

  expect_no_error(
    get_samps(
      x = nb_indep_model_samples_updatedattr,
      draws = draws,
      doses = attr(nb_indep_model_samples_updatedattr, "doses"),
      contrast = contrast2
    )
  )
  expect_invisible(
    get_samps(
      x = nb_indep_model_samples_updatedattr,
      draws = draws,
      doses = attr(nb_indep_model_samples_updatedattr, "doses"),
      contrast = contrast2
    )
  )

  samps2 <- get_samps(
    x = nb_indep_model_samples_updatedattr,
    draws = draws,
    doses = attr(nb_indep_model_samples_updatedattr, "doses"),
    contrast = contrast2
  )

  get_samps_checks(
    samps = samps2,
    samples = nb_indep_model_samples_updatedattr,
    draws = draws,
    contrast = contrast2
  )
})

test_that("get_samps works against an S3 object of class beaver_mcmc, with covariates, produces an object with correct properties", { # nolint

  nb_monotone_incr_cov_new <- readRDS(test_path("fixtures", "nb_monotone_incr_cov_new.rds")) # nolint

  expect_failure(expect_s3_class(nb_monotone_incr_cov_new, NA))
  expect_s3_class(nb_monotone_incr_cov_new, "data.frame")

  load(test_path("fixtures", "nb_emax_cov_mcmc+_objects.Rdata"))

  expect_failure(expect_s3_class(nb_emax_model_cov_samples_updatedattr, NA))
  expect_s3_class(nb_emax_model_cov_samples_updatedattr, "beaver_mcmc")
  expect_no_error(
    checkmate::assertSubset(
      c("covariate_names", "formula", "doses"),
      names(attributes(nb_emax_model_cov_samples_updatedattr))
    )
  )

  draws <- draws(nb_emax_model_cov_samples_updatedattr)

  #new_data----

  contrast1 <- get_contrast(
    x = nb_emax_model_cov_samples_updatedattr,
    new_data = nb_monotone_incr_cov_new,
    contrast = NULL
  )

  expect_no_error(
    get_samps(
      x = nb_emax_model_cov_samples_updatedattr,
      draws = draws,
      doses = attr(nb_emax_model_cov_samples_updatedattr, "doses"),
      contrast = contrast1
    )
  )
  expect_invisible(
    get_samps(
      x = nb_emax_model_cov_samples_updatedattr,
      draws = draws,
      doses = attr(nb_emax_model_cov_samples_updatedattr, "doses"),
      contrast = contrast1
    )
  )

  samps1 <- get_samps(
    x = nb_emax_model_cov_samples_updatedattr,
    draws = draws,
    doses = attr(nb_emax_model_cov_samples_updatedattr, "doses"),
    contrast = contrast1
  )

  get_samps_checks(
    samps = samps1,
    samples = nb_emax_model_cov_samples_updatedattr,
    draws = draws,
    contrast = contrast1
  )

  #contrast----

  contrast2 <- get_contrast(
    x = nb_emax_model_cov_samples_updatedattr,
    new_data = NULL,
    contrast = matrix(c(1, 40), 1, 2)
  )

  expect_no_error(
    get_samps(
      x = nb_emax_model_cov_samples_updatedattr,
      draws = draws,
      doses = attr(nb_emax_model_cov_samples_updatedattr, "doses"),
      contrast = contrast2
    )
  )
  expect_invisible(
    get_samps(
      x = nb_emax_model_cov_samples_updatedattr,
      draws = draws,
      doses = attr(nb_emax_model_cov_samples_updatedattr, "doses"),
      contrast = contrast2
    )
  )

  samps2 <- get_samps(
    x = nb_emax_model_cov_samples_updatedattr,
    draws = draws,
    doses = attr(nb_emax_model_cov_samples_updatedattr, "doses"),
    contrast = contrast2
  )

  get_samps_checks(
    samps = samps2,
    samples = nb_emax_model_cov_samples_updatedattr,
    draws = draws,
    contrast = contrast2
  )

  get_samps_checks(
    samps = samps2,
    samples = nb_emax_model_cov_samples_updatedattr,
    draws = draws,
    contrast = contrast2
  )
})

test_that("adjust_reference works against an S3 object of class beaver_mcmc, produces an object with correct properties", { # nolint

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

  draws <- draws(nb_indep_model_samples_updatedattr)

  #new_data----

  contrast1 <- get_contrast(
    x = nb_indep_model_samples_updatedattr,
    new_data = nb_monotone_incr_new,
    contrast = NULL
  )

  samps1 <- get_samps(
    x = nb_indep_model_samples_updatedattr,
    draws = draws,
    doses = attr(nb_indep_model_samples_updatedattr, "doses"),
    contrast = contrast1
  )

  #>reference_dose == NULL----

  expect_no_error(
    adjust_reference(
      x = nb_indep_model_samples_updatedattr,
      samps = samps1,
      draws = draws,
      contrast = contrast1,
      reference_dose = NULL
    )
  )

  adjust_reference1 <- adjust_reference(
    x = nb_indep_model_samples_updatedattr,
    samps = samps1,
    draws = draws,
    contrast = contrast1,
    reference_dose = NULL
  )

  adjust_reference_checks(
    adjusted = adjust_reference1,
    samples = nb_indep_model_samples_updatedattr,
    draws = draws,
    contrast = contrast1
  )

  #>reference_dose == [first dose], reference_type == "difference"----

  expect_no_error(
    adjust_reference(
      x = nb_indep_model_samples_updatedattr,
      samps = samps1,
      draws = draws,
      contrast = contrast1,
      reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
      reference_type = "difference"
    )
  )

  adjust_reference1a <- adjust_reference(
    x = nb_indep_model_samples_updatedattr,
    samps = samps1,
    draws = draws,
    contrast = contrast1,
    reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
    reference_type = "difference"
  )

  adjust_reference_checks(
    adjusted = adjust_reference1a,
    samples = nb_indep_model_samples_updatedattr,
    draws = draws,
    contrast = contrast1
  )

  #>reference_dose == [first dose], reference_type == "ratio"----

  expect_no_error(
    adjust_reference(
      x = nb_indep_model_samples_updatedattr,
      samps = samps1,
      draws = draws,
      contrast = contrast1,
      reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
      reference_type = "ratio"
    )
  )

  adjust_reference1b <- adjust_reference(
    x = nb_indep_model_samples_updatedattr,
    samps = samps1,
    draws = draws,
    contrast = contrast1,
    reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
    reference_type = "ratio"
  )

  adjust_reference_checks(
    adjusted = adjust_reference1b,
    samples = nb_indep_model_samples_updatedattr,
    draws = draws,
    contrast = contrast1
  )

  #contrast----

  contrast2 <- get_contrast(
    x = nb_indep_model_samples_updatedattr,
    new_data = NULL,
    contrast = matrix(1, 1, 1)
  )

  samps2 <- get_samps(
    x = nb_indep_model_samples_updatedattr,
    draws = draws,
    doses = attr(nb_indep_model_samples_updatedattr, "doses"),
    contrast = contrast2
  )

  #>reference_dose == NULL----

  expect_no_error(
    adjust_reference(
      x = nb_indep_model_samples_updatedattr,
      samps = samps2,
      draws = draws,
      contrast = contrast2,
      reference_dose = NULL
    )
  )

  adjust_reference2 <- adjust_reference(
    x = nb_indep_model_samples_updatedattr,
    samps = samps2,
    draws = draws,
    contrast = contrast2,
    reference_dose = NULL
  )

  adjust_reference_checks(
    adjusted = adjust_reference2,
    samples = nb_indep_model_samples_updatedattr,
    draws = draws,
    contrast = contrast2
  )

  #>reference_dose == [first dose], reference_type == "difference"----

  expect_no_error(
    adjust_reference(
      x = nb_indep_model_samples_updatedattr,
      samps = samps2,
      draws = draws,
      contrast = contrast2,
      reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
      reference_type = "difference"
    )
  )

  adjust_reference2a <- adjust_reference(
    x = nb_indep_model_samples_updatedattr,
    samps = samps2,
    draws = draws,
    contrast = contrast2,
    reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
    reference_type = "difference"
  )

  adjust_reference_checks(
    adjusted = adjust_reference2a,
    samples = nb_indep_model_samples_updatedattr,
    draws = draws,
    contrast = contrast2
  )

  #>reference_dose == [first dose], reference_type == "ratio"----

  expect_no_error(
    adjust_reference(
      x = nb_indep_model_samples_updatedattr,
      samps = samps2,
      draws = draws,
      contrast = contrast2,
      reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
      reference_type = "ratio"
    )
  )

  adjust_reference2b <- adjust_reference(
    x = nb_indep_model_samples_updatedattr,
    samps = samps2,
    draws = draws,
    contrast = contrast2,
    reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
    reference_type = "ratio"
  )

  adjust_reference_checks(
    adjusted = adjust_reference2b,
    samples = nb_indep_model_samples_updatedattr,
    draws = draws,
    contrast = contrast2
  )
})

test_that("adjust_reference_impl works against an S3 object of class beaver_mcmc, produces an object with correct properties", { # nolint

  skip_on_cran()

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

  draws <- draws(nb_indep_model_samples_updatedattr)

  #new_data----

  contrast1 <- get_contrast(
    x = nb_indep_model_samples_updatedattr,
    new_data = nb_monotone_incr_new,
    contrast = NULL
  )

  samps1 <- get_samps(
    x = nb_indep_model_samples_updatedattr,
    draws = draws,
    doses = attr(nb_indep_model_samples_updatedattr, "doses"),
    contrast = contrast1
  )

  samps_ref1 <- get_samps(
    x = nb_indep_model_samples_updatedattr,
    draws = draws,
    doses = attr(nb_indep_model_samples_updatedattr, "doses")[1],
    contrast = contrast1
  )

  #>reference_dose == [first dose], reference_type == "difference"----

  expect_no_error(
    adjust_reference_impl(
      samps = samps1,
      samps_ref = samps_ref1,
      reference_type = "difference",
      reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1]
    )
  )

  adjust_reference_impl1a <- adjust_reference_impl(
    samps = samps1,
    samps_ref = samps_ref1,
    reference_type = "difference",
    reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1]
  )

  adjust_reference_checks(
    adjusted = adjust_reference_impl1a,
    samples = nb_indep_model_samples_updatedattr,
    draws = draws,
    contrast = contrast1
  )

  #>reference_dose == [first dose], reference_type == "ratio"----

  expect_no_error(
    adjust_reference_impl1b <- adjust_reference_impl(
      samps = samps1,
      samps_ref = samps_ref1,
      reference_type = "ratio",
      reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1]
    )
  )

  adjust_reference_impl1b <- adjust_reference_impl(
    samps = samps1,
    samps_ref = samps_ref1,
    reference_type = "ratio",
    reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1]
  )

  adjust_reference_checks(
    adjusted = adjust_reference_impl1b,
    samples = nb_indep_model_samples_updatedattr,
    draws = draws,
    contrast = contrast1
  )

  #contrast----

  contrast2 <- get_contrast(
    x = nb_indep_model_samples_updatedattr,
    new_data = NULL,
    contrast = matrix(1, 1, 1)
  )

  samps2 <- get_samps(
    x = nb_indep_model_samples_updatedattr,
    draws = draws,
    doses = attr(nb_indep_model_samples_updatedattr, "doses"),
    contrast = contrast2
  )

  samps_ref2 <- get_samps(
    x = nb_indep_model_samples_updatedattr,
    draws = draws,
    doses = attr(nb_indep_model_samples_updatedattr, "doses")[1],
    contrast = contrast2
  )

  #>reference_dose == [first dose], reference_type == "difference"----

  expect_no_error(
    adjust_reference_impl(
      samps = samps2,
      samps_ref = samps_ref2,
      reference_type = "difference",
      reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1]
    )
  )

  adjust_reference_impl2a <- adjust_reference_impl(
    samps = samps2,
    samps_ref = samps_ref2,
    reference_type = "difference",
    reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1]
  )

  adjust_reference_checks(
    adjusted = adjust_reference_impl2a,
    samples = nb_indep_model_samples_updatedattr,
    draws = draws,
    contrast = contrast2
  )

  #>reference_dose == [first dose], reference_type == "ratio"----

  expect_no_error(
    adjust_reference_impl2b <- adjust_reference_impl(
      samps = samps2,
      samps_ref = samps_ref2,
      reference_type = "ratio",
      reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1]
    )
  )

  adjust_reference_impl2b <- adjust_reference_impl(
    samps = samps2,
    samps_ref = samps_ref2,
    reference_type = "ratio",
    reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1]
  )

  adjust_reference_checks(
    adjusted = adjust_reference_impl2b,
    samples = nb_indep_model_samples_updatedattr,
    draws = draws,
    contrast = contrast2
  )
})

test_that("get_stats works against an S3 object of class beaver_mcmc, produces an object with correct properties", { # nolint

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

  draws <- draws(nb_indep_model_samples_updatedattr)

  #new_data----

  contrast1 <- get_contrast(
    x = nb_indep_model_samples_updatedattr,
    new_data = nb_monotone_incr_new,
    contrast = NULL
  )

  samps1 <- get_samps(
    x = nb_indep_model_samples_updatedattr,
    draws = draws,
    doses = attr(nb_indep_model_samples_updatedattr, "doses"),
    contrast = contrast1
  )

  #>reference_dose == NULL----

  adjust_reference1 <- adjust_reference(
    x = nb_indep_model_samples_updatedattr,
    samps = samps1,
    draws = draws,
    contrast = contrast1,
    reference_dose = NULL
  )

  expect_no_error(
    get_stats(
      samples = adjust_reference1,
      prob = c(.025, .975),
      return_stats = TRUE
    )
  )

  stats1 <- get_stats(
    samples = adjust_reference1,
    prob = c(.025, .975),
    return_stats = TRUE
  )

  get_stats_checks(
    stats = stats1,
    samples = nb_indep_model_samples_updatedattr,
    contrast = contrast1
  )

  #>reference_dose == [first dose], reference_type == "difference"----

  adjust_reference1a <- adjust_reference(
    x = nb_indep_model_samples_updatedattr,
    samps = samps1,
    draws = draws,
    contrast = contrast1,
    reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
    reference_type = "difference"
  )

  expect_no_error(
    get_stats(
      samples = adjust_reference1a,
      prob = c(.025, .975),
      return_stats = TRUE
    )
  )

  stats1a <- get_stats(
    samples = adjust_reference1a,
    prob = c(.025, .975),
    return_stats = TRUE
  )

  get_stats_checks(
    stats = stats1a,
    samples = nb_indep_model_samples_updatedattr,
    contrast = contrast1
  )

  #>reference_dose == [first dose], reference_type == "ratio"----

  adjust_reference1b <- adjust_reference(
    x = nb_indep_model_samples_updatedattr,
    samps = samps1,
    draws = draws,
    contrast = contrast1,
    reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
    reference_type = "ratio"
  )

  expect_no_error(
    get_stats(
      samples = adjust_reference1b,
      prob = c(.025, .975),
      return_stats = TRUE
    )
  )

  stats1b <- get_stats(
    samples = adjust_reference1b,
    prob = c(.025, .975),
    return_stats = TRUE
  )

  get_stats_checks(
    stats = stats1b,
    samples = nb_indep_model_samples_updatedattr,
    contrast = contrast1
  )

  #contrast----

  contrast2 <- get_contrast(
    x = nb_indep_model_samples_updatedattr,
    new_data = NULL,
    contrast = matrix(1, 1, 1)
  )

  samps2 <- get_samps(
    x = nb_indep_model_samples_updatedattr,
    draws = draws,
    doses = attr(nb_indep_model_samples_updatedattr, "doses"),
    contrast = contrast2
  )

  #>reference_dose == NULL----

  adjust_reference2 <- adjust_reference(
    x = nb_indep_model_samples_updatedattr,
    samps = samps2,
    draws = draws,
    contrast = contrast2,
    reference_dose = NULL
  )

  expect_no_error(
    get_stats(
      samples = adjust_reference2,
      prob = c(.025, .975),
      return_stats = TRUE
    )
  )

  stats2 <- get_stats(
    samples = adjust_reference2,
    prob = c(.025, .975),
    return_stats = TRUE
  )

  get_stats_checks(
    stats = stats2,
    samples = nb_indep_model_samples_updatedattr,
    contrast = contrast2
  )

  #>reference_dose == [first dose], reference_type == "difference"----

  adjust_reference2a <- adjust_reference(
    x = nb_indep_model_samples_updatedattr,
    samps = samps2,
    draws = draws,
    contrast = contrast2,
    reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
    reference_type = "difference"
  )

  expect_no_error(
    get_stats(
      samples = adjust_reference2a,
      prob = c(.025, .975),
      return_stats = TRUE
    )
  )

  stats2a <- get_stats(
    samples = adjust_reference2a,
    prob = c(.025, .975),
    return_stats = TRUE
  )

  get_stats_checks(
    stats = stats2a,
    samples = nb_indep_model_samples_updatedattr,
    contrast = contrast2
  )

  #>reference_dose == [first dose], reference_type == "ratio"----

  adjust_reference2b <- adjust_reference(
    x = nb_indep_model_samples_updatedattr,
    samps = samps2,
    draws = draws,
    contrast = contrast2,
    reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
    reference_type = "ratio"
  )

  expect_no_error(
    get_stats(
      samples = adjust_reference2b,
      prob = c(.025, .975),
      return_stats = TRUE
    )
  )

  stats2b <- get_stats(
    samples = adjust_reference2b,
    prob = c(.025, .975),
    return_stats = TRUE
  )

  get_stats_checks(
    stats = stats2b,
    samples = nb_indep_model_samples_updatedattr,
    contrast = contrast2
  )
})

test_that("contrast_to_list works against a matrix object, produces an object with correct properties", { # nolint

  nb_monotone_incr_new <- readRDS(test_path("fixtures", "nb_monotone_incr_new.rds")) # nolint

  expect_failure(expect_s3_class(nb_monotone_incr_new, NA))
  expect_s3_class(nb_monotone_incr_new, "data.frame")

  load(test_path("fixtures", "nb_indep_mcmc+_objects.Rdata"))

  expect_failure(expect_s3_class(nb_indep_model_samples_updatedattr, NA))
  expect_s3_class(nb_indep_model_samples_updatedattr, "beaver_mcmc")
  expect_no_error(
    checkmate::assertSubset(
      c("doses"),
      names(attributes(nb_indep_model_samples_updatedattr))
    )
  )

  #new_data----

  contrast1 <- get_contrast(
    x = nb_indep_model_samples_updatedattr,
    new_data = nb_monotone_incr_new,
    contrast = NULL
  )

  expect_true(is.matrix(contrast1))

  expect_no_error(contrast_to_list(contrast = contrast1))

  contrast_list1 <- contrast_to_list(contrast = contrast1)

  expect_true(is.list(contrast_list1))
  expect_named(contrast_list1, NULL)
  expect_identical(length(contrast_list1), nrow(contrast1))

  #contrast----

  contrast2 <- get_contrast(
    x = nb_indep_model_samples_updatedattr,
    new_data = NULL,
    contrast = matrix(1, 1, 1)
  )

  expect_true(is.matrix(contrast2))

  expect_no_error(contrast_to_list(contrast = contrast2))

  contrast_list2 <- contrast_to_list(contrast = contrast2)

  expect_true(is.list(contrast_list2))
  expect_named(contrast_list2, NULL)
  expect_identical(length(contrast_list2), nrow(contrast2))
})

test_that("get_contrast works against an S3 object of class beaver_mcmc, produces an object with correct properties", { # nolint

  nb_monotone_incr_new <- readRDS(test_path("fixtures", "nb_monotone_incr_new.rds")) # nolint

  expect_failure(expect_s3_class(nb_monotone_incr_new, NA))
  expect_s3_class(nb_monotone_incr_new, "data.frame")

  load(test_path("fixtures", "nb_indep_mcmc+_objects.Rdata"))

  expect_failure(expect_s3_class(nb_indep_model_samples_updatedattr, NA))
  expect_s3_class(nb_indep_model_samples_updatedattr, "beaver_mcmc")
  expect_no_error(
    checkmate::assertSubset(
      c("covariate_names", "formula"),
      names(attributes(nb_indep_model_samples_updatedattr))
    )
  )

  #new_data and contrast both NULL----

  expect_error(
    get_contrast(
      x = nb_indep_model_samples_updatedattr,
      new_data = NULL,
      contrast = NULL
    )
  )

  #new_data not NULL, contrast NULL----

  expect_no_error(
    get_contrast(
      x = nb_indep_model_samples_updatedattr,
      new_data = nb_monotone_incr_new,
      contrast = NULL
    )
  )
  expect_invisible(
    get_contrast(
      x = nb_indep_model_samples_updatedattr,
      new_data = nb_monotone_incr_new,
      contrast = NULL
    )
  )

  contrast1 <- get_contrast(
    x = nb_indep_model_samples_updatedattr,
    new_data = nb_monotone_incr_new,
    contrast = NULL
  )

  expect_true(is.matrix(contrast1))
  expect_identical(
    colnames(contrast1),
    attr(nb_indep_model_samples_updatedattr, "covariate_names")
  )
  expect_identical(nrow(contrast1), nrow(nb_monotone_incr_new))
  expect_identical(
    ncol(contrast1),
    length(attr(nb_indep_model_samples_updatedattr, "covariate_names"))
  )

  #new_data NULL, contrast not NULL----

  expect_no_error(
    get_contrast(
      x = nb_indep_model_samples_updatedattr,
      new_data = NULL,
      contrast = matrix(1, 1, 1)
    )
  )

  contrast2 <- get_contrast(
    x = nb_indep_model_samples_updatedattr,
    new_data = NULL,
    contrast = matrix(1, 1, 1)
  )

  expect_true(is.matrix(contrast2))
  expect_identical(
    colnames(contrast2),
    attr(nb_indep_model_samples_updatedattr, "covariate_names")
  )
  expect_identical(
    ncol(contrast2),
    length(attr(nb_indep_model_samples_updatedattr, "covariate_names"))
  )
})

test_that("post_mean works against an S3 object of class beaver_mcmc, produces an object with correct properties", { # nolint

  load(test_path("fixtures", "nb_indep_mcmc+_objects.Rdata"))

  expect_failure(expect_s3_class(nb_indep_model_samples_updatedattr, NA))
  expect_s3_class(nb_indep_model_samples_updatedattr, "beaver_mcmc")
  expect_no_error(
    checkmate::assertSubset(
      c("covariate_names", "formula", "doses"),
      names(attributes(nb_indep_model_samples_updatedattr))
    )
  )

  draws <- draws(nb_indep_model_samples_updatedattr)

  contrast <- get_contrast(
    x = nb_indep_model_samples_updatedattr,
    new_data = NULL,
    contrast = matrix(1, 1, 1)
  )

  expect_no_error(
    post_mean(
      x = nb_indep_model_samples_updatedattr,
      dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
      samps = draws,
      contrast = contrast,
      .contrast_index = 1
    )
  )

  post_mean <- post_mean(
    x = nb_indep_model_samples_updatedattr,
    dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
    samps = draws,
    contrast = contrast,
    .contrast_index = 1
  )

  expect_failure(expect_s3_class(post_mean, NA))
  expect_s3_class(post_mean, "data.frame")
  expect_identical(
    nrow(post_mean),
    sum(sapply(nb_indep_model_samples_updatedattr, nrow))
  )
  expect_failure(expect_named(post_mean, NULL))
  expect_identical(
    names(post_mean),
    c("dose", "value", "iter", ".contrast_index", colnames(contrast))
  )
  for (i in names(post_mean)) expect_true(is.numeric(post_mean[[i]]))
  expect_identical(
    post_mean %>%
      dplyr::distinct(dose) %>%
      dplyr::pull(dose),
    attr(nb_indep_model_samples_updatedattr, "doses")[1]
  )
  expect_identical(
    post_mean %>%
      dplyr::pull(iter),
    draws %>%
      dplyr::pull(iter)
  )
  expect_identical(
    post_mean %>%
      dplyr::distinct(.contrast_index) %>%
      dplyr::pull(.contrast_index),
    1
  )
  expect_identical(
    post_mean %>%
      dplyr::distinct(!!sym(colnames(contrast))) %>%
      dplyr::pull(colnames(contrast)),
    contrast %>%
      tibble::as_tibble() %>%
      dplyr::pull(colnames(contrast))
  )
})

test_that("get_intercept works against an S3 object of class beaver_mcmc, produces an object with correct properties", { # nolint

  nb_monotone_incr_new <- readRDS(test_path("fixtures", "nb_monotone_incr_new.rds")) # nolint

  expect_failure(expect_s3_class(nb_monotone_incr_new, NA))
  expect_s3_class(nb_monotone_incr_new, "data.frame")

  load(test_path("fixtures", "nb_indep_mcmc+_objects.Rdata"))

  expect_failure(expect_s3_class(nb_indep_model_samples_updatedattr, NA))
  expect_s3_class(nb_indep_model_samples_updatedattr, "beaver_mcmc")
  expect_no_error(
    checkmate::assertSubset(
      c("covariate_names", "formula"),
      names(attributes(nb_indep_model_samples_updatedattr))
    )
  )

  #new_data----

  expect_no_error(
    get_intercept(
      mcmc = draws(nb_indep_model_samples_updatedattr),
      contrast = get_contrast(
        x = nb_indep_model_samples_updatedattr,
        new_data = nb_monotone_incr_new,
        contrast = NULL
      )
    )
  )

  intercept1 <- get_intercept(
    mcmc = draws(nb_indep_model_samples_updatedattr),
    contrast = get_contrast(
      x = nb_indep_model_samples_updatedattr,
      new_data = nb_monotone_incr_new,
      contrast = NULL
    )
  )

  expect_true(is.matrix(intercept1))
  expect_identical(
    nrow(intercept1),
    sum(sapply(nb_indep_model_samples_updatedattr, nrow))
  )
  expect_identical(ncol(intercept1), nrow(nb_monotone_incr_new))
  expect_identical(
    tibble::as_tibble(intercept1) %>%
      dplyr::rowwise() %>%
      dplyr::summarize(
        a = length(unique(dplyr::c_across(cols = dplyr::everything())))
      ) %>%
      dplyr::ungroup() %>%
      dplyr::distinct(a) %>%
      dplyr::pull(a),
    length(attr(nb_indep_model_samples_updatedattr, "covariate_names"))
  )

  #contrast----

  expect_no_error(
    get_intercept(
      mcmc = draws(nb_indep_model_samples_updatedattr),
      contrast = matrix(1, 1, 1)
    )
  )

  intercept2 <- get_intercept(
    mcmc = draws(nb_indep_model_samples_updatedattr),
    contrast = matrix(1, 1, 1)
  )

  expect_true(is.matrix(intercept2))
  expect_identical(
    nrow(intercept2),
    sum(sapply(nb_indep_model_samples_updatedattr, nrow))
  )
  expect_identical(ncol(intercept2), 1L)
})

test_that("select_cols works against an S3 object of class data.frame, produces an object with correct properties", { # nolint

  load(test_path("fixtures", "nb_indep_mcmc+_objects.Rdata"))

  expect_failure(expect_s3_class(nb_indep_model_samples_updatedattr, NA))
  expect_s3_class(nb_indep_model_samples_updatedattr, "beaver_mcmc")
  expect_no_error(
    checkmate::assertSubset(
      c("doses"),
      names(attributes(nb_indep_model_samples_updatedattr))
    )
  )

  draws <- draws(nb_indep_model_samples_updatedattr)

  expect_s3_class(draws, c("tbl_df", "data.frame"))

  expect_no_error(
    draws %>%
      dplyr::mutate(
        dose = attr(nb_indep_model_samples_updatedattr, "doses")[1]
      ) %>%
      tibble::add_column(value = rnorm(nrow(.))) %>%
      tibble::add_column(.contrast_index = rnorm(nrow(.))) %>%
      select_cols()
  )

  set.seed(1234)
  draws1 <- draws %>%
    dplyr::mutate(
      dose = attr(nb_indep_model_samples_updatedattr, "doses")[1]
    ) %>%
    tibble::add_column(value = rnorm(nrow(.))) %>%
    tibble::add_column(.contrast_index = rnorm(nrow(.))) %>%
    select_cols()

  expect_failure(expect_s3_class(draws1, NA))
  expect_s3_class(draws1, c("tbl_df", "data.frame"))
  expect_identical(nrow(draws1), nrow(draws))
  expect_failure(expect_named(draws1, NULL))
  expect_identical(
    names(draws1),
    c("dose", "value", "iter", ".contrast_index")
  )
})

test_that("posterior_g_comp works against an S3 object of class class beaver_mcmc_bma, produces an object with correct properties", { # nolint

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

  posterior1 <- posterior_g_comp(
    x = nb_bma_cov,
    doses = attr(nb_bma_cov, "doses"),
    reference_dose = NULL,
    prob = c(.025, .975),
    return_stats = TRUE,
    return_samples = TRUE,
    new_data = nb_monotone_incr_cov
  )

  expect_true(is.list(posterior1))
  expect_named(posterior1, c("stats", "samples"))
  expect_failure(expect_s3_class(posterior1$stats, NA))
  expect_s3_class(posterior1$stats, "data.frame")
  expect_failure(expect_s3_class(posterior1$samples, NA))
  expect_s3_class(posterior1$samples, "data.frame")

  #>>stats only----

  posterior1_stats <- posterior_g_comp(
    x = nb_bma_cov,
    doses = attr(nb_bma_cov, "doses"),
    reference_dose = NULL,
    prob = c(.025, .975),
    return_stats = TRUE,
    return_samples = FALSE,
    new_data = nb_monotone_incr_cov
  )

  expect_true(is.list(posterior1_stats))
  expect_named(posterior1_stats, c("stats", "samples"))
  expect_identical(posterior1_stats$stats, posterior1$stats)
  expect_true(is.null(posterior1_stats$samples))

  #>>samples only----

  posterior1_samples <- posterior_g_comp(
    x = nb_bma_cov,
    doses = attr(nb_bma_cov, "doses"),
    reference_dose = NULL,
    prob = c(.025, .975),
    return_stats = FALSE,
    return_samples = TRUE,
    new_data = nb_monotone_incr_cov
  )

  expect_true(is.list(posterior1_samples))
  expect_named(posterior1_samples, c("stats", "samples"))
  expect_true(is.null(posterior1_samples$stats))
  expect_identical(posterior1_samples$samples, posterior1$samples)

  #>reference_dose == [first dose], reference_type == "difference"----

  posterior1a <- posterior_g_comp(
    x = nb_bma_cov,
    doses = attr(nb_bma_cov, "doses"),
    reference_dose = attr(nb_bma_cov, "doses")[1],
    prob = c(.025, .975),
    return_stats = TRUE,
    return_samples = TRUE,
    new_data = nb_monotone_incr_cov,
    reference_type = "difference"
  )

  expect_true(is.list(posterior1a))
  expect_named(posterior1a, c("stats", "samples"))
  expect_failure(expect_s3_class(posterior1a$stats, NA))
  expect_s3_class(posterior1a$stats, "data.frame")
  expect_failure(expect_s3_class(posterior1a$samples, NA))
  expect_s3_class(posterior1a$samples, "data.frame")

  #>reference_dose == [first dose], reference_type == "ratio"----

  posterior1b <- posterior_g_comp(
    x = nb_bma_cov,
    doses = attr(nb_bma_cov, "doses"),
    reference_dose = attr(nb_bma_cov, "doses")[1],
    prob = c(.025, .975),
    return_stats = TRUE,
    return_samples = TRUE,
    new_data = nb_monotone_incr_cov,
    reference_type = "ratio"
  )

  expect_true(is.list(posterior1b))
  expect_named(posterior1b, c("stats", "samples"))
  expect_failure(expect_s3_class(posterior1b$stats, NA))
  expect_s3_class(posterior1b$stats, "data.frame")
  expect_failure(expect_s3_class(posterior1b$samples, NA))
  expect_s3_class(posterior1b$samples, "data.frame")

  # nolint start
  # #contrast----
  #
  # #>reference_dose == NULL----
  #
  # posterior2 <- posterior_g_comp(
  #   x = nb_bma_cov,
  #   doses = attr(nb_bma_cov, "doses"),
  #   reference_dose = NULL,
  #   prob = c(.025, .975),
  #   return_stats = TRUE,
  #   return_samples = TRUE,
  #   contrast = matrix(1, 1, 1)
  # )
  #
  # expect_true(is.list(posterior2))
  # expect_named(posterior2, c("stats", "samples"))
  # expect_failure(expect_s3_class(posterior2$stats, NA))
  # expect_s3_class(posterior2$stats, "data.frame")
  # expect_failure(expect_s3_class(posterior2$samples, NA))
  # expect_s3_class(posterior2$samples, "data.frame")
  #
  # #>>stats only----
  #
  # posterior2_stats <- posterior_g_comp(
  #   x = nb_bma_cov,
  #   doses = attr(nb_bma_cov, "doses"),
  #   reference_dose = NULL,
  #   prob = c(.025, .975),
  #   return_stats = TRUE,
  #   return_samples = FALSE,
  #   contrast = matrix(1, 1, 1)
  # )
  #
  # expect_true(is.list(posterior2_stats))
  # expect_named(posterior2_stats, c("stats", "samples"))
  # expect_identical(posterior2_stats$stats, posterior2$stats)
  # expect_true(is.null(posterior2_stats$samples))
  #
  # #>>samples only----
  #
  # posterior2_samples <- posterior_g_comp(
  #   x = nb_bma_cov,
  #   doses = attr(nb_bma_cov, "doses"),
  #   reference_dose = NULL,
  #   prob = c(.025, .975),
  #   return_stats = FALSE,
  #   return_samples = TRUE,
  #   contrast = matrix(1, 1, 1)
  # )
  #
  # expect_true(is.list(posterior2_samples))
  # expect_named(posterior2_samples, c("stats", "samples"))
  # expect_true(is.null(posterior2_samples$stats))
  # expect_identical(posterior2_samples$samples, posterior2$samples)
  #
  # #>reference_dose == [first dose], reference_type == "difference"----
  #
  # posterior2a <- posterior_g_comp(
  #   x = nb_bma_cov,
  #   doses = attr(nb_bma_cov, "doses"),
  #   reference_dose = attr(nb_bma_cov, "doses")[1],
  #   prob = c(.025, .975),
  #   return_stats = TRUE,
  #   return_samples = TRUE,
  #   contrast = matrix(1, 1, 1),
  #   reference_type = "difference"
  # )
  #
  # expect_true(is.list(posterior2a))
  # expect_named(posterior2a, c("stats", "samples"))
  # expect_failure(expect_s3_class(posterior2a$stats, NA))
  # expect_s3_class(posterior2a$stats, "data.frame")
  # expect_failure(expect_s3_class(posterior2a$samples, NA))
  # expect_s3_class(posterior2a$samples, "data.frame")
  #
  # #>reference_dose == [first dose], reference_type == "ratio"----
  #
  # posterior2b <- posterior_g_comp(
  #   x = nb_bma_cov,
  #   doses = attr(nb_bma_cov, "doses"),
  #   reference_dose = attr(nb_bma_cov, "doses")[1],
  #   prob = c(.025, .975),
  #   return_stats = TRUE,
  #   return_samples = TRUE,
  #   contrast = matrix(1, 1, 1),
  #   reference_type = "ratio"
  # )
  #
  # expect_true(is.list(posterior2b))
  # expect_named(posterior2b, c("stats", "samples"))
  # expect_failure(expect_s3_class(posterior2b$stats, NA))
  # expect_s3_class(posterior2b$stats, "data.frame")
  # expect_failure(expect_s3_class(posterior2b$samples, NA))
  # expect_s3_class(posterior2b$samples, "data.frame")
  # nolint end
})

test_that("posterior_g_comp works against an S3 object of class beaver_mcmc, produces an object with correct properties", { # nolint

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

  posterior1 <- posterior_g_comp(
    x = nb_emax_model_cov_samples_updatedattr,
    doses = attr(nb_emax_model_cov_samples_updatedattr, "doses"),
    reference_dose = NULL,
    prob = c(.025, .975),
    return_stats = TRUE,
    return_samples = TRUE,
    new_data = nb_monotone_incr_cov
  )

  expect_true(is.list(posterior1))
  expect_named(posterior1, c("stats", "samples"))
  expect_failure(expect_s3_class(posterior1$stats, NA))
  expect_s3_class(posterior1$stats, "data.frame")
  expect_failure(expect_s3_class(posterior1$samples, NA))
  expect_s3_class(posterior1$samples, "data.frame")

  #>>stats only----

  posterior1_stats <- posterior_g_comp(
    x = nb_emax_model_cov_samples_updatedattr,
    doses = attr(nb_emax_model_cov_samples_updatedattr, "doses"),
    reference_dose = NULL,
    prob = c(.025, .975),
    return_stats = TRUE,
    return_samples = FALSE,
    new_data = nb_monotone_incr_cov
  )

  expect_true(is.list(posterior1_stats))
  expect_named(posterior1_stats, c("stats", "samples"))
  expect_identical(posterior1_stats$stats, posterior1$stats)
  expect_true(is.null(posterior1_stats$samples))

  #>>samples only----

  posterior1_samples <- posterior_g_comp(
    x = nb_emax_model_cov_samples_updatedattr,
    doses = attr(nb_emax_model_cov_samples_updatedattr, "doses"),
    reference_dose = NULL,
    prob = c(.025, .975),
    return_stats = FALSE,
    return_samples = TRUE,
    new_data = nb_monotone_incr_cov
  )

  expect_true(is.list(posterior1_samples))
  expect_named(posterior1_samples, c("stats", "samples"))
  expect_true(is.null(posterior1_samples$stats))
  expect_identical(posterior1_samples$samples, posterior1$samples)

  #>reference_dose == [first dose], reference_type == "difference"----

  posterior1a <- posterior_g_comp(
    x = nb_emax_model_cov_samples_updatedattr,
    doses = attr(nb_emax_model_cov_samples_updatedattr, "doses"),
    reference_dose = attr(nb_emax_model_cov_samples_updatedattr, "doses")[1],
    prob = c(.025, .975),
    return_stats = TRUE,
    return_samples = TRUE,
    new_data = nb_monotone_incr_cov,
    reference_type = "difference"
  )

  expect_true(is.list(posterior1a))
  expect_named(posterior1a, c("stats", "samples"))
  expect_failure(expect_s3_class(posterior1a$stats, NA))
  expect_s3_class(posterior1a$stats, "data.frame")
  expect_failure(expect_s3_class(posterior1a$samples, NA))
  expect_s3_class(posterior1a$samples, "data.frame")

  #>reference_dose == [first dose], reference_type == "ratio"----

  posterior1b <- posterior_g_comp(
    x = nb_emax_model_cov_samples_updatedattr,
    doses = attr(nb_emax_model_cov_samples_updatedattr, "doses"),
    reference_dose = attr(nb_emax_model_cov_samples_updatedattr, "doses")[1],
    prob = c(.025, .975),
    return_stats = TRUE,
    return_samples = TRUE,
    new_data = nb_monotone_incr_cov,
    reference_type = "ratio"
  )

  expect_true(is.list(posterior1b))
  expect_named(posterior1b, c("stats", "samples"))
  expect_failure(expect_s3_class(posterior1b$stats, NA))
  expect_s3_class(posterior1b$stats, "data.frame")
  expect_failure(expect_s3_class(posterior1b$samples, NA))
  expect_s3_class(posterior1b$samples, "data.frame")

  # nolint start
  # #contrast----
  #
  # #>reference_dose == NULL----
  #
  # posterior2 <- posterior_g_comp(
  #   x = nb_emax_model_cov_samples_updatedattr,
  #   doses = attr(nb_emax_model_cov_samples_updatedattr, "doses"),
  #   reference_dose = NULL,
  #   prob = c(.025, .975),
  #   return_stats = TRUE,
  #   return_samples = TRUE,
  #   contrast = matrix(1, 1, 1)
  # )
  #
  # expect_true(is.list(posterior2))
  # expect_named(posterior2, c("stats", "samples"))
  # expect_failure(expect_s3_class(posterior2$stats, NA))
  # expect_s3_class(posterior2$stats, "data.frame")
  # expect_failure(expect_s3_class(posterior2$samples, NA))
  # expect_s3_class(posterior2$samples, "data.frame")
  #
  # #>>stats only----
  #
  # posterior2_stats <- posterior_g_comp(
  #   x = nb_emax_model_cov_samples_updatedattr,
  #   doses = attr(nb_emax_model_cov_samples_updatedattr, "doses"),
  #   reference_dose = NULL,
  #   prob = c(.025, .975),
  #   return_stats = TRUE,
  #   return_samples = FALSE,
  #   contrast = matrix(1, 1, 1)
  # )
  #
  # expect_true(is.list(posterior2_stats))
  # expect_named(posterior2_stats, c("stats", "samples"))
  # expect_identical(posterior2_stats$stats, posterior2$stats)
  # expect_true(is.null(posterior2_stats$samples))
  #
  # #>>samples only----
  #
  # posterior2_samples <- posterior_g_comp(
  #   x = nb_emax_model_cov_samples_updatedattr,
  #   doses = attr(nb_emax_model_cov_samples_updatedattr, "doses"),
  #   reference_dose = NULL,
  #   prob = c(.025, .975),
  #   return_stats = FALSE,
  #   return_samples = TRUE,
  #   contrast = matrix(1, 1, 1)
  # )
  #
  # expect_true(is.list(posterior2_samples))
  # expect_named(posterior2_samples, c("stats", "samples"))
  # expect_true(is.null(posterior2_samples$stats))
  # expect_identical(posterior2_samples$samples, posterior2$samples)
  #
  # #>reference_dose == [first dose], reference_type == "difference"----
  #
  # posterior2a <- posterior_g_comp(
  #   x = nb_emax_model_cov_samples_updatedattr,
  #   doses = attr(nb_emax_model_cov_samples_updatedattr, "doses"),
  #   reference_dose = attr(nb_emax_model_cov_samples_updatedattr, "doses")[1],
  #   prob = c(.025, .975),
  #   return_stats = TRUE,
  #   return_samples = TRUE,
  #   contrast = matrix(1, 1, 1),
  #   reference_type = "difference"
  # )
  #
  # expect_true(is.list(posterior2a))
  # expect_named(posterior2a, c("stats", "samples"))
  # expect_failure(expect_s3_class(posterior2a$stats, NA))
  # expect_s3_class(posterior2a$stats, "data.frame")
  # expect_failure(expect_s3_class(posterior2a$samples, NA))
  # expect_s3_class(posterior2a$samples, "data.frame")
  #
  # #>reference_dose == [first dose], reference_type == "ratio"----
  #
  # posterior2b <- posterior_g_comp(
  #   x = nb_emax_model_cov_samples_updatedattr,
  #   doses = attr(nb_emax_model_cov_samples_updatedattr, "doses"),
  #   reference_dose = attr(nb_emax_model_cov_samples_updatedattr, "doses")[1],
  #   prob = c(.025, .975),
  #   return_stats = TRUE,
  #   return_samples = TRUE,
  #   contrast = matrix(1, 1, 1),
  #   reference_type = "ratio"
  # )
  #
  # expect_true(is.list(posterior2b))
  # expect_named(posterior2b, c("stats", "samples"))
  # expect_failure(expect_s3_class(posterior2b$stats, NA))
  # expect_s3_class(posterior2b$stats, "data.frame")
  # expect_failure(expect_s3_class(posterior2b$samples, NA))
  # expect_s3_class(posterior2b$samples, "data.frame")
  # nolint end
})
