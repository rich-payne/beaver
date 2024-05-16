
test_that("pr_eoi works against an S3 object of class beaver_mcmc, produces an object with correct properties", { # nolint

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

  eoi <- c(9, 35, 60, 110)
  eoia <- c(25, 50, 100)
  eoib <- c(4, 6, 12)

  #new_data----

  contrast1 <- get_contrast(
    x = nb_indep_model_samples_updatedattr,
    new_data = nb_monotone_incr_new,
    contrast = NULL
  )

  #>reference_dose == NULL----

  samps1 <- yodel::posterior(
    x = nb_indep_model_samples_updatedattr,
    return_stats = FALSE,
    return_samples = TRUE,
    new_data = nb_monotone_incr_new
  )$samples

  pr_eoi1 <- pr_eoi(
    x = nb_indep_model_samples_updatedattr,
    eoi = eoi,
    new_data = nb_monotone_incr_new
  )

  pr_eoi_checks(
    pr_eoi = pr_eoi1,
    samples = nb_indep_model_samples_updatedattr,
    contrast = contrast1,
    posteriors = samps1,
    eoi = eoi
  )

  #>>direction == "less"----

  pr_eoi1_less <- pr_eoi(
    x = nb_indep_model_samples_updatedattr,
    eoi = eoi,
    new_data = nb_monotone_incr_new,
    direction = "less"
  )

  expect_identical(
    pr_eoi1_less %>%
      dplyr::select(- c(prob, direction)),
    pr_eoi1 %>%
      dplyr::select(- c(prob, direction)),
  )
  expect_equal(
    pr_eoi1_less %>%
      dplyr::mutate(prob1 = 1 - prob) %>%
      dplyr::pull(prob1),
    pr_eoi1 %>%
      dplyr::pull(prob)
  )
  expect_identical(
    pr_eoi1_less %>%
      dplyr::ungroup() %>%
      dplyr::distinct(direction) %>%
      dplyr::pull(direction),
    "less"
  )

  #>reference_dose == [first dose], reference_type == "difference"----

  samps1a <- yodel::posterior(
    x = nb_indep_model_samples_updatedattr,
    reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
    return_stats = FALSE,
    return_samples = TRUE,
    new_data = nb_monotone_incr_new,
    reference_type = "difference"
  )$samples

  expect_no_error(
    pr_eoi(
      x = nb_indep_model_samples_updatedattr,
      reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
      eoi = eoia,
      new_data = nb_monotone_incr_new,
      reference_type = "difference"
    )
  )

  pr_eoi1a <- pr_eoi(
    x = nb_indep_model_samples_updatedattr,
    reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
    eoi = eoia,
    new_data = nb_monotone_incr_new,
    reference_type = "difference"
  )

  pr_eoi_checks(
    pr_eoi = pr_eoi1a,
    samples = nb_indep_model_samples_updatedattr,
    contrast = contrast1,
    posteriors = samps1a,
    eoi = eoia
  )

  #>reference_dose == [first dose], reference_type == "ratio"----

  samps1b <- yodel::posterior(
    x = nb_indep_model_samples_updatedattr,
    reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
    return_stats = FALSE,
    return_samples = TRUE,
    new_data = nb_monotone_incr_new,
    reference_type = "ratio"
  )$samples

  pr_eoi1b <- pr_eoi(
    x = nb_indep_model_samples_updatedattr,
    reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
    eoi = eoib,
    new_data = nb_monotone_incr_new,
    reference_type = "ratio"
  )

  pr_eoi_checks(
    pr_eoi = pr_eoi1b,
    samples = nb_indep_model_samples_updatedattr,
    contrast = contrast1,
    posteriors = samps1b,
    eoi = eoib
  )

  #contrast----

  contrast2 <- get_contrast(
    x = nb_indep_model_samples_updatedattr,
    new_data = NULL,
    contrast = matrix(1, 1, 1)
  )

  #>reference_dose == NULL----

  samps2 <- yodel::posterior(
    x = nb_indep_model_samples_updatedattr,
    return_stats = FALSE,
    return_samples = TRUE,
    contrast = matrix(1, 1, 1)
  )$samples

  pr_eoi2 <- pr_eoi(
    x = nb_indep_model_samples_updatedattr,
    eoi = eoi,
    contrast = matrix(1, 1, 1)
  )

  pr_eoi_checks(
    pr_eoi = pr_eoi2,
    samples = nb_indep_model_samples_updatedattr,
    contrast = contrast2,
    posteriors = samps2,
    eoi = eoi
  )

  #>>direction == "less"----

  pr_eoi2_less <- pr_eoi(
    x = nb_indep_model_samples_updatedattr,
    eoi = eoi,
    contrast = matrix(1, 1, 1),
    direction = "less"
  )

  expect_identical(
    pr_eoi2_less %>%
      dplyr::select(- c(prob, direction)),
    pr_eoi2 %>%
      dplyr::select(- c(prob, direction)),
  )
  expect_equal(
    pr_eoi2_less %>%
      dplyr::mutate(prob1 = 1 - prob) %>%
      dplyr::pull(prob1),
    pr_eoi2 %>%
      dplyr::pull(prob)
  )
  expect_identical(
    pr_eoi2_less %>%
      dplyr::ungroup() %>%
      dplyr::distinct(direction) %>%
      dplyr::pull(direction),
    "less"
  )

  #>reference_dose == [first dose], reference_type == "difference"----

  samps2a <- yodel::posterior(
    x = nb_indep_model_samples_updatedattr,
    reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
    return_stats = FALSE,
    return_samples = TRUE,
    contrast = matrix(1, 1, 1),
    reference_type = "difference"
  )$samples

  pr_eoi2a <- pr_eoi(
    x = nb_indep_model_samples_updatedattr,
    reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
    eoi = eoia,
    contrast = matrix(1, 1, 1),
    reference_type = "difference"
  )

  pr_eoi_checks(
    pr_eoi = pr_eoi2a,
    samples = nb_indep_model_samples_updatedattr,
    contrast = contrast2,
    posteriors = samps2a,
    eoi = eoia
  )

  #>reference_dose == [first dose], reference_type == "ratio"----

  samps2b <- yodel::posterior(
    x = nb_indep_model_samples_updatedattr,
    reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
    return_stats = FALSE,
    return_samples = TRUE,
    contrast = matrix(1, 1, 1),
    reference_type = "ratio"
  )$samples

  pr_eoi2b <- pr_eoi(
    x = nb_indep_model_samples_updatedattr,
    reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
    eoi = eoib,
    contrast = matrix(1, 1, 1),
    reference_type = "ratio"
  )

  pr_eoi_checks(
    pr_eoi = pr_eoi2b,
    samples = nb_indep_model_samples_updatedattr,
    contrast = contrast2,
    posteriors = samps2b,
    eoi = eoib
  )
})

test_that("pr_eoi works against an S3 object of class beaver_mcmc_bma, produces an object with correct properties", { # nolint

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
  expect_no_error(
    checkmate::assertSubset(
      c("formula", "doses"),
      names(attributes(nb_bma))
    )
  )

  eoi <- c(9, 35, 60, 110)
  eoia <- c(25, 50, 100)
  eoib <- c(4, 6, 12)

  #new_data----

  contrast1 <- get_contrast(
    x = nb_bma,
    new_data = nb_monotone_incr_new,
    contrast = NULL
  )

  #>reference_dose == NULL----

  samps1 <- yodel::posterior(
    x = nb_bma,
    return_stats = FALSE,
    return_samples = TRUE,
    new_data = nb_monotone_incr_new
  )$samples

  pr_eoi1 <- pr_eoi(
    x = nb_bma,
    eoi = eoi,
    new_data = nb_monotone_incr_new
  )

  pr_eoi_checks(
    pr_eoi = pr_eoi1,
    samples = nb_bma,
    contrast = contrast1,
    posteriors = samps1,
    eoi = eoi
  )

  #>>direction == "less"----

  pr_eoi1_less <- pr_eoi(
    x = nb_bma,
    eoi = eoi,
    new_data = nb_monotone_incr_new,
    direction = "less"
  )

  expect_identical(
    pr_eoi1_less %>%
      dplyr::select(- c(prob, direction)),
    pr_eoi1 %>%
      dplyr::select(- c(prob, direction)),
  )
  expect_equal(
    pr_eoi1_less %>%
      dplyr::mutate(prob1 = 1 - prob) %>%
      dplyr::pull(prob1),
    pr_eoi1 %>%
      dplyr::pull(prob)
  )
  expect_identical(
    pr_eoi1_less %>%
      dplyr::ungroup() %>%
      dplyr::distinct(direction) %>%
      dplyr::pull(direction),
    "less"
  )

  #>reference_dose == [first dose], reference_type == "difference"----

  samps1a <- yodel::posterior(
    x = nb_bma,
    reference_dose = attr(nb_bma, "doses")[1],
    return_stats = FALSE,
    return_samples = TRUE,
    new_data = nb_monotone_incr_new,
    reference_type = "difference"
  )$samples

  pr_eoi1a <- pr_eoi(
    x = nb_bma,
    reference_dose = attr(nb_bma, "doses")[1],
    eoi = eoia,
    new_data = nb_monotone_incr_new,
    reference_type = "difference"
  )

  pr_eoi_checks(
    pr_eoi = pr_eoi1a,
    samples = nb_bma,
    contrast = contrast1,
    posteriors = samps1a,
    eoi = eoia
  )

  #>reference_dose == [first dose], reference_type == "ratio"----

  samps1b <- yodel::posterior(
    x = nb_bma,
    reference_dose = attr(nb_bma, "doses")[1],
    return_stats = FALSE,
    return_samples = TRUE,
    new_data = nb_monotone_incr_new,
    reference_type = "ratio"
  )$samples

  pr_eoi1b <- pr_eoi(
    x = nb_bma,
    reference_dose = attr(nb_bma, "doses")[1],
    eoi = eoib,
    new_data = nb_monotone_incr_new,
    reference_type = "ratio"
  )

  pr_eoi_checks(
    pr_eoi = pr_eoi1b,
    samples = nb_bma,
    contrast = contrast1,
    posteriors = samps1b,
    eoi = eoib
  )

  #contrast----

  contrast2 <- get_contrast(
    x = nb_bma,
    new_data = NULL,
    contrast = matrix(1, 1, 1)
  )

  #>reference_dose == NULL----

  samps2 <- yodel::posterior(
    x = nb_bma,
    return_stats = FALSE,
    return_samples = TRUE,
    contrast = matrix(1, 1, 1)
  )$samples

  pr_eoi2 <- pr_eoi(
    x = nb_bma,
    eoi = eoi,
    contrast = matrix(1, 1, 1)
  )

  pr_eoi_checks(
    pr_eoi = pr_eoi2,
    samples = nb_bma,
    contrast = contrast2,
    posteriors = samps2,
    eoi = eoi
  )

  #>>direction == "less"----

  pr_eoi2_less <- pr_eoi(
    x = nb_bma,
    eoi = eoi,
    contrast = matrix(1, 1, 1),
    direction = "less"
  )

  expect_identical(
    pr_eoi2_less %>%
      dplyr::select(- c(prob, direction)),
    pr_eoi2 %>%
      dplyr::select(- c(prob, direction)),
  )
  expect_equal(
    pr_eoi2_less %>%
      dplyr::mutate(prob1 = 1 - prob) %>%
      dplyr::pull(prob1),
    pr_eoi2 %>%
      dplyr::pull(prob)
  )
  expect_identical(
    pr_eoi2_less %>%
      dplyr::ungroup() %>%
      dplyr::distinct(direction) %>%
      dplyr::pull(direction),
    "less"
  )

  #>reference_dose == [first dose], reference_type == "difference"----

  samps2a <- yodel::posterior(
    x = nb_bma,
    reference_dose = attr(nb_bma, "doses")[1],
    return_stats = FALSE,
    return_samples = TRUE,
    contrast = matrix(1, 1, 1),
    reference_type = "difference"
  )$samples

  pr_eoi2a <- pr_eoi(
    x = nb_bma,
    reference_dose = attr(nb_bma, "doses")[1],
    eoi = eoia,
    contrast = matrix(1, 1, 1),
    reference_type = "difference"
  )

  pr_eoi_checks(
    pr_eoi = pr_eoi2a,
    samples = nb_bma,
    contrast = contrast2,
    posteriors = samps2a,
    eoi = eoia
  )

  #>reference_dose == [first dose], reference_type == "ratio"----

  samps2b <- yodel::posterior(
    x = nb_bma,
    reference_dose = attr(nb_bma, "doses")[1],
    return_stats = FALSE,
    return_samples = TRUE,
    contrast = matrix(1, 1, 1),
    reference_type = "ratio"
  )$samples

  pr_eoi2b <- pr_eoi(
    x = nb_bma,
    reference_dose = attr(nb_bma, "doses")[1],
    eoi = eoib,
    contrast = matrix(1, 1, 1),
    reference_type = "ratio"
  )

  pr_eoi_checks(
    pr_eoi = pr_eoi2b,
    samples = nb_bma,
    contrast = contrast2,
    posteriors = samps2b,
    eoi = eoib
  )
})

test_that("pr_eoi_impl works against an S3 object of class beaver_mcmc, produces an object with correct properties", { # nolint

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

  eoi <- 60
  eoia <- 50
  eoib <- 6

  #new_data----

  contrast1 <- get_contrast(
    x = nb_indep_model_samples_updatedattr,
    new_data = nb_monotone_incr_new,
    contrast = NULL
  )

  #>reference_dose == NULL----

  samps1 <- yodel::posterior(
    x = nb_indep_model_samples_updatedattr,
    return_stats = FALSE,
    return_samples = TRUE,
    new_data = nb_monotone_incr_new
  )$samples

  expect_no_error(
    pr_eoi_impl(
      x = samps1,
      eoi = eoi
    )
  )

  pr_eoi1 <- pr_eoi_impl(
    x = samps1,
    eoi = eoi
  )

  pr_eoi_checks(
    pr_eoi = pr_eoi1,
    samples = nb_indep_model_samples_updatedattr,
    contrast = contrast1,
    posteriors = samps1,
    eoi = eoi
  )

  #>reference_dose == [first dose], reference_type == "difference"----

  samps1a <- yodel::posterior(
    x = nb_indep_model_samples_updatedattr,
    reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
    return_stats = FALSE,
    return_samples = TRUE,
    new_data = nb_monotone_incr_new,
    reference_type = "difference"
  )$samples

  expect_no_error(
    pr_eoi_impl(
      x = samps1a,
      eoi = eoia
    )
  )

  pr_eoi1a <- pr_eoi_impl(
    x = samps1a,
    eoi = eoia
  )

  pr_eoi_checks(
    pr_eoi = pr_eoi1a,
    samples = nb_indep_model_samples_updatedattr,
    contrast = contrast1,
    posteriors = samps1a,
    eoi = eoia
  )

  #>reference_dose == [first dose], reference_type == "ratio"----

  samps1b <- yodel::posterior(
    x = nb_indep_model_samples_updatedattr,
    reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
    return_stats = FALSE,
    return_samples = TRUE,
    new_data = nb_monotone_incr_new,
    reference_type = "ratio"
  )$samples

  expect_no_error(
    pr_eoi_impl(
      x = samps1b,
      eoi = eoib
    )
  )

  pr_eoi1b <- pr_eoi_impl(
    x = samps1b,
    eoi = eoib
  )

  pr_eoi_checks(
    pr_eoi = pr_eoi1b,
    samples = nb_indep_model_samples_updatedattr,
    contrast = contrast1,
    posteriors = samps1b,
    eoi = eoib
  )

  #contrast----

  contrast2 <- get_contrast(
    x = nb_indep_model_samples_updatedattr,
    new_data = NULL,
    contrast = matrix(1, 1, 1)
  )

  #>reference_dose == NULL----

  samps2 <- yodel::posterior(
    x = nb_indep_model_samples_updatedattr,
    return_stats = FALSE,
    return_samples = TRUE,
    contrast = matrix(1, 1, 1)
  )$samples

  expect_no_error(
    pr_eoi_impl(
      x = samps2,
      eoi = eoi
    )
  )

  pr_eoi2 <- pr_eoi_impl(
    x = samps2,
    eoi = eoi
  )

  pr_eoi_checks(
    pr_eoi = pr_eoi2,
    samples = nb_indep_model_samples_updatedattr,
    contrast = contrast2,
    posteriors = samps2,
    eoi = eoi
  )

  #>reference_dose == [first dose], reference_type == "difference"----

  samps2a <- yodel::posterior(
    x = nb_indep_model_samples_updatedattr,
    reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
    return_stats = FALSE,
    return_samples = TRUE,
    contrast = matrix(1, 1, 1),
    reference_type = "difference"
  )$samples

  expect_no_error(
    pr_eoi_impl(
      x = samps2a,
      eoi = eoia
    )
  )

  pr_eoi2a <- pr_eoi_impl(
    x = samps2a,
    eoi = eoia
  )

  pr_eoi_checks(
    pr_eoi = pr_eoi2a,
    samples = nb_indep_model_samples_updatedattr,
    contrast = contrast2,
    posteriors = samps2a,
    eoi = eoia
  )

  #>reference_dose == [first dose], reference_type == "ratio"----

  samps2b <- yodel::posterior(
    x = nb_indep_model_samples_updatedattr,
    reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
    return_stats = FALSE,
    return_samples = TRUE,
    contrast = matrix(1, 1, 1),
    reference_type = "ratio"
  )$samples

  expect_no_error(
    pr_eoi_impl(
      x = samps2b,
      eoi = eoib
    )
  )

  pr_eoi2b <- pr_eoi_impl(
    x = samps2b,
    eoi = eoib
  )

  pr_eoi_checks(
    pr_eoi = pr_eoi2b,
    samples = nb_indep_model_samples_updatedattr,
    contrast = contrast2,
    posteriors = samps2b,
    eoi = eoib
  )
})

test_that("pr_eoi_impl works against an S3 object of class beaver_mcmc_bma, produces an object with correct properties", { # nolint

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
  expect_no_error(
    checkmate::assertSubset(
      c("formula", "doses"),
      names(attributes(nb_bma))
    )
  )

  eoi <- 60
  eoia <- 50
  eoib <- 6

  #new_data----

  contrast1 <- get_contrast(
    x = nb_bma,
    new_data = nb_monotone_incr_new,
    contrast = NULL
  )

  #>reference_dose == NULL----

  samps1 <- yodel::posterior(
    x = nb_bma,
    return_stats = FALSE,
    return_samples = TRUE,
    new_data = nb_monotone_incr_new
  )$samples

  expect_no_error(
    pr_eoi_impl(
      x = samps1,
      eoi = eoi
    )
  )

  pr_eoi1 <- pr_eoi_impl(
    x = samps1,
    eoi = eoi
  )

  pr_eoi_checks(
    pr_eoi = pr_eoi1,
    samples = nb_bma,
    contrast = contrast1,
    posteriors = samps1,
    eoi = eoi
  )

  #>reference_dose == [first dose], reference_type == "difference"----

  samps1a <- yodel::posterior(
    x = nb_bma,
    reference_dose = attr(nb_bma, "doses")[1],
    return_stats = FALSE,
    return_samples = TRUE,
    new_data = nb_monotone_incr_new,
    reference_type = "difference"
  )$samples

  expect_no_error(
    pr_eoi_impl(
      x = samps1a,
      eoi = eoia
    )
  )

  pr_eoi1a <- pr_eoi_impl(
    x = samps1a,
    eoi = eoia
  )

  pr_eoi_checks(
    pr_eoi = pr_eoi1a,
    samples = nb_bma,
    contrast = contrast1,
    posteriors = samps1a,
    eoi = eoia
  )

  #>reference_dose == [first dose], reference_type == "ratio"----

  samps1b <- yodel::posterior(
    x = nb_bma,
    reference_dose = attr(nb_bma, "doses")[1],
    return_stats = FALSE,
    return_samples = TRUE,
    new_data = nb_monotone_incr_new,
    reference_type = "ratio"
  )$samples

  expect_no_error(
    pr_eoi_impl(
      x = samps1b,
      eoi = eoib
    )
  )

  pr_eoi1b <- pr_eoi_impl(
    x = samps1b,
    eoi = eoib
  )

  pr_eoi_checks(
    pr_eoi = pr_eoi1b,
    samples = nb_bma,
    contrast = contrast1,
    posteriors = samps1b,
    eoi = eoib
  )

  #contrast----

  contrast2 <- get_contrast(
    x = nb_bma,
    new_data = NULL,
    contrast = matrix(1, 1, 1)
  )

  #>reference_dose == NULL----

  samps2 <- yodel::posterior(
    x = nb_bma,
    return_stats = FALSE,
    return_samples = TRUE,
    contrast = matrix(1, 1, 1)
  )$samples

  expect_no_error(
    pr_eoi_impl(
      x = samps2,
      eoi = eoi
    )
  )

  pr_eoi2 <- pr_eoi_impl(
    x = samps2,
    eoi = eoi
  )

  pr_eoi_checks(
    pr_eoi = pr_eoi2,
    samples = nb_bma,
    contrast = contrast2,
    posteriors = samps2,
    eoi = eoi
  )

  #>reference_dose == [first dose], reference_type == "difference"----

  samps2a <- yodel::posterior(
    x = nb_bma,
    reference_dose = attr(nb_bma, "doses")[1],
    return_stats = FALSE,
    return_samples = TRUE,
    contrast = matrix(1, 1, 1),
    reference_type = "difference"
  )$samples

  expect_no_error(
    pr_eoi_impl(
      x = samps2a,
      eoi = eoia
    )
  )

  pr_eoi2a <- pr_eoi_impl(
    x = samps2a,
    eoi = eoia
  )

  pr_eoi_checks(
    pr_eoi = pr_eoi2a,
    samples = nb_bma,
    contrast = contrast2,
    posteriors = samps2a,
    eoi = eoia
  )

  #>reference_dose == [first dose], reference_type == "ratio"----

  samps2b <- yodel::posterior(
    x = nb_bma,
    reference_dose = attr(nb_bma, "doses")[1],
    return_stats = FALSE,
    return_samples = TRUE,
    contrast = matrix(1, 1, 1),
    reference_type = "ratio"
  )$samples

  expect_no_error(
    pr_eoi_impl(
      x = samps2b,
      eoi = eoib
    )
  )

  pr_eoi2b <- pr_eoi_impl(
    x = samps2b,
    eoi = eoib
  )

  pr_eoi_checks(
    pr_eoi = pr_eoi2b,
    samples = nb_bma,
    contrast = contrast2,
    posteriors = samps2b,
    eoi = eoib
  )
})

test_that("apply_direction works against an S3 object of class data.frame, produces an object with correct properties", { # nolint

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

  eoi <- 60
  eoia <- 50
  eoib <- 6

  #new_data----

  contrast1 <- get_contrast(
    x = nb_indep_model_samples_updatedattr,
    new_data = nb_monotone_incr_new,
    contrast = NULL
  )

  #>reference_dose == NULL----

  samps1 <- yodel::posterior(
    x = nb_indep_model_samples_updatedattr,
    return_stats = FALSE,
    return_samples = TRUE,
    new_data = nb_monotone_incr_new
  )$samples

  pr_eoi_impl1 <- purrr::map_dfr(eoi, pr_eoi_impl, x = samps1)

  expect_failure(expect_s3_class(pr_eoi_impl1, NA))
  expect_s3_class(pr_eoi_impl1, "data.frame")

  expect_no_error(
    apply_direction(
      x = pr_eoi_impl1,
      direction = "greater"
    )
  )

  direction1 <- apply_direction(
    x = pr_eoi_impl1,
    direction = "greater"
  )

  expect_identical(
    direction1 %>%
      dplyr::select(- c(direction)),
    pr_eoi_impl1,
  )
  expect_identical(
    direction1 %>%
      dplyr::ungroup() %>%
      dplyr::distinct(direction) %>%
      dplyr::pull(direction),
    "greater"
  )

  #>>direction == "less"----

  expect_no_error(
    apply_direction(
      x = pr_eoi_impl1,
      direction = "less"
    )
  )

  direction1_less <- apply_direction(
    x = pr_eoi_impl1,
    direction = "less"
  )

  expect_identical(
    direction1_less %>%
      dplyr::select(- c(prob, direction)),
    direction1 %>%
      dplyr::select(- c(prob, direction)),
  )
  expect_equal(
    direction1_less %>%
      dplyr::mutate(prob1 = 1 - prob) %>%
      dplyr::pull(prob1),
    direction1 %>%
      dplyr::pull(prob)
  )
  expect_identical(
    direction1_less %>%
      dplyr::ungroup() %>%
      dplyr::distinct(direction) %>%
      dplyr::pull(direction),
    "less"
  )

  #>reference_dose == [first dose], reference_type == "difference"----

  samps1a <- yodel::posterior(
    x = nb_indep_model_samples_updatedattr,
    reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
    return_stats = FALSE,
    return_samples = TRUE,
    new_data = nb_monotone_incr_new,
    reference_type = "difference"
  )$samples

  pr_eoi_impl1a <- purrr::map_dfr(eoia, pr_eoi_impl, x = samps1a)

  expect_failure(expect_s3_class(pr_eoi_impl1a, NA))
  expect_s3_class(pr_eoi_impl1a, "data.frame")

  expect_no_error(
    apply_direction(
      x = pr_eoi_impl1a,
      direction = "greater"
    )
  )

  direction1a <- apply_direction(
    x = pr_eoi_impl1a,
    direction = "greater"
  )

  expect_identical(
    direction1a %>%
      dplyr::select(- c(direction)),
    pr_eoi_impl1a,
  )
  expect_identical(
    direction1a %>%
      dplyr::ungroup() %>%
      dplyr::distinct(direction) %>%
      dplyr::pull(direction),
    "greater"
  )

  #>reference_dose == [first dose], reference_type == "ratio"----

  samps1b <- yodel::posterior(
    x = nb_indep_model_samples_updatedattr,
    reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
    return_stats = FALSE,
    return_samples = TRUE,
    new_data = nb_monotone_incr_new,
    reference_type = "ratio"
  )$samples

  pr_eoi_impl1b <- purrr::map_dfr(eoia, pr_eoi_impl, x = samps1b)

  expect_failure(expect_s3_class(pr_eoi_impl1b, NA))
  expect_s3_class(pr_eoi_impl1b, "data.frame")

  expect_no_error(
    apply_direction(
      x = pr_eoi_impl1b,
      direction = "greater"
    )
  )

  direction1b <- apply_direction(
    x = pr_eoi_impl1b,
    direction = "greater"
  )

  expect_identical(
    direction1b %>%
      dplyr::select(- c(direction)),
    pr_eoi_impl1b,
  )
  expect_identical(
    direction1b %>%
      dplyr::ungroup() %>%
      dplyr::distinct(direction) %>%
      dplyr::pull(direction),
    "greater"
  )

  #contrast----

  contrast2 <- get_contrast(
    x = nb_indep_model_samples_updatedattr,
    new_data = NULL,
    contrast = matrix(1, 1, 1)
  )

  #>reference_dose == NULL----

  samps2 <- yodel::posterior(
    x = nb_indep_model_samples_updatedattr,
    return_stats = FALSE,
    return_samples = TRUE,
    contrast = matrix(1, 1, 1)
  )$samples

  pr_eoi_impl2 <- purrr::map_dfr(eoi, pr_eoi_impl, x = samps2)

  expect_failure(expect_s3_class(pr_eoi_impl2, NA))
  expect_s3_class(pr_eoi_impl2, "data.frame")

  expect_no_error(
    apply_direction(
      x = pr_eoi_impl2,
      direction = "greater"
    )
  )

  direction2 <- apply_direction(
    x = pr_eoi_impl2,
    direction = "greater"
  )

  expect_identical(
    direction2 %>%
      dplyr::select(- c(direction)),
    pr_eoi_impl2,
  )
  expect_identical(
    direction2 %>%
      dplyr::ungroup() %>%
      dplyr::distinct(direction) %>%
      dplyr::pull(direction),
    "greater"
  )

  #>>direction == "less"----

  expect_no_error(
    apply_direction(
      x = pr_eoi_impl2,
      direction = "less"
    )
  )

  direction2_less <- apply_direction(
    x = pr_eoi_impl2,
    direction = "less"
  )

  expect_identical(
    direction2_less %>%
      dplyr::select(- c(prob, direction)),
    direction2 %>%
      dplyr::select(- c(prob, direction)),
  )
  expect_equal(
    direction2_less %>%
      dplyr::mutate(prob1 = 1 - prob) %>%
      dplyr::pull(prob1),
    direction2 %>%
      dplyr::pull(prob)
  )
  expect_identical(
    direction2_less %>%
      dplyr::ungroup() %>%
      dplyr::distinct(direction) %>%
      dplyr::pull(direction),
    "less"
  )

  #>reference_dose == [first dose], reference_type == "difference"----

  samps2a <- yodel::posterior(
    x = nb_indep_model_samples_updatedattr,
    reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
    return_stats = FALSE,
    return_samples = TRUE,
    contrast = matrix(1, 1, 1),
    reference_type = "difference"
  )$samples

  pr_eoi_impl2a <- purrr::map_dfr(eoia, pr_eoi_impl, x = samps2a)

  expect_failure(expect_s3_class(pr_eoi_impl2a, NA))
  expect_s3_class(pr_eoi_impl2a, "data.frame")

  expect_no_error(
    apply_direction(
      x = pr_eoi_impl2a,
      direction = "greater"
    )
  )

  direction2a <- apply_direction(
    x = pr_eoi_impl2a,
    direction = "greater"
  )

  expect_identical(
    direction2a %>%
      dplyr::select(- c(direction)),
    pr_eoi_impl2a,
  )
  expect_identical(
    direction2a %>%
      dplyr::ungroup() %>%
      dplyr::distinct(direction) %>%
      dplyr::pull(direction),
    "greater"
  )

  #>reference_dose == [first dose], reference_type == "ratio"----

  samps2b <- yodel::posterior(
    x = nb_indep_model_samples_updatedattr,
    reference_dose = attr(nb_indep_model_samples_updatedattr, "doses")[1],
    return_stats = FALSE,
    return_samples = TRUE,
    contrast = matrix(1, 1, 1),
    reference_type = "ratio"
  )$samples

  pr_eoi_impl2b <- purrr::map_dfr(eoia, pr_eoi_impl, x = samps2b)

  expect_failure(expect_s3_class(pr_eoi_impl2b, NA))
  expect_s3_class(pr_eoi_impl2b, "data.frame")

  expect_no_error(
    apply_direction(
      x = pr_eoi_impl2b,
      direction = "greater"
    )
  )

  direction2b <- apply_direction(
    x = pr_eoi_impl2b,
    direction = "greater"
  )

  expect_identical(
    direction2b %>%
      dplyr::select(- c(direction)),
    pr_eoi_impl2b,
  )
  expect_identical(
    direction2b %>%
      dplyr::ungroup() %>%
      dplyr::distinct(direction) %>%
      dplyr::pull(direction),
    "greater"
  )
})

test_that("pr_eoi_g_comp works against an S3 object of class beaver_mcmc, produces an object with correct properties", {  # nolint

  nb_monotone_incr_cov <- readRDS(test_path("fixtures", "nb_monotone_incr_cov.rds"))  # nolint

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

  eoi <- c(9, 35, 60, 110)
  eoia <- c(25, 50, 100)
  eoib <- c(4, 6, 12)

  #new_data----

  #>reference_dose == NULL----

  samps1 <- posterior_g_comp(
    x = nb_emax_model_cov_samples_updatedattr,
    return_stats = FALSE,
    return_samples = TRUE,
    new_data = nb_monotone_incr_cov
  )$samples

  pr_eoi_g_comp1 <- pr_eoi_g_comp(
    x = nb_emax_model_cov_samples_updatedattr,
    eoi = eoi,
    new_data = nb_monotone_incr_cov
  )

  pr_eoi_g_comp_checks(
    pr_eoi = pr_eoi_g_comp1,
    samples = nb_emax_model_cov_samples_updatedattr,
    posteriors = samps1,
    eoi = eoi
  )

  #>>direction == "less"----

  pr_eoi_g_comp1_less <- pr_eoi_g_comp(
    x = nb_emax_model_cov_samples_updatedattr,
    eoi = eoi,
    new_data = nb_monotone_incr_cov,
    direction = "less"
  )

  expect_identical(
    pr_eoi_g_comp1_less %>%
      dplyr::select(- c(prob, direction)),
    pr_eoi_g_comp1 %>%
      dplyr::select(- c(prob, direction)),
  )
  expect_equal(
    pr_eoi_g_comp1_less %>%
      dplyr::mutate(prob1 = 1 - prob) %>%
      dplyr::pull(prob1),
    pr_eoi_g_comp1 %>%
      dplyr::pull(prob)
  )
  expect_identical(
    pr_eoi_g_comp1_less %>%
      dplyr::ungroup() %>%
      dplyr::distinct(direction) %>%
      dplyr::pull(direction),
    "less"
  )

  #>reference_dose == [first dose], reference_type == "difference"----

  samps1a <- posterior_g_comp(
    x = nb_emax_model_cov_samples_updatedattr,
    reference_dose = attr(nb_emax_model_cov_samples_updatedattr, "doses")[1],
    return_stats = FALSE,
    return_samples = TRUE,
    new_data = nb_monotone_incr_cov,
    reference_type = "difference"
  )$samples

  expect_no_error(
    pr_eoi_g_comp(
      x = nb_emax_model_cov_samples_updatedattr,
      reference_dose = attr(nb_emax_model_cov_samples_updatedattr, "doses")[1],
      eoi = eoia,
      new_data = nb_monotone_incr_cov,
      reference_type = "difference"
    )
  )

  pr_eoi_g_comp1a <- pr_eoi_g_comp(
    x = nb_emax_model_cov_samples_updatedattr,
    reference_dose = attr(nb_emax_model_cov_samples_updatedattr, "doses")[1],
    eoi = eoia,
    new_data = nb_monotone_incr_cov,
    reference_type = "difference"
  )

  pr_eoi_g_comp_checks(
    pr_eoi = pr_eoi_g_comp1a,
    samples = nb_emax_model_cov_samples_updatedattr,
    posteriors = samps1a,
    eoi = eoia
  )

  #>reference_dose == [first dose], reference_type == "ratio"----

  samps1b <- posterior_g_comp(
    x = nb_emax_model_cov_samples_updatedattr,
    reference_dose = attr(nb_emax_model_cov_samples_updatedattr, "doses")[1],
    return_stats = FALSE,
    return_samples = TRUE,
    new_data = nb_monotone_incr_cov,
    reference_type = "ratio"
  )$samples

  pr_eoi_g_comp1b <- pr_eoi_g_comp(
    x = nb_emax_model_cov_samples_updatedattr,
    reference_dose = attr(nb_emax_model_cov_samples_updatedattr, "doses")[1],
    eoi = eoib,
    new_data = nb_monotone_incr_cov,
    reference_type = "ratio"
  )

  pr_eoi_g_comp_checks(
    pr_eoi = pr_eoi_g_comp1b,
    samples = nb_emax_model_cov_samples_updatedattr,
    posteriors = samps1b,
    eoi = eoib
  )

  # nolint start
  # #contrast----
  #
  # #>reference_dose == NULL----
  #
  # samps2 <- posterior_g_comp(
  #   x = nb_emax_model_cov_samples_updatedattr,
  #   return_stats = FALSE,
  #   return_samples = TRUE,
  #   contrast = matrix(1, 1, 1)
  # )$samples
  #
  # pr_eoi_g_comp2 <- pr_eoi_g_comp(
  #   x = nb_emax_model_cov_samples_updatedattr,
  #   eoi = eoi,
  #   contrast = matrix(1, 1, 1)
  # )
  #
  # pr_eoi_g_comp_checks(
  #   pr_eoi = pr_eoi_g_comp2,
  #   samples = nb_emax_model_cov_samples_updatedattr,
  #   posteriors = samps2,
  #   eoi = eoi
  # )
  #
  # #>>direction == "less"----
  #
  # pr_eoi_g_comp2_less <- pr_eoi_g_comp(
  #   x = nb_emax_model_cov_samples_updatedattr,
  #   eoi = eoi,
  #   contrast = matrix(1, 1, 1),
  #   direction = "less"
  # )
  #
  # expect_identical(
  #   pr_eoi_g_comp2_less %>%
  #     dplyr::select(-c(prob, direction)),
  #   pr_eoi_g_comp2 %>%
  #     dplyr::select(-c(prob, direction)),
  # )
  # expect_equal(
  #   pr_eoi_g_comp2_less %>%
  #     dplyr::mutate(prob1 = 1 - prob) %>%
  #     dplyr::pull(prob1),
  #   pr_eoi_g_comp2 %>%
  #     dplyr::pull(prob)
  # )
  # expect_identical(
  #   pr_eoi_g_comp2_less %>%
  #     dplyr::ungroup() %>%
  #     dplyr::distinct(direction) %>%
  #     dplyr::pull(direction),
  #   "less"
  # )
  #
  # #>reference_dose == [first dose], reference_type == "difference"----
  #
  # samps2a <- posterior_g_comp(
  #   x = nb_emax_model_cov_samples_updatedattr,
  #   reference_dose = attr(nb_emax_model_cov_samples_updatedattr, "doses")[1],
  #   return_stats = FALSE,
  #   return_samples = TRUE,
  #   contrast = matrix(1, 1, 1),
  #   reference_type = "difference"
  # )$samples
  #
  # pr_eoi_g_comp2a <- pr_eoi_g_comp(
  #   x = nb_emax_model_cov_samples_updatedattr,
  #   reference_dose = attr(nb_emax_model_cov_samples_updatedattr, "doses")[1],
  #   eoi = eoia,
  #   contrast = matrix(1, 1, 1),
  #   reference_type = "difference"
  # )
  #
  # pr_eoi_g_comp_checks(
  #   pr_eoi = pr_eoi_g_comp2a,
  #   samples = nb_emax_model_cov_samples_updatedattr,
  #   posteriors = samps2a,
  #   eoi = eoia
  # )
  #
  # #>reference_dose == [first dose], reference_type == "ratio"----
  #
  # samps2b <- posterior_g_comp(
  #   x = nb_emax_model_cov_samples_updatedattr,
  #   reference_dose = attr(nb_emax_model_cov_samples_updatedattr, "doses")[1],
  #   return_stats = FALSE,
  #   return_samples = TRUE,
  #   contrast = matrix(1, 1, 1),
  #   reference_type = "ratio"
  # )$samples
  #
  # pr_eoi_g_comp2b <- pr_eoi_g_comp(
  #   x = nb_emax_model_cov_samples_updatedattr,
  #   reference_dose = attr(nb_emax_model_cov_samples_updatedattr, "doses")[1],
  #   eoi = eoib,
  #   contrast = matrix(1, 1, 1),
  #   reference_type = "ratio"
  # )
  #
  # pr_eoi_g_comp_checks(
  #   pr_eoi = pr_eoi_g_comp2b,
  #   samples = nb_emax_model_cov_samples_updatedattr,
  #   posteriors = samps2b,
  #   eoi = eoib
  # )
  # nolint end
})

test_that("pr_eoi_g_comp works against an S3 object of class beaver_mcmc_bma, produces an object with correct properties", {  # nolint

  nb_monotone_incr_cov <- readRDS(test_path("fixtures", "nb_monotone_incr_cov.rds"))  # nolint

  expect_failure(expect_s3_class(nb_monotone_incr_cov, NA))
  expect_s3_class(nb_monotone_incr_cov, "data.frame")

  load(test_path("fixtures", "nb_bma_cov_objects.Rdata"))

  expect_failure(expect_s3_class(nb_bma_cov, NA))
  expect_s3_class(
    nb_bma_cov,
    c("beaver_mcmc_bma", "yodel_bma", "beaver_mcmc"),
    exact = TRUE
  )
  expect_no_error(
    checkmate::assertSubset(
      c("formula", "doses"),
      names(attributes(nb_bma_cov))
    )
  )

  eoi <- c(9, 35, 60, 110)
  eoia <- c(25, 50, 100)
  eoib <- c(4, 6, 12)

  #new_data----

  #>reference_dose == NULL----

  samps1 <- posterior_g_comp(
    x = nb_bma_cov,
    return_stats = FALSE,
    return_samples = TRUE,
    new_data = nb_monotone_incr_cov
  )$samples

  pr_eoi1 <- pr_eoi_g_comp(
    x = nb_bma_cov,
    eoi = eoi,
    new_data = nb_monotone_incr_cov
  )

  pr_eoi_g_comp_checks(
    pr_eoi = pr_eoi1,
    samples = nb_bma_cov,
    posteriors = samps1,
    eoi = eoi
  )

  #>>direction == "less"----

  pr_eoi1_less <- pr_eoi_g_comp(
    x = nb_bma_cov,
    eoi = eoi,
    new_data = nb_monotone_incr_cov,
    direction = "less"
  )

  expect_identical(
    pr_eoi1_less %>%
      dplyr::select(- c(prob, direction)),
    pr_eoi1 %>%
      dplyr::select(- c(prob, direction)),
  )
  expect_equal(
    pr_eoi1_less %>%
      dplyr::mutate(prob1 = 1 - prob) %>%
      dplyr::pull(prob1),
    pr_eoi1 %>%
      dplyr::pull(prob)
  )
  expect_identical(
    pr_eoi1_less %>%
      dplyr::ungroup() %>%
      dplyr::distinct(direction) %>%
      dplyr::pull(direction),
    "less"
  )

  #>reference_dose == [first dose], reference_type == "difference"----

  samps1a <- posterior_g_comp(
    x = nb_bma_cov,
    reference_dose = attr(nb_bma_cov, "doses")[1],
    return_stats = FALSE,
    return_samples = TRUE,
    new_data = nb_monotone_incr_cov,
    reference_type = "difference"
  )$samples

  pr_eoi1a <- pr_eoi_g_comp(
    x = nb_bma_cov,
    reference_dose = attr(nb_bma_cov, "doses")[1],
    eoi = eoia,
    new_data = nb_monotone_incr_cov,
    reference_type = "difference"
  )

  pr_eoi_g_comp_checks(
    pr_eoi = pr_eoi1a,
    samples = nb_bma_cov,
    posteriors = samps1a,
    eoi = eoia
  )

  #>reference_dose == [first dose], reference_type == "ratio"----

  samps1b <- posterior_g_comp(
    x = nb_bma_cov,
    reference_dose = attr(nb_bma_cov, "doses")[1],
    return_stats = FALSE,
    return_samples = TRUE,
    new_data = nb_monotone_incr_cov,
    reference_type = "ratio"
  )$samples

  pr_eoi1b <- pr_eoi_g_comp(
    x = nb_bma_cov,
    reference_dose = attr(nb_bma_cov, "doses")[1],
    eoi = eoib,
    new_data = nb_monotone_incr_cov,
    reference_type = "ratio"
  )

  pr_eoi_g_comp_checks(
    pr_eoi = pr_eoi1b,
    samples = nb_bma_cov,
    posteriors = samps1b,
    eoi = eoib
  )

  # nolint start
  # #contrast----
  #
  # #>reference_dose == NULL----
  #
  # samps2 <- posterior_g_comp(
  #   x = nb_bma_cov,
  #   return_stats = FALSE,
  #   return_samples = TRUE,
  #   contrast = matrix(1, 1, 1)
  # )$samples
  #
  # pr_eoi2 <- pr_eoi_g_comp(
  #   x = nb_bma_cov,
  #   eoi = eoi,
  #   contrast = matrix(1, 1, 1)
  # )
  #
  # pr_eoi_g_comp_checks(
  #   pr_eoi = pr_eoi2,
  #   samples = nb_bma_cov,
  #   posteriors = samps2,
  #   eoi = eoi
  # )
  #
  # #>>direction == "less"----
  #
  # pr_eoi2_less <- pr_eoi_g_comp(
  #   x = nb_bma_cov,
  #   eoi = eoi,
  #   contrast = matrix(1, 1, 1),
  #   direction = "less"
  # )
  #
  # expect_identical(
  #   pr_eoi2_less %>%
  #     dplyr::select(-c(prob, direction)),
  #   pr_eoi2 %>%
  #     dplyr::select(-c(prob, direction)),
  # )
  # expect_equal(
  #   pr_eoi2_less %>%
  #     dplyr::mutate(prob1 = 1 - prob) %>%
  #     dplyr::pull(prob1),
  #   pr_eoi2 %>%
  #     dplyr::pull(prob)
  # )
  # expect_identical(
  #   pr_eoi2_less %>%
  #     dplyr::ungroup() %>%
  #     dplyr::distinct(direction) %>%
  #     dplyr::pull(direction),
  #   "less"
  # )
  #
  # #>reference_dose == [first dose], reference_type == "difference"----
  #
  # samps2a <- posterior_g_comp(
  #   x = nb_bma_cov,
  #   reference_dose = attr(nb_bma_cov, "doses")[1],
  #   return_stats = FALSE,
  #   return_samples = TRUE,
  #   contrast = matrix(1, 1, 1),
  #   reference_type = "difference"
  # )$samples
  #
  # pr_eoi2a <- pr_eoi_g_comp(
  #   x = nb_bma_cov,
  #   reference_dose = attr(nb_bma_cov, "doses")[1],
  #   eoi = eoia,
  #   contrast = matrix(1, 1, 1),
  #   reference_type = "difference"
  # )
  #
  # pr_eoi_g_comp_checks(
  #   pr_eoi = pr_eoi2a,
  #   samples = nb_bma_cov,
  #   posteriors = samps2a,
  #   eoi = eoia
  # )
  #
  # #>reference_dose == [first dose], reference_type == "ratio"----
  #
  # samps2b <- posterior_g_comp(
  #   x = nb_bma_cov,
  #   reference_dose = attr(nb_bma_cov, "doses")[1],
  #   return_stats = FALSE,
  #   return_samples = TRUE,
  #   contrast = matrix(1, 1, 1),
  #   reference_type = "ratio"
  # )$samples
  #
  # pr_eoi2b <- pr_eoi_g_comp(
  #   x = nb_bma_cov,
  #   reference_dose = attr(nb_bma_cov, "doses")[1],
  #   eoi = eoib,
  #   contrast = matrix(1, 1, 1),
  #   reference_type = "ratio"
  # )
  #
  # pr_eoi_g_comp_checks(
  #   pr_eoi = pr_eoi2b,
  #   samples = nb_bma_cov,
  #   posteriors = samps2b,
  #   eoi = eoib
  # )
  # nolint end
})
