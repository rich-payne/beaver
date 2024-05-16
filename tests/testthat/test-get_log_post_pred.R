
test_that("get_log_post_pred works against an S3 object of class beaver_mcmc, produces an object with correct properties", { # nolint

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

  formula <- ~ 1
  nb_monotone_incr_x <- model.matrix(formula, data = nb_monotone_incr)

  load(test_path("fixtures", "nb_indep_mcmc+_objects.Rdata"))

  expect_failure(expect_s3_class(nb_indep_model_samples_updatedattr, NA))
  expect_s3_class(nb_indep_model_samples_updatedattr, "beaver_mcmc")
  expect_no_error(
    checkmate::assertSubset(
      c("covariate_names", "formula"),
      names(attributes(nb_indep_model_samples_updatedattr))
    )
  )

  #n_models == 1----

  expect_no_error(
    get_log_post_pred(
      samples = nb_indep_model_samples_updatedattr,
      data = nb_monotone_incr,
      x = nb_monotone_incr_x,
      n_models = 1
    )
  )

  log_post_pred1 <- get_log_post_pred(
    samples = nb_indep_model_samples_updatedattr,
    data = nb_monotone_incr,
    x = nb_monotone_incr_x,
    n_models = 1
  )

  expect_true(is.matrix(log_post_pred1))
  expect_identical(log_post_pred1, matrix(1, 1, 1))

  #n_models == 2----

  expect_no_error(
    get_log_post_pred(
      samples = nb_indep_model_samples_updatedattr,
      data = nb_monotone_incr,
      x = nb_monotone_incr_x,
      n_models = 2
    )
  )

  log_post_pred2 <- get_log_post_pred(
    samples = nb_indep_model_samples_updatedattr,
    data = nb_monotone_incr,
    x = nb_monotone_incr_x,
    n_models = 2
  )

  expect_true(is.matrix(log_post_pred2))
  expect_identical(
    nrow(log_post_pred2),
    sum(sapply(nb_indep_model_samples_updatedattr, nrow))
  )
  expect_identical(ncol(log_post_pred2), nrow(nb_monotone_incr))
  expect_identical(
    tibble::as_tibble(log_post_pred2) %>%
      dplyr::rowwise() %>%
      dplyr::summarize(
        a = length(unique(dplyr::c_across(cols = dplyr::everything())))
      ) %>%
      dplyr::ungroup() %>%
      dplyr::distinct(a) %>%
      dplyr::pull(a),
    nb_monotone_incr %>%
      dplyr::group_by(dose) %>%
      dplyr::distinct(response) %>%
      dplyr::ungroup() %>%
      dplyr::tally() %>%
      dplyr::pull(n)
  )

  #n_models == 3----

  expect_identical(
    get_log_post_pred(
      samples = nb_indep_model_samples_updatedattr,
      data = nb_monotone_incr,
      x = nb_monotone_incr_x,
      n_models = 3
    ),
    log_post_pred2
  )
})

test_that("get_p works against an S3 object of class beaver_mcmc, produces an object with correct properties", { # nolint

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

  nb_monotone_incr_doses <- nb_monotone_incr %>%
    dplyr::distinct(dose) %>%
    dplyr::arrange(dose) %>%
    dplyr::pull(dose)

  nb_monotone_incr <- nb_monotone_incr %>%
    dplyr::rowwise() %>%
    dplyr::mutate(dose_index =  which(dose == nb_monotone_incr_doses)) %>%
    dplyr::ungroup()

  load(test_path("fixtures", "nb_indep_mcmc+_objects.Rdata"))

  expect_failure(expect_s3_class(nb_indep_model_samples_updatedattr, NA))
  expect_s3_class(nb_indep_model_samples_updatedattr, "beaver_mcmc")
  expect_no_error(
    checkmate::assertSubset(
      c("covariate_names", "formula"),
      names(attributes(nb_indep_model_samples_updatedattr))
    )
  )

  samps <- dplyr::as_tibble(as.matrix(nb_indep_model_samples_updatedattr)) %>%
    dplyr::mutate(iter = seq_len(n()))

  expect_no_error(get_p(nb_monotone_incr$dose_index, samps))

  p <- get_p(nb_monotone_incr$dose_index, samps)

  expect_true(is.matrix(p))
  expect_identical(
    nrow(p),
    sum(sapply(nb_indep_model_samples_updatedattr, nrow))
  )
  expect_identical(ncol(p), nrow(nb_monotone_incr))
  expect_identical(
    suppressMessages(tibble::as_tibble(p, .name_repair = "unique")) %>%
      dplyr::rowwise() %>%
      dplyr::summarize(
        a = length(unique(dplyr::c_across(cols = dplyr::everything())))
      ) %>%
      dplyr::ungroup() %>%
      dplyr::distinct(a) %>%
      dplyr::pull(a),
    length(attr(nb_indep_model_samples_updatedattr, "doses"))
  )
})
