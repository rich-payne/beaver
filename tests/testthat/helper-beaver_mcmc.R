
beaver_mcmc_checks <- function(beaver_mcmc,
                               model_class) {
  expect_failure(expect_s3_class(beaver_mcmc, NA))
  expect_s3_class(
    beaver_mcmc,
    c("beaver_mcmc_bma", "yodel_bma", "beaver_mcmc"),
    exact = TRUE
  )
  expect_named(beaver_mcmc, c("w_prior", "w_post", "models", "seed"))
  expect_true(is.numeric(beaver_mcmc$w_prior))
  expect_named(
    beaver_mcmc$w_prior,
    stringr::word(
      model_class,
      3,
      stringr::str_count(model_class, pattern = "_") + 1,
      sep = "_"
    )
  )
  expect_true(is.numeric(beaver_mcmc$w_post))
  expect_named(
    beaver_mcmc$w_post,
    stringr::word(
      model_class,
      3,
      stringr::str_count(model_class, pattern = "_") + 1,
      sep = "_"
    )
  )
  expect_true(is.list(beaver_mcmc$models))
  expect_named(
    beaver_mcmc$models,
    stringr::word(
      model_class,
      3,
      stringr::str_count(model_class, pattern = "_") + 1,
      sep = "_"
    )
  )
  expect_true(is.numeric(beaver_mcmc$seed))
}

run_mcmc_checks <- function(mcmc,
                            model_class,
                            n_iter,
                            n_chains) {
  expect_true(is.list(mcmc))
  expect_named(mcmc, c("samples", "log_post_pred"))
  expect_failure(expect_s3_class(mcmc$samples, NA))
  expect_s3_class(
    mcmc$samples,
    c(
      paste0(
        stringr::word(model_class, 1, sep = "_"),
        "_mcmc_",
        stringr::word(
          model_class,
          2,
          stringr::str_count(model_class, pattern = "_") + 1,
          sep = "_"
        )
      ),
      "beaver_mcmc",
      "mcmc.list"
    ),
    exact = TRUE
  )
  expect_identical(length(mcmc$samples), n_chains)
  expect_identical(unique(sapply(mcmc$samples, nrow)), n_iter)
  expect_true(is.matrix(mcmc$log_post_pred))
}

set_jags_seed_checks <- function(seed,
                                 n_chains) {
  expect_true(is.list(seed))
  expect_named(seed, NULL)
  expect_identical(length(seed), n_chains)
  expect_identical(unique(sapply(seed, length)), 2L)
  expect_identical(
    tibble::as_tibble(as.data.frame(sapply(seed, names))) %>%
      tidyr::pivot_longer(cols = dplyr::everything()) %>%
      dplyr::distinct(.data$value) %>%
      dplyr::pull(value),
    c(".RNG.seed", ".RNG.name")
  )
  expect_true(is.numeric(sapply(seed, function(x) x[[1]])))
  expect_identical(
    unique(sapply(seed, function(x) x[[2]])),
    "base::Mersenne-Twister"
  )
}

rename_b1_checks <- function(samples,
                             samples_renamed,
                             b1_names) {
  expect_identical(
    tibble::as_tibble(as.data.frame(sapply(samples_renamed, colnames))) %>%
      tidyr::pivot_longer(cols = dplyr::everything()) %>%
      dplyr::distinct(.data$value) %>%
      dplyr::pull(value),
    c(
      b1_names,
      grep(
        "^(?!b1)",
        tibble::as_tibble(as.data.frame(sapply(samples, colnames))) %>%
          tidyr::pivot_longer(cols = dplyr::everything()) %>%
          dplyr::distinct(.data$value) %>%
          dplyr::pull(value),
        perl = TRUE,
        value = TRUE
      )
    )
  )
}

bma_fit_checks <- function(bma_fit,
                           models) {
  expect_failure(expect_s3_class(bma_fit, NA))
  expect_s3_class(bma_fit, "yodel_bma")
  expect_named(bma_fit, c("w_prior", "w_post", "models", "seed"))
  expect_true(is.numeric(bma_fit$w_prior))
  expect_named(bma_fit$w_prior, names(models))
  expect_true(is.numeric(bma_fit$w_post))
  expect_named(bma_fit$w_post, names(models))
  expect_true(is.list(bma_fit$models))
  expect_named(bma_fit$models, names(models))
  expect_true(is.numeric(bma_fit$seed))
}

bma_arg_checks <- function(bma_arg,
                           mcmc) {
  expect_failure(expect_s3_class(bma_arg, NA))
  expect_s3_class(
    bma_arg,
    c("yodel_model_predictive", "yodel_bma_candidate"),
    exact = TRUE
  )
  expect_named(
    bma_arg,
    c("mcmc", "log_post_pred", "adjustment", "fun", "w_prior")
  )
  expect_identical(bma_arg$mcmc, mcmc$samples)
  expect_identical(bma_arg$log_post_pred, mcmc$log_post_pred)
  expect_true(is.numeric(bma_arg$adjustment))
  expect_true(is.function(bma_arg$fun))
  expect_true(is.numeric(bma_arg$w_prior))
}
