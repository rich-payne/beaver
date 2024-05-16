
samples_process <- function(samples,
                            data) {
  samps <- tibble::as_tibble(as.matrix(samples)) %>%
    dplyr::mutate(iter = seq_len(n()))
  n_mcmc <- nrow(samps)
  b1 <- samps %>%
    dplyr::select(attr(samples, "covariate_names")) %>%
    as.matrix()
  intercept <- b1 %*% t(model.matrix(attr(samples, "formula"), data = data))

  objs <- ls()[!(ls() %in% formalArgs(samples_process))]

  for (i in seq_along(objs)) assign(objs[i], get(objs[i]), envir = parent.frame()) # nolint
}

mean_checks <- function(means,
                        samples,
                        data) {
  expect_true(is.matrix(means))
  expect_identical(nrow(means), sum(sapply(samples, nrow)))
  expect_identical(ncol(means), nrow(data))
  expect_identical(
    tibble::as_tibble(means) %>%
      dplyr::rowwise() %>%
      dplyr::summarize(
        a = length(unique(dplyr::c_across(cols = dplyr::everything())))
      ) %>%
      dplyr::ungroup() %>%
      dplyr::distinct(.data$a) %>%
      dplyr::pull(a),
    length(attr(samples, "doses"))
  )
}
