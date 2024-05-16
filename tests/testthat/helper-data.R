
data_negbin_emax_checks <- function(data,
                                    n_per_arm,
                                    doses,
                                    p,
                                    b1,
                                    x = NULL) {
  expect_failure(expect_s3_class(data, NA))
  expect_s3_class(data, "data.frame")
  expect_identical(nrow(data), as.integer(length(doses) * n_per_arm))
  expect_failure(expect_named(data, NULL))
  if (is.null(x)) {
    expect_identical(
      names(data),
      c("subject", "dose", "p", "mu", "r", "response")
    )
  } else {
    expect_identical(
      names(data),
      c("subject", "dose", "p", colnames(x), "b1", "mu", "r", "response")
    )
  }
  for (i in names(data)) expect_true(is.numeric(data[[i]]))
  expect_identical(
    data %>%
      dplyr::pull(subject),
    seq_len(as.integer(length(doses) * n_per_arm))
  )
  expect_identical(
    data %>%
      dplyr::distinct(.data$dose) %>%
      dplyr::arrange(.data$dose) %>%
      dplyr::pull(dose),
    doses
  )
  expect_identical(
    data %>%
      dplyr::distinct(.data$p) %>%
      dplyr::pull(p),
    p
  )
  expect_no_error(
    checkmate::assertNumeric(
      x = data %>% dplyr::pull(mu),
      lower = 0,
      finite = TRUE,
      any.missing = FALSE,
      len = length(doses) * n_per_arm
    )
  )
  expect_no_error(
    checkmate::assertNumeric(
      x = data %>% dplyr::pull(r),
      lower = 0,
      finite = TRUE,
      any.missing = FALSE,
      len = length(doses) * n_per_arm
    )
  )
  expect_no_error(
    checkmate::assertInteger(
      x = data %>% dplyr::pull(response),
      lower = 0,
      any.missing = FALSE,
      len = length(doses) * n_per_arm
    )
  )

  if (!is.null(x)) {

    expect_identical(
      data %>%
        dplyr::select(colnames(x)),
      x %>%
        tibble::as_tibble()
    )
    expect_no_error(
      checkmate::assertNumeric(
        x = data %>% dplyr::pull(b1),
        lower = 0,
        finite = TRUE,
        any.missing = FALSE,
        len = length(doses) * n_per_arm
      )
    )

  }
}
