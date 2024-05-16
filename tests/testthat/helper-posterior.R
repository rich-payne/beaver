
get_samps_checks <- function(samps,
                             samples,
                             draws,
                             contrast) {
  expect_failure(expect_s3_class(samps, NA))
  expect_s3_class(samps, "data.frame")
  expect_identical(
    nrow(samps),
    sum(sapply(samples, nrow)) * length(attr(samples, "doses")) *
      nrow(contrast)
  )
  expect_failure(expect_named(samps, NULL))
  expect_identical(
    names(samps),
    c("dose", "value", "iter", ".contrast_index", colnames(contrast))
  )
  for (i in names(samps)) expect_true(is.numeric(samps[[i]]))
  expect_identical(
    samps %>%
      dplyr::distinct(.data$dose) %>%
      dplyr::arrange(.data$dose) %>%
      dplyr::pull(dose),
    attr(samples, "doses")
  )
  expect_identical(
    samps %>%
      dplyr::distinct(.data$iter) %>%
      dplyr::arrange(.data$iter),
    draws %>%
      dplyr::distinct(.data$iter) %>%
      dplyr::arrange(.data$iter)
  )
  expect_identical(
    samps %>%
      dplyr::distinct(.data$.contrast_index) %>%
      dplyr::pull(.contrast_index),
    seq_len(nrow(contrast))
  )
  expect_identical(
    samps %>%
      dplyr::distinct(.data$.contrast_index, !!!syms(colnames(contrast))) %>%
      dplyr::select(colnames(contrast)),
    contrast %>%
      as.data.frame() %>%
      tibble::as_tibble()
  )
}

adjust_reference_checks <- function(adjusted,
                                    samples,
                                    draws,
                                    contrast) {
  expect_failure(expect_s3_class(adjusted, NA))
  expect_s3_class(adjusted, "data.frame")
  expect_identical(
    nrow(adjusted),
    sum(sapply(samples, nrow)) * length(attr(samples, "doses")) *
      nrow(contrast)
  )
  expect_failure(expect_named(adjusted, NULL))
  expect_no_error(
    checkmate::assertSubset(
      names(adjusted),
      c(
        "dose",
        "value",
        "iter",
        ".contrast_index",
        colnames(contrast),
        "reference_dose",
        "reference_type"
      )
    )
  )
  for (i in names(adjusted)[!names(adjusted) %in% c("reference_type")]) expect_true(is.numeric(adjusted[[i]])) # nolint
  for (j in names(adjusted)[names(adjusted) %in% c("reference_type")]) expect_true(is.character(adjusted[[j]])) # nolint
  expect_identical(
    adjusted %>%
      dplyr::distinct(.data$dose) %>%
      dplyr::arrange(.data$dose) %>%
      dplyr::pull(dose),
    attr(samples, "doses")
  )
  expect_identical(
    adjusted %>%
      dplyr::distinct(.data$iter) %>%
      dplyr::arrange(.data$iter),
    draws %>%
      dplyr::distinct(.data$iter) %>%
      dplyr::arrange(.data$iter)
  )
  expect_identical(
    adjusted %>%
      dplyr::distinct(.data$.contrast_index) %>%
      dplyr::pull(.contrast_index),
    seq_len(nrow(contrast))
  )
  expect_identical(
    adjusted %>%
      dplyr::distinct(.data$.contrast_index, !!sym(colnames(contrast))) %>%
      dplyr::pull(colnames(contrast)),
    contrast %>%
      tibble::as_tibble() %>%
      dplyr::pull(colnames(contrast)) %>%
      as.numeric()
  )

  if ("reference_dose" %in% names(adjusted)) {

    expect_identical(
      adjusted %>%
        dplyr::distinct(.data$reference_dose) %>%
        dplyr::pull(reference_dose),
      attr(samples, "doses")[1]
    )

  }

  if ("reference_type" %in% names(adjusted)) {

    expect_no_error(checkmate::assertSubset(
      adjusted %>%
        dplyr::distinct(.data$reference_type) %>%
        dplyr::pull(reference_type),
      c("difference", "ratio")
    ))

  }
}

get_stats_checks <- function(stats,
                             samples,
                             contrast) {
  expect_failure(expect_s3_class(stats, NA))
  expect_s3_class(stats, "data.frame")
  expect_identical(
    nrow(stats),
    length(attr(samples, "doses")) * nrow(contrast)
  )
  expect_failure(expect_named(stats, NULL))
  expect_no_error(
    checkmate::assertSubset(
      names(stats),
      c(
        "dose",
        ".contrast_index",
        colnames(contrast),
        "value",
        "2.50%",
        "97.50%",
        "reference_dose",
        "reference_type"
      )
    )
  )
  for (i in names(stats)[!names(stats) %in% c("reference_type")]) expect_true(is.numeric(stats[[i]])) # nolint
  for (j in names(stats)[names(stats) %in% c("reference_type")]) expect_true(is.character(stats[[j]])) # nolint
  expect_identical(
    stats %>%
      dplyr::distinct(.data$dose) %>%
      dplyr::arrange(.data$dose) %>%
      dplyr::pull(dose),
    attr(samples, "doses")
  )
  expect_identical(
    stats %>%
      dplyr::distinct(.data$.contrast_index) %>%
      dplyr::pull(.contrast_index),
    seq_len(nrow(contrast))
  )
  expect_identical(
    stats %>%
      dplyr::distinct(.data$.contrast_index, !!sym(colnames(contrast))) %>%
      dplyr::pull(colnames(contrast)),
    contrast %>%
      tibble::as_tibble() %>%
      dplyr::pull(colnames(contrast)) %>%
      as.numeric()
  )

  if ("reference_dose" %in% names(stats)) {

    expect_identical(
      stats %>%
        dplyr::distinct(.data$reference_dose) %>%
        dplyr::pull(reference_dose),
      attr(samples, "doses")[1]
    )

  }

  if ("reference_type" %in% names(stats)) {

    expect_no_error(checkmate::assertSubset(
      stats %>%
        dplyr::distinct(.data$reference_type) %>%
        dplyr::pull(reference_type),
      c("difference", "ratio")
    ))

  }
}
