
pr_eoi_checks <- function(pr_eoi,
                          samples,
                          contrast,
                          posteriors,
                          eoi) {
  expect_failure(expect_s3_class(pr_eoi, NA))
  expect_s3_class(pr_eoi, "data.frame")
  expect_identical(
    nrow(pr_eoi),
    length(attr(samples, "doses")) * nrow(contrast) * length(eoi)
  )
  expect_failure(expect_named(pr_eoi, NULL))
  expect_no_error(
    checkmate::assertSubset(
      names(pr_eoi),
      c(
        names(posteriors)[!names(posteriors) %in% c("value", "iter", "model")],
        "eoi",
        "prob",
        "direction"
      )
    )
  )
  for (i in names(pr_eoi)[!names(pr_eoi) %in% c("direction", "reference_type")]) expect_true(is.numeric(pr_eoi[[i]])) # nolint
  for (j in names(pr_eoi)[names(pr_eoi) %in% c("direction", "reference_type")]) expect_true(is.character(pr_eoi[[j]])) # nolint
  expect_identical(
    pr_eoi %>%
      dplyr::ungroup() %>%
      dplyr::distinct(.data$dose) %>%
      dplyr::arrange(.data$dose) %>%
      dplyr::pull(dose),
    attr(samples, "doses")
  )
  expect_identical(
    pr_eoi %>%
      dplyr::ungroup() %>%
      dplyr::distinct(.data$eoi) %>%
      dplyr::pull(eoi),
    eoi
  )
  expect_no_error(
    checkmate::assertNumeric(
      x = pr_eoi %>% dplyr::pull(prob),
      lower = 0,
      upper = 1,
      finite = TRUE,
      any.missing = FALSE,
      len = length(attr(samples, "doses")) * nrow(contrast) * length(eoi)
    )
  )

  if ("direction" %in% names(pr_eoi)) {

    expect_identical(
      pr_eoi %>%
        dplyr::ungroup() %>%
        dplyr::distinct(.data$direction) %>%
        dplyr::pull(direction),
      "greater"
    )

  }

  expect_identical(
    pr_eoi %>%
      dplyr::ungroup() %>%
      dplyr::distinct(.data$.contrast_index) %>%
      dplyr::arrange(.data$.contrast_index) %>%
      dplyr::pull(.contrast_index),
    posteriors %>%
      dplyr::distinct(.data$.contrast_index) %>%
      dplyr::arrange(.data$.contrast_index) %>%
      dplyr::pull(.contrast_index)
  )
  expect_identical(
    pr_eoi %>%
      dplyr::ungroup() %>%
      dplyr::distinct(!!!syms(names(posteriors)[!names(posteriors) %in% c(
        "dose",
        "value",
        "iter",
        "model",
        "reference_dose",
        "reference_type"
      )])) %>%
      dplyr::arrange(names(posteriors)[!names(posteriors) %in% c(
        "dose",
        "value",
        "iter",
        ".contrast_index",
        "model",
        "reference_dose",
        "reference_type"
      )]) %>%
      dplyr::pull(names(posteriors)[!names(posteriors) %in% c(
        "dose",
        "value",
        "iter",
        ".contrast_index",
        "model",
        "reference_dose",
        "reference_type"
      )]),
    posteriors %>%
      dplyr::distinct(!!!syms(names(posteriors)[!names(posteriors) %in% c(
        "dose",
        "value",
        "iter",
        "model",
        "reference_dose",
        "reference_type"
      )])) %>%
      dplyr::arrange(names(posteriors)[!names(posteriors) %in% c(
        "dose",
        "value",
        "iter",
        ".contrast_index",
        "model",
        "reference_dose",
        "reference_type"
      )]) %>%
      dplyr::pull(names(posteriors)[!names(posteriors) %in% c(
        "dose",
        "value",
        "iter",
        ".contrast_index",
        "model",
        "reference_dose",
        "reference_type"
      )])
  )

  if ("reference_dose" %in% names(posteriors)) {

    expect_identical(
      pr_eoi %>%
        dplyr::ungroup() %>%
        dplyr::distinct(.data$reference_dose) %>%
        dplyr::pull(reference_dose),
      posteriors %>%
        dplyr::distinct(.data$reference_dose) %>%
        dplyr::pull(reference_dose)
    )

  }

  if ("reference_type" %in% names(posteriors)) {

    expect_identical(
      pr_eoi %>%
        dplyr::ungroup() %>%
        dplyr::distinct(.data$reference_type) %>%
        dplyr::pull(reference_type),
      posteriors %>%
        dplyr::distinct(.data$reference_type) %>%
        dplyr::pull(reference_type)
    )

  }
}

pr_eoi_g_comp_checks <- function(pr_eoi,
                                 samples,
                                 posteriors,
                                 eoi) {
  expect_failure(expect_s3_class(pr_eoi, NA))
  expect_s3_class(pr_eoi, "data.frame")
  expect_identical(nrow(pr_eoi), length(attr(samples, "doses")) * length(eoi))
  expect_failure(expect_named(pr_eoi, NULL))
  expect_no_error(
    checkmate::assertSubset(
      names(pr_eoi),
      c(
        names(posteriors)[!names(posteriors) %in% c("value", "iter", "model")],
        "eoi",
        "prob",
        "direction"
      )
    )
  )
  for (i in names(pr_eoi)[!names(pr_eoi) %in% c("direction", "reference_type")]) expect_true(is.numeric(pr_eoi[[i]])) # nolint
  for (j in names(pr_eoi)[names(pr_eoi) %in% c("direction", "reference_type")]) expect_true(is.character(pr_eoi[[j]])) # nolint
  expect_identical(
    pr_eoi %>%
      dplyr::ungroup() %>%
      dplyr::distinct(dose) %>%
      dplyr::arrange(dose) %>%
      dplyr::pull(dose),
    attr(samples, "doses")
  )
  expect_identical(
    pr_eoi %>%
      dplyr::ungroup() %>%
      dplyr::distinct(eoi) %>%
      dplyr::pull(eoi),
    eoi
  )
  expect_no_error(
    checkmate::assertNumeric(
      x = pr_eoi %>% dplyr::pull(prob),
      lower = 0,
      upper = 1,
      finite = TRUE,
      any.missing = FALSE,
      len = length(attr(samples, "doses")) * length(eoi)
    )
  )

  if ("direction" %in% names(pr_eoi)) {

    expect_identical(
      pr_eoi %>%
        dplyr::ungroup() %>%
        dplyr::distinct(direction) %>%
        dplyr::pull(direction),
      "greater"
    )

  }

  if ("reference_dose" %in% names(posteriors)) {

    expect_identical(
      pr_eoi %>%
        dplyr::ungroup() %>%
        dplyr::distinct(reference_dose) %>%
        dplyr::pull(reference_dose),
      posteriors %>%
        dplyr::distinct(reference_dose) %>%
        dplyr::pull(reference_dose)
    )

  }

  if ("reference_type" %in% names(posteriors)) {

    expect_identical(
      pr_eoi %>%
        dplyr::ungroup() %>%
        dplyr::distinct(reference_type) %>%
        dplyr::pull(reference_type),
      posteriors %>%
        dplyr::distinct(reference_type) %>%
        dplyr::pull(reference_type)
    )

  }
}
