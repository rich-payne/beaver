
get_jags_data_checks <- function(jags_data,
                                 data,
                                 x) {
  data_doses <- data %>%
    dplyr::distinct(.data$dose) %>%
    dplyr::arrange(.data$dose) %>%
    dplyr::pull(dose)

  expect_true(is.list(jags_data))
  expect_named(
    jags_data,
    c("y", "n_obs", "dose_index", "dose", "n_doses", "x", "k")
  )
  expect_identical(jags_data$y, data %>% dplyr::pull(response))
  expect_identical(jags_data$n_obs, nrow(data))
  expect_identical(
    jags_data$dose_index,
    data %>%
      dplyr::rowwise() %>%
      dplyr::mutate(dose_index =  which(.data$dose == data_doses)) %>%
      dplyr::ungroup() %>%
      dplyr::pull(dose_index)
  )
  expect_identical(jags_data$dose, data_doses)
  expect_identical(jags_data$n_doses, length(data_doses))
  expect_identical(jags_data$x, x)
  expect_identical(jags_data$k, ncol(x))
}
