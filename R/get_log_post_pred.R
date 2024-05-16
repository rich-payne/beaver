get_log_post_pred <- function(samples, data, x, n_models) {
  samps <- draws(samples)
  n_mcmc <- nrow(samps)
  if (n_models < 2) {
    return(matrix(1, 1, 1))
  }
  u_doses <- sort(unique(data$dose))
  data <- data %>%
    dplyr::mutate(
      dose_index = vapply(
        .data$dose,
        function(xx) which(xx == !!u_doses),
        integer(1)
      )
    )
  intercept <- get_intercept(samps, contrast = x)
  doses <- rep(data$dose, each = n_mcmc)
  mu <- get_mean(samples, doses, intercept, u_doses = u_doses)
  p <- get_p(data$dose_index, samps)
  r <- mu * p / (1 - p)
  log_post_pred <-
    t(dnbinom(data$response, size = t(r), prob = t(p), log = TRUE))
  return(log_post_pred)
}

get_p <- function(dose_index, samps) {
  p <- matrix(NA_real_, nrow(samps), length(dose_index))
  for (i in seq_len(length(dose_index))) {
    p[, i] <- samps[[paste0("p[", dose_index[i], "]")]]
  }
  p
}
