get_jags_data <- function(model, data, x) UseMethod("get_jags_data", model)

#' @keywords internal
#' @export
get_jags_data.default <- function(model, data, x) {
  u_doses <- sort(unique(data$dose))
  data <- data %>%
    dplyr::mutate(
      dose_index = vapply(
        .data$dose,
        function(xx, u_doses) which(xx == u_doses),
        u_doses = !!u_doses,
        integer(1)
      )
    )
  hyparms <- model
  hyparms$w_prior <- NULL
  jags_data <- c(
    list(
      y = data$response,
      n_obs = nrow(data),
      dose_index = data$dose_index,
      dose = u_doses,
      n_doses = length(unique(u_doses)),
      x = x,
      k = ncol(x)
    ),
    hyparms
  )
  invisible(jags_data)
}
