#' @import ggplot2
#' @export
plot.beaver_mcmc <- function(
  x,
  doses = attr(x, "doses"),
  prob = c(.025, .975),
  new_data = NULL,
  contrast = NULL,
  type = c("ls", "g-comp"),
  ...
) {
  if (length(prob) != 2)
    rlang::abort("\"prob\" must have length 2.", class = "miscbayes")
  type <- match.arg(type)
  # no_covariates <- formula(~1) == attr(mcmc_indep, "formula") # nolint
  # no_contrast <- (is.null(new_data) && is.null(contrast)) # nolint
  if (type == "ls") {
    if (!is.null(new_data) && nrow(new_data) > 1) {
      rlang::abort(
        "\"new_data\" must have only one row for plotting.",
        class = "beaver"
      )
    }
    if (!is.null(contrast) && nrow(contrast) > 1) {
      rlang::abort(
        "\"contrast\" must have only one row for plotting.",
        class = "beaver"
      )
    }
    data_plot <- posterior(
      x,
      doses = doses,
      prob = prob,
      new_data = new_data,
      contrast = contrast,
      ...
    )$stats
  } else if (type == "g-comp") {
    data_plot <- posterior_g_comp(
      x,
      prob = prob,
      new_data = new_data
    )$stats
  }
  colnames(data_plot)[grepl("%", colnames(data_plot))] <- c("lb", "ub")
  p <- ggplot(data_plot, aes(.data$dose, .data$value)) +
    geom_errorbar(aes(ymin = .data$lb, ymax = .data$ub)) +
    geom_point(shape = 24) +
    scale_x_continuous(breaks = doses) +
    labs(x = "Dose", y = "Posterior Mean")
  return(p)
}
