assert_models <- function(x) {
  beaver <- vapply(x, inherits, what = "beaver_model", logical(1))
  if (!all(beaver)) {
    rlang::abort(
      "All models must have class \"beaver_model\"",
      class = "beaver"
    )
  }
}

assert_data <- function(data, formula) {
  vars <- c(all.vars(formula), "dose", "response")
  if (!all(vars %in% colnames(data))) {
    rlang::abort(
      paste0(
        "data must have columns \"dose\", \"response\", and any variables",
        " in the formula argument."
      ),
      class = "beaver"
    )
  }
}

assert_matrix <- function(x, can_be_null = FALSE) {
  nme <- deparse(substitute(x))
  msg <- paste0("\"", nme, "\" must be a matrix.")
  if ((!can_be_null && is.null(x))) {
    rlang::abort(msg, class = "beaver")
  }
  if (!is.null(x) && !is.matrix(x)) {
    rlang::abort(msg, class = "beaver")
  }
}

assert_length <- function(x, len, can_be_null = FALSE) {
  if (can_be_null && is.null(x)) return()
  bad_null <- !can_be_null && is.null(x)
  if ((length(x) != len) || bad_null) {
    name <- deparse(substitute(x))
    rlang::abort(
      paste0("\"", name, "\"", " must have length ", len, "."),
      class = "beaver"
    )
  }
}

assert_misc_lengths <- function(x, doses) {
  lengths_differ <- length(doses) != length(x)
  name <- deparse(substitute(x))
  if (lengths_differ && length(x) != 1) {
    rlang::abort(
      paste0("length(", name, ") must be 1 or equal length(doses)"),
      class = "beaver"
    )
  }
}

assert_r_positive <- function(x) {
  if (!all(x$r > 0)) {
    df_chr <- dplyr::distinct(x, .data$dose, .data$mu, .data$r) %>%
      utils::capture.output() %>%
      paste(collapse = "\n")
    rlang::abort(
      c(
        "All \"r\" must be positive.",
        "i" = paste0(
          " Inspect output for negative r's."
        ),
        df_chr,
        "i" = "Choose different hyperparameter."
      ),
      class = "beaver"
    )
  }
}

assert_prior_weights <- function(models) {
  w_prior <- get_prior_weights(models)
  if (!isTRUE(all.equal(1, sum(w_prior)))) {
    rlang::abort("Prior weights must sum to 1.", class = "beaver")
  }
}

get_prior_weights <- function(models) {
  purrr::map_dbl(models, ~ .x$w_prior)
}

assert_n_rows <- function(x, n) {
  if (nrow(x) != n) {
    nme <- deparse(substitute(x))
    rlang::abort(
      paste0("\"", nme, "\" must have ", n, " rows."),
      class = "beaver"
    )
  }
}

assert_new_data_and_contrast <- function(new_data, contrast) {
  if (is.null(new_data) && is.null(contrast)) {
    rlang::abort(
      "\"new_data\" or \"contrast\" must be specified.",
      class = "beaver"
    )
  }
  if (!is.null(new_data) && !is.null(contrast)) {
    rlang::abort(
      "Only one of \"new_data\" and \"contrast\" can be specified.",
      class = "beaver"
    )
  }
}

assert_doses <- function(x, doses) {
  if (is.null(doses)) return()
  all_doses <- attr(x, "doses")
  if (!all(doses %in% all_doses)) {
    msg <- paste0(
      "Doses must be one of ", paste(all_doses, collapse = ", "), "."
    )
    rlang::abort(msg, class = "beaver")
  }
}
