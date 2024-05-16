
jags_model_checks <- function(model,
                              model_class) {
  expect_s3_class(model, c("fs_path", "character"), exact = TRUE)
  class(model) <- class(model)[- which(class(model) == "fs_path")]
  expect_true(
    identical(
      model,
      system.file(
        "jags",
        paste0(
          stringr::word(
            model_class,
            2,
            stringr::str_count(model_class, pattern = "_") + 1,
            sep = "_"
          ),
          ".jags"
        ),
        package = "beaver"
      )
    ) ||
      identical(
        model,
        system.file(
          "jags",
          paste0(
            stringr::word(model_class, 2, sep = "_"),
            "_",
            stringr::str_replace_all(
              stringr::word(
                model_class,
                3,
                stringr::str_count(model_class, pattern = "_") + 1,
                sep = "_"
              ),
              "_",
              ""
            ),
            ".jags"
          ),
          package = "beaver"
        )
      )
  )
}
