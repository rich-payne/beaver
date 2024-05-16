
model_checks <- function(model,
                         model_class,
                         function_args) {
  expect_failure(expect_s3_class(model, NA))
  expect_s3_class(model, model_class, exact = TRUE)

  expect_true(is.list(model))
  expect_identical(names(model), function_args)
  for (i in names(model)) expect_identical(model[[i]], 1)
}
