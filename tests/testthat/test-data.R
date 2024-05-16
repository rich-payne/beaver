
test_that("data_negbin_emax produces an S3 data.frame object with correct properties", { # nolint

  n_per_arm <- 50
  doses <- c(0.00, 0.25, 0.50, 1.00)
  p <- 0.3

  #no covariates----

  data1 <- data_negbin_emax(
    n_per_arm = n_per_arm,
    doses = doses,
    b1 = log(10),
    b2 = 3,
    b3 = 0.25,
    ps = p
  )

  data_negbin_emax_checks(
    data = data1,
    n_per_arm = n_per_arm,
    doses = doses,
    p = p
  )

  #covariates----

  set.seed(1234)
  covariates <- data.frame(
    log_age = log(runif(length(doses) * n_per_arm, 18, 65))
  )
  x <- covariates %>%
    model.matrix(~ log_age, data = .)

  data2 <- data_negbin_emax(
    n_per_arm = n_per_arm,
    doses = doses,
    b1 = c(0, log(10) / log(40)),
    b2 = 3,
    b3 = 0.25,
    ps = p,
    x = x
  )

  data_negbin_emax_checks(
    data = data2,
    n_per_arm = n_per_arm,
    doses = doses,
    p = p,
    x = x
  )
})
