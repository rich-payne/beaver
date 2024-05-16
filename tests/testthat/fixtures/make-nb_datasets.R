
#General----


#Monotone increasing----

doses <- c(0, 25, 50, 100)
n_per_dose <- 50

mu <- DoseFinding::linear(doses, 10, 1)
r <- 10
p <- r / (r + mu)


#>Generate dataset----

set.seed(1313)

nb_monotone_incr <-
  mapply(
    rnbinom,
    prob = p,
    MoreArgs = list(
      n = n_per_dose,
      size = r
    ),
    SIMPLIFY = TRUE
  ) %>%
  tibble::as_tibble() %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "name",
    values_to = "response"
  ) %>%
  dplyr::mutate(dose = dplyr::case_when(
    name == "V1" ~ doses[1],
    name == "V2" ~ doses[2],
    name == "V3" ~ doses[3],
    name == "V4" ~ doses[4]
  )) %>%
  dplyr::mutate(dose = dose / 100) %>%
  dplyr::select(- name) %>%
  dplyr::relocate(dose) %>%
  dplyr::arrange(dose) %>%
  dplyr::mutate(log_response = log(response))

#>Summarize and visualize dataset----

nb_monotone_incr_summary <- nb_monotone_incr %>%
  dplyr::group_by(dose) %>%
  dplyr::summarize(
    response_n = n(),
    response_mean = mean(response),
    response_sd = sd(response),
    # log_response_mean = mean(log_response), # nolint
    # log_response_sd = sd(log_response) # nolint
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(response_mean_se = response_sd / sqrt(response_n)) %>%
  dplyr::relocate(response_mean_se, .after = response_sd) %>%
  dplyr::mutate(
    log_response_mean = log(response_mean)
  ) %>%
  tibble::add_column(mu = mu) %>%
  tibble::add_column(sigma = sqrt(r * (1 - p) / p^2)) %>%
  dplyr::relocate(c("mu", "sigma"), .after = dose)

ggplot(data = nb_monotone_incr, aes(x = dose, y = response)) +
  geom_point() +
  geom_line(
    data = nb_monotone_incr %>%
      dplyr::group_by(dose) %>%
      dplyr::summarize(
        response = mean(response)
      ) %>%
      dplyr::ungroup()
  ) +
  scale_x_continuous(
    breaks = nb_monotone_incr %>% dplyr::distinct(dose) %>% dplyr::pull(dose)
  )

#>Save----

saveRDS(
  nb_monotone_incr,
  file = test_path("fixtures", "nb_monotone_incr.rds")
)


#Monotone increasing (new)----

doses <- c(0, 25, 50, 100)
n_per_dose <- 10

mu <- DoseFinding::linear(doses, 10, 1)
r <- 10
p <- r / (r + mu)


#>Generate dataset----

set.seed(2424)

nb_monotone_incr_new <-
  mapply(
    rnbinom,
    prob = p,
    MoreArgs = list(
      n = n_per_dose,
      size = r
    ),
    SIMPLIFY = TRUE
  ) %>%
  tibble::as_tibble() %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "name",
    values_to = "response"
  ) %>%
  dplyr::mutate(dose = dplyr::case_when(
    name == "V1" ~ doses[1],
    name == "V2" ~ doses[2],
    name == "V3" ~ doses[3],
    name == "V4" ~ doses[4]
  )) %>%
  dplyr::mutate(dose = dose / 100) %>%
  dplyr::select(- name) %>%
  dplyr::relocate(dose) %>%
  dplyr::arrange(dose) %>%
  dplyr::mutate(log_response = log(response))

#>Summarize and visualize dataset----

nb_monotone_incr_new_summary <- nb_monotone_incr_new %>%
  dplyr::group_by(dose) %>%
  dplyr::summarize(
    response_n = n(),
    response_mean = mean(response),
    response_sd = sd(response),
    # log_response_mean = mean(log_response), # nolint
    # log_response_sd = sd(log_response) # nolint
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(response_mean_se = response_sd / sqrt(response_n)) %>%
  dplyr::relocate(response_mean_se, .after = response_sd) %>%
  dplyr::mutate(
    log_response_mean = log(response_mean)
  ) %>%
  tibble::add_column(mu = mu) %>%
  tibble::add_column(sigma = sqrt(r * (1 - p) / p^2)) %>%
  dplyr::relocate(c("mu", "sigma"), .after = dose)

ggplot(data = nb_monotone_incr_new, aes(x = dose, y = response)) +
  geom_point() +
  geom_line(
    data = nb_monotone_incr_new %>%
      dplyr::group_by(dose) %>%
      dplyr::summarize(
        response = mean(response)
      ) %>%
      dplyr::ungroup()
  ) +
  scale_x_continuous(
    breaks = nb_monotone_incr_new %>%
      dplyr::distinct(dose) %>%
      dplyr::pull(dose)
  )

#>Save----

saveRDS(
  nb_monotone_incr_new,
  file = test_path("fixtures", "nb_monotone_incr_new.rds")
)


#Monotone increasing w/covariates----

doses <- c(0, 25, 50, 100)
n_per_dose <- 50

set.seed(1234)

covariates <- data.frame(
  age = runif(length(doses) * n_per_dose, 18, 65)
)
x <- covariates %>%
  model.matrix(~ age, data = .)
b1 <- c(x %*% c(0, 10 / 40))
covariates_mod <- covariates %>%
  tibble::add_column(b1)

mus <- covariates_mod %>%
  tibble::add_column(dose = rep(doses, each = n_per_dose)) %>%
  dplyr::mutate(mu = DoseFinding::linear(dose, b1, 1))

mu <- mus %>%
  dplyr::pull(mu)
r <- 10
p <- r / (r + mu)


#>Generate dataset----

set.seed(3636)

nb_monotone_incr_cov <-
  mapply(
    rnbinom,
    prob = p,
    MoreArgs = list(
      n = 1,
      size = r
    ),
    SIMPLIFY = TRUE
  ) %>%
  tibble::as_tibble() %>%
  dplyr::rename(response = value) %>%
  dplyr::bind_cols(
    mus %>% dplyr::select(age, dose)
  ) %>%
  dplyr::mutate(dose = dose / 100) %>%
  dplyr::relocate(dose, age) %>%
  dplyr::arrange(dose) %>%
  dplyr::mutate(log_response = log(response))

#>Summarize and visualize dataset----

nb_monotone_incr_cov_summary <- nb_monotone_incr_cov %>%
  dplyr::group_by(dose) %>%
  dplyr::summarize(
    response_n = n(),
    response_mean = mean(response),
    response_sd = sd(response),
    # log_response_mean = mean(log_response), # nolint
    # log_response_sd = sd(log_response) # nolint
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(response_mean_se = response_sd / sqrt(response_n)) %>%
  dplyr::relocate(response_mean_se, .after = response_sd) %>%
  dplyr::mutate(
    log_response_mean = log(response_mean)
  )

ggplot(data = nb_monotone_incr_cov, aes(x = dose, y = response)) +
  geom_point() +
  geom_line(
    data = nb_monotone_incr_cov %>%
      dplyr::group_by(dose) %>%
      dplyr::summarize(
        response = mean(response)
      ) %>%
      dplyr::ungroup()
  ) +
  scale_x_continuous(
    breaks = nb_monotone_incr_cov %>%
      dplyr::distinct(dose) %>%
      dplyr::pull(dose)
  )

#>Save----

saveRDS(
  nb_monotone_incr_cov,
  file = test_path("fixtures", "nb_monotone_incr_cov.rds")
)


#Monotone increasing w/covariates (new)----

doses <- c(0, 25, 50, 100)
n_per_dose <- 10

set.seed(5678)

covariates <- data.frame(
  age = runif(length(doses) * n_per_dose, 18, 65)
)
x <- covariates %>%
  model.matrix(~ age, data = .)
b1 <- c(x %*% c(0, 10 / 40))
covariates_mod <- covariates %>%
  tibble::add_column(b1)

mus <- covariates_mod %>%
  tibble::add_column(dose = rep(doses, each = n_per_dose)) %>%
  dplyr::mutate(mu = DoseFinding::linear(dose, b1, 1))

mu <- mus %>%
  dplyr::pull(mu)
r <- 10
p <- r / (r + mu)


#>Generate dataset----

set.seed(4848)

nb_monotone_incr_cov_new <- mapply(
  rnbinom,
  prob = p,
  MoreArgs = list(
    n = 1,
    size = r
  ),
  SIMPLIFY = TRUE
) %>%
  tibble::as_tibble() %>%
  dplyr::rename(response = value) %>%
  dplyr::bind_cols(
    mus %>% dplyr::select(age, dose)
  ) %>%
  dplyr::mutate(dose = dose / 100) %>%
  dplyr::relocate(dose, age) %>%
  dplyr::arrange(dose) %>%
  dplyr::mutate(log_response = log(response))

#>Summarize and visualize dataset----

nb_monotone_incr_cov_new_summary <- nb_monotone_incr_cov_new %>% # nolint
  dplyr::group_by(dose) %>%
  dplyr::summarize(
    response_n = n(),
    response_mean = mean(response),
    response_sd = sd(response),
    # log_response_mean = mean(log_response), # nolint
    # log_response_sd = sd(log_response) # nolint
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(response_mean_se = response_sd / sqrt(response_n)) %>%
  dplyr::relocate(response_mean_se, .after = response_sd) %>%
  dplyr::mutate(
    log_response_mean = log(response_mean)
  )

ggplot(data = nb_monotone_incr_cov_new, aes(x = dose, y = response)) +
  geom_point() +
  geom_line(
    data = nb_monotone_incr_cov_new %>%
      dplyr::group_by(dose) %>%
      dplyr::summarize(
        response = mean(response)
      ) %>%
      dplyr::ungroup()
  ) +
  scale_x_continuous(
    breaks = nb_monotone_incr_cov_new %>%
      dplyr::distinct(dose) %>%
      dplyr::pull(dose)
  )

#>Save----

saveRDS(
  nb_monotone_incr_cov_new,
  file = test_path("fixtures", "nb_monotone_incr_cov_new.rds")
)


#Cleanup----

rm(doses, n_per_dose, mu, r, p, nb_monotone_incr, nb_monotone_incr_summary)
rm(nb_monotone_incr_new, nb_monotone_incr_new_summary)
rm(covariates, x, b1, covariates_mod, mus, nb_monotone_incr_cov, nb_monotone_incr_cov_summary) # nolint
rm(nb_monotone_incr_cov_new, nb_monotone_incr_cov_new_summary)
