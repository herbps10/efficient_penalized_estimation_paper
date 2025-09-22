estimator <- function(data, m, seed, index, P, path) {
  if(!file.exists(dirname(path))) dir.create(dirname(path), recursive = TRUE)
  if(file.exists(path)) return(read_rds(path))

  N <- nrow(data)
  learners <- list(
    list("lightgbm", num_iterations = 200), 
    list("lightgbm", num_iterations = 100), 
    list("lightgbm", num_iterations = 50), 
    "gam",
    "glm" 
  )

  trt_sizes <- count(data, A) |> rename(trt_size = n, trt = A) |> mutate(trt = as.character(trt))

  # Each obs must have at least 10 observations
  data <- data |>
    filter(as.character(A) %in% filter(trt_sizes, trt_size >= 10)$trt)

  largedat <- simulate(N = 1e6, m = m, seed = seed, P = P, outcome_type = "binomial")
  truth <- largedat %>% group_by(A) %>% 
    summarize(psi1 = mean(gamma1), 
              psi2 = mean(gamma2),
        n = n()) %>%
    ungroup() %>%
    mutate(a = as.character(A)) %>%
    mutate(smr = psi1 / psi2 - 1) %>%
    arrange(A) %>%
    rename(trt = A) %>%
    mutate(trt = as.character(trt))
  rm(largedat)

  baseline <- paste0("W", 1:P)
  folds <- 5

  comparison_fe <- indirect_pprof(data, outcome = "Y", baseline = baseline, trt = "A", outcome_type = "binomial", model = "fe") |>
    mutate(psi = psi - 1, lower = lower - 1, upper = upper - 1)
  comparison_re <- indirect_pprof(data, outcome = "Y", baseline = baseline, trt = "A", outcome_type = "binomial", model = "re") |>
    mutate(psi = psi - 1, lower = lower - 1, upper = upper - 1)

  fit <- indirect_tmle(
    data,
    outcome = "Y",
    trt = "A",
    baseline = baseline,
    outcome_type = "binomial",
    learners_trt = learners,
    learners_outcome = learners,
    folds = folds,
    control = standardization_control(.learners_trt_folds = 5, .learners_outcome_folds = 5)
  ) 

  fit_tidy <- tidy(fit) %>% 
    filter(parameter == "SMR") %>% 
    arrange(trt)

  point_estimates   <- fit_tidy$estimate - 1
  se                <- fit_tidy$std.error
  efficiency_bounds <- fit_tidy$std.error^2 * N

  fit_l0 <- CausalShrink::causal_shrink(point_estimates, efficiency_bounds, N, method = "l0", lambda_max = 2)
  fit_l1 <- CausalShrink::causal_shrink(point_estimates, efficiency_bounds, N, method = "l1", lambda_max = 2)
  fit_l2 <- CausalShrink::causal_shrink(point_estimates, efficiency_bounds, N, method = "l2", lambda_max = 2)
  fit_l2_adaptive <- CausalShrink::causal_shrink(point_estimates, efficiency_bounds, N, method = "empirical_bayes", lambda_max = 2)

  l2_eb_se <- se * fit_l2$shrinkage
  l2_adaptive_eb_se <- se * fit_l2_adaptive$shrinkage

  lower <- c(
    point_estimates + qnorm(0.025) * se, 
    fit_l0$point_estimates + qnorm(0.025) * se, 
    fit_l1$point_estimates + qnorm(0.025) * se, 
    fit_l2$point_estimates + qnorm(0.025) * l2_eb_se,
    fit_l2_adaptive$point_estimates + qnorm(0.025) * l2_adaptive_eb_se
  )
  upper <- c(
    point_estimates + qnorm(0.975) * se, 
    fit_l0$point_estimates + qnorm(0.975) * se, 
    fit_l1$point_estimates + qnorm(0.975) * se, 
    fit_l2$point_estimates + qnorm(0.975) * l2_eb_se,
    fit_l2_adaptive$point_estimates + qnorm(0.975) * l2_adaptive_eb_se 
  )

  psi <- c(point_estimates, fit_l0$point_estimates, fit_l1$point_estimates, fit_l2$point_estimates, fit_l2_adaptive$point_estimates)

  res <- tibble(
    method   = rep(c("unpenalized", "l0", "l1", "l2", "empirical_bayes"), each = length(point_estimates)),
    trt      = rep(fit_tidy$trt, times = 5),
    psi      = psi,
    lower    = lower,
    upper    = upper,
    se       = c(se, se, se, l2_eb_se, l2_adaptive_eb_se)
  ) %>%
    bind_rows(comparison_fe) |>
    bind_rows(comparison_re) |>
    mutate(N = N, m = m, index = index, seed = seed) |>
    left_join(truth %>% select(trt, smr)) |>
    left_join(trt_sizes)

  write_rds(res, path, compress = "gz")

  return(res)
}
