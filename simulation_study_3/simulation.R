dyn.load("/gpfs/share/apps/gcc/11.2.0/lib64/libstdc++.so.6")
library(tidyverse)
library(CausalShrink)
library(TargetedSMR)
library(broom)
library(mlr3extralearners)

source("simulate.R")

estimator <- function(data, m, seed, P) {
  N <- nrow(data)
  learners <- list(
    list("lightgbm", num_iterations = 200), 
    list("lightgbm", num_iterations = 100), 
    list("lightgbm", num_iterations = 50), 
    "glm", 
    "gam"
  )

  largedat <- simulate(N = 1e6, m = m, seed = seed, P = P)
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

  baseline <- paste0("X", 1:P)
  folds <- 5

  cache_file <- glue::glue("/gpfs/scratch/susmah01/PenalizedCausalInference/indirect_cache/fits/{seed}_{m}_{N}.rds")
  if(file.exists(cache_file)) {
    fit <- read_rds(cache_file)
  }
  else {
    fit <- indirect_tmle(
      data,
      outcome = "Y",
      trt = "A",
      baseline = baseline,
      trt_method = "default",
      outcome_type = "continuous",
      learners_trt = learners,
      learners_outcome = learners,
      folds = folds,
      control = standardization_control(.learners_trt_folds = 5, .learners_outcome_folds = 5)
    ) 
    write_rds(fit, cache_file)
  }

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
  left_join(truth %>% select(trt, smr))

  return(res)
}

index <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
P <- 5

filename <- glue::glue("/gpfs/scratch/susmah01/PenalizedCausalInference/indirect_cache/{index}.rds")

simulations <- expand_grid(
  index = index,
  m = 50,
  N = c(3e3, 5e3, 1e4)
) %>%
  mutate(
    seed = index * 1e5 + 1:n(),
    data = pmap(list(seed, N, m), simulate, P = P),
    results = pmap(list(data, m, seed), estimator, P = P)
  ) %>%
  select(-data) %>%
  unnest(results)

write_rds(simulations, filename)
