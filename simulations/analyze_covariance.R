library(tidyverse)

simulations <- read_rds("/gpfs/home/susmah01/PenalizedCausalInference/results/covariance_results.rds")

results <- simulations |>
  select(-data) |>
  mutate(point_estimates = pmap(list(index, fit, glmnet_l1, glmnet_l2, ols), \(index, fit, glmnet_l1, glmnet_l2, ols) {
    tibble(
      method = c(
        rep("unpenalized", length(fit$unpenalized$point_estimates)), 
        rep("l0", length(fit$l1$point_estimates)), 
        rep("l1", length(fit$l1$point_estimates)), 
        rep("l2", length(fit$l2$point_estimates)),
        rep("glmnet_l1", length(glmnet_l1$point_estimates)),
        rep("glmnet_l2", length(glmnet_l2$point_estimates)),
        rep("ols", length(ols$point_estimates))
      ),
      variable = c(
        names(fit$unpenalized$point_estimates), 
        names(fit$unpenalized$point_estimates), 
        names(fit$l1$point_estimates), 
        names(fit$l2$point_estimates),
        names(glmnet_l1$point_estimates),
        names(glmnet_l2$point_estimates),
        names(ols$point_estimates)
      ),
      point_estimates = c(
        fit$unpenalized$point_estimates, 
        fit$l0$point_estimates, 
        fit$l1$point_estimates, 
        fit$l2$point_estimates,
        glmnet_l1$point_estimates,
        glmnet_l2$point_estimates,
        ols$point_estimates
      ),
      lower = c(
        fit$unpenalized$lower, 
        fit$l0$lower, 
        fit$l1$lower, 
        fit$l2$lower,
        glmnet_l1$lower,
        glmnet_l2$lower,
        ols$lower
      ),
      upper = c(
        fit$unpenalized$upper, 
        fit$l0$upper, 
        fit$l1$upper, 
        fit$l2$upper,
        glmnet_l1$upper,
        glmnet_l2$upper,
        ols$upper
      )
    )
  })) |>
  select(-fit, -glmnet_l1, -glmnet_l2, -ols) |>
  unnest(point_estimates) |>
  left_join(simulations |> select(seed, D, data, N, dgp, sigma, theta) |> mutate(beta = map(data, `[[`, "beta")) |> select(-data) |> unnest())

summarized_results_combined <- results |>
  mutate(error = beta - point_estimates) |>
  group_by(dgp, N, sigma, theta, method, D) |>
  summarize(me = mean(error, na.rm = TRUE),
            mse = mean(error^2, na.rm = TRUE),
            var = var(error),
            coverage = mean(lower < beta & upper > beta, na.rm = TRUE))

write_rds(summarized_results_combined, "/gpfs/home/susmah01/PenalizedCausalInference/results/summarized_results_combined.rds")

remove_dups <- \(x) {
  x <- as.character(x)
  x[x == lag(x)] = ""
  x
}

results_table <- summarized_results_combined |>
  filter(D == 100) |>
  ungroup() |>
  select(-dgp, coverage) |>
  filter(method %in% c("glmnet_l1", "glmnet_l2", "unpenalized", "l1", "l2", "ols")) |>
  pivot_wider(names_from = "method", values_from = c("mse", "me", "coverage", "coverage", "var")) |>
  select(sigma, N, mse_unpenalized, mse_ols, mse_l1, mse_glmnet_l1, mse_l2, mse_glmnet_l2, me_unpenalized, me_ols, me_l2, me_glmnet_l2, me_l1, me_glmnet_l1, var_unpenalized, var_ols, var_l2, var_glmnet_l2, var_l1, var_glmnet_l1, coverage_unpenalized, coverage_ols, coverage_l1, coverage_glmnet_l1, coverage_l2, coverage_glmnet_l2) |> 
  arrange(sigma, N) 

results_table_latex <- results_table |>
  select(-starts_with("var"), -starts_with("coverage")) |>
  select(-ends_with("ols")) |>
  mutate_at(vars(sigma), remove_dups) |>
  mutate_at(vars(starts_with("mse"), starts_with("me"), starts_with("var")), scales::number_format(accuracy = 0.1, scale = 100)) |>
  mutate_at(vars(starts_with("coverage")), scales::percent_format(accuracy = 0.1)) |>
  knitr::kable(format = "latex")

results_table_latex2 <- results_table |>
  select(-starts_with("mse"), -starts_with("me")) |>
  select(-ends_with("ols")) |>
  mutate_at(vars(sigma), remove_dups) |>
  mutate_at(vars(starts_with("mse"), starts_with("me"), starts_with("var")), scales::number_format(accuracy = 0.1, scale = 100)) |>
  mutate_at(vars(starts_with("coverage")), scales::percent_format(accuracy = 0.1)) |>
  knitr::kable(format = "latex")
