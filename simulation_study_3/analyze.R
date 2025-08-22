library(tidyverse)

simulations <- read_rds("/gpfs/home/susmah01/PenalizedCausalInference/results/indirect_results.rds")

summarized_results <- simulations |> 
  group_by(m, N, method) |>
  mutate(
    error = smr - psi, 
    covered = lower < smr & upper > smr,
    covered_conservative = covered
  ) |> 
  summarize(n = n(), mse = mean(error^2), coverage_conservative = mean(covered_conservative), coverage = mean(covered), me = mean(error)) |>
  arrange(m, N)

results_table <- summarized_results |> 
  filter(method != "l0") |> 
  pivot_wider(names_from = "method", values_from = c("mse", "me", "coverage", "coverage_conservative")) |>
  arrange(m, N) |> 
  ungroup() |>
  select(m, N, mse_unpenalized, mse_l1, mse_l2, mse_empirical_bayes, me_unpenalized, me_l1, me_l2, me_empirical_bayes, coverage_unpenalized, coverage_l1, coverage_l2, coverage_empirical_bayes)

remove_dups <- \(x) {
  x <- as.character(x)
  x[x == lag(x)] = ""
  x
}

results_table_latex <- results_table |>
  mutate_at(vars(m), remove_dups) |>
  mutate_at(vars(starts_with("mse"), starts_with("me")), scales::number_format(accuracy = 0.1, scale = 100)) |>
  mutate_at(vars(starts_with("coverage")), scales::percent_format(accuracy = 0.1)) |>
  knitr::kable(format = "latex")


results_table_latex_mse <- results_table |>
  select(N, starts_with("mse")) |>
  mutate_at(vars(starts_with("mse")), scales::number_format(accuracy = 0.1, scale = 100)) |>
  knitr::kable(format = "latex")

results_table_latex_me <- results_table |>
  select(N, starts_with("me")) |>
  mutate_at(vars(starts_with("me")), scales::number_format(accuracy = 0.1, scale = 100)) |>
  knitr::kable(format = "latex")

results_table_latex_coverage <- results_table |>
  select(N, starts_with("coverage")) |>
  mutate_at(vars(starts_with("coverage")), scales::percent_format(accuracy = 0.1)) |>
  knitr::kable(format = "latex")
