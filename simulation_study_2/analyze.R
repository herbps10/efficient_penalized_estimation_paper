library(tidyverse)
library(ebci)

root <- rprojroot::is_git_root                                                                         
basepath <- root$find_file("simulation_study_3")  

source(glue::glue("{basepath}/env.R"))
source(glue::glue("{basepath}/../R/helpers.R"))

results_path <- Sys.getenv("SIMULATION_RESULTS_PATH")
if(results_path == "") stop("Please set SIMULATION_RESULTS_PATH environment variable.")

simulations <- read_rds(glue::glue("{results_path}/simulation_results.rds"))

summarized_results <- simulations |> 
  group_by(G, theta, beta, N, method, sigma) |>
  mutate(
    error = true_psi - psi, 
    covered = lower < true_psi & upper > true_psi,
    ci_width = upper - lower,
  ) |> 
  summarize(
    n = n(), 
    mse = mean(error^2), 
    shrinkage = mean(shrinkage),
    coverage = mean(covered), 
    ci_width = mean(ci_width),
    var = var(error),
    me = mean(error)
  ) |>
  arrange(G, theta, beta, sigma, N)

results_table <- summarized_results |> 
  filter(method != "l0") |> 
  pivot_wider(names_from = "method", values_from = c("mse", "me", "coverage", "ci_width", "var", "shrinkage")) |>
  arrange(theta, sigma, N) |> 
  ungroup() |>
  select(
    theta, sigma, N, 
    mse_HKB, mse_unpenalized, mse_l1, mse_l2, mse_empirical_bayes,
    me_HKB, me_unpenalized, me_l1, me_l2, me_empirical_bayes,
    coverage_unpenalized, coverage_l1, coverage_l2, coverage_empirical_bayes,
    ci_width_unpenalized, ci_width_l1, ci_width_l2, ci_width_empirical_bayes
  )

results_table_latex <- results_table |>
  select(-starts_with("ci_width")) |>
  mutate_at(vars(theta, sigma), remove_dups) |>
  mutate_at(vars(starts_with("mse"), starts_with("me")), scales::number_format(accuracy = 0.1, scale = 100)) |>
  mutate_at(vars(starts_with("coverage")), scales::percent_format(accuracy = 0.1)) |>
  #mutate_at(vars(starts_with("ci_width")), scales::number_format(accuracy = 0.01)) |>
  knitr::kable(format = "latex")

results_table_latex_mse <- results_table |>
  select(theta, sigma, N, mse_unpenalized, mse_l1, mse_l2, mse_empirical_bayes) |>
  mutate_at(vars(theta, sigma), remove_dups) |>
  mutate_at(vars(starts_with("mse")), scales::number_format(accuracy = 0.1, scale = 100)) |>
  knitr::kable(format = "latex")

results_table_latex_me <- results_table |>
  select(theta, sigma, N, me_unpenalized, me_l1, me_l2, me_empirical_bayes) |>
  mutate_at(vars(theta, sigma), remove_dups) |>
  mutate_at(vars(starts_with("me")), scales::number_format(accuracy = 0.1, scale = 100)) |>
  knitr::kable(format = "latex")

results_table_latex_coverage <- results_table |>
  select(theta, sigma, N, coverage_unpenalized, coverage_l1, coverage_l2, coverage_empirical_bayes) |>
  mutate_at(vars(theta, sigma), remove_dups) |>
  mutate_at(vars(starts_with("coverage")), scales::percent_format(accuracy = 0.1)) |> 
  knitr::kable(format = "latex")

#write_rds(summarized_results_combined, "/gpfs/home/susmah01/PenalizedCausalInference/results/group_effects_results_combined.rds")
