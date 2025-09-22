library(tidyverse)

root <- rprojroot::is_git_root                                                                         
basepath <- root$find_file("simulation_study_3")  

source(glue::glue("{basepath}/env.R"))
source(glue::glue("{basepath}/../R/helpers.R"))

results_path <- Sys.getenv("SIMULATION_RESULTS_PATH")
if(results_path == "") stop("Please set SIMULATION_RESULTS_PATH environment variable.")

simulations <- read_rds(glue::glue("{results_path}/simulation_results.rds"))

summarized_results <- simulations |> 
  group_by(m, N, method) |>
  mutate(
    error = smr - psi, 
    covered = lower < smr & upper > smr,
  ) |> 
  summarize(n = n(), mse = mean(error^2), coverage = mean(covered), me = mean(error)) |>
  arrange(m, N)

make_breaks <- \(x, n) {
  b <- quantile(x, na.rm = TRUE, seq(0, 1, length.out = n))
  b[1] <- 0
  b[length(b)] <- Inf
  b
}

trt_size_breaks <- simulations |>
  group_by(N) |>
  summarize(breaks = make_breaks(trt_size, 4)) |>
  nest(.key = "breaks")

summarized_results_by_size <- simulations |>
  left_join(trt_size_breaks) |>
  mutate(trt_size_group = map2_chr(trt_size, breaks, \(trt_size, breaks) cut(trt_size, breaks = breaks$breaks))) |>
  group_by(m, N, method, trt_size_group) |>
  mutate(
    error = smr - psi, 
    covered = lower < smr & upper > smr
  ) |> 
  summarize(n = n(), mse = mean(error^2), coverage = mean(covered), me = mean(error)) |>
  arrange(m, N)


#
# Overall results table
#
results_table <- summarized_results |> 
  ungroup() |>
  filter(method != "l0") |> 
  select(-n) |>
  pivot_wider(names_from = "method", values_from = c("mse", "me", "coverage")) |>
  arrange(m, N) |> 
  ungroup() |>
  select(
    m, N, 
    mse_unpenalized, mse_l1, mse_l2, mse_empirical_bayes, mse_pprof_fe, mse_pprof_re, 
    me_unpenalized, me_l1, me_l2, me_empirical_bayes, me_pprof_fe, me_pprof_re, 
    coverage_unpenalized, coverage_l1, coverage_l2, coverage_empirical_bayes, coverage_pprof_fe, coverage_pprof_re
  )

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


#
# Results table by group size
#
results_table_by_size <- summarized_results_by_size |> 
  ungroup() |>
  filter(method != "l0") |> 
  select(-n) |>
  pivot_wider(names_from = "method", values_from = c("mse", "me", "coverage")) |>
  arrange(trt_size_group, m, N) |> 
  ungroup() |>
  select(
    trt_size_group, N, 
    mse_unpenalized, mse_l1, mse_l2, mse_empirical_bayes, mse_pprof_fe, mse_pprof_re, 
    me_unpenalized, me_l1, me_l2, me_empirical_bayes, me_pprof_fe, me_pprof_re, 
    coverage_unpenalized, coverage_l1, coverage_l2, coverage_empirical_bayes, coverage_pprof_fe, coverage_pprof_re 
  )

results_table_by_size_latex <- results_table_by_size |>
  mutate_at(vars(trt_size_group), remove_dups) |>
  mutate_at(vars(starts_with("mse"), starts_with("me")), scales::number_format(accuracy = 0.1, scale = 100)) |>
  mutate_at(vars(starts_with("coverage")), scales::percent_format(accuracy = 0.1)) |>
  knitr::kable(format = "latex")

results_table_by_size_latex_mse <- results_table_by_size |>
  select(trt_size_group, N, starts_with("mse")) |>
  mutate_at(vars(trt_size_group), remove_dups) |>
  mutate_at(vars(starts_with("mse")), scales::number_format(accuracy = 0.1, scale = 100)) |>
  knitr::kable(format = "latex")

results_table_by_size_latex_me <- results_table_by_size |>
  select(trt_size_group, N, starts_with("me")) |>
  mutate_at(vars(trt_size_group), remove_dups) |>
  mutate_at(vars(starts_with("me")), scales::number_format(accuracy = 0.1, scale = 100)) |>
  knitr::kable(format = "latex")

results_table_by_size_latex_coverage <- results_table_by_size |>
  select(trt_size_group, N, starts_with("coverage")) |>
  mutate_at(vars(trt_size_group), remove_dups) |>
  mutate_at(vars(starts_with("coverage")), scales::percent_format(accuracy = 0.1)) |>
  knitr::kable(format = "latex")
