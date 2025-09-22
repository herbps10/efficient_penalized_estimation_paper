library(tidyverse)
library(CausalShrink)
library(TargetedRisk)
library(broom)
library(mlr3extralearners)

root <- rprojroot::is_git_root                                                                         
basepath <- root$find_file("simulation_study_3")

source(glue::glue("{basepath}/../R/estimator_pprof.R"))
source(glue::glue("{basepath}/simulate.R"))
source(glue::glue("{basepath}/wrapper.R"))
source(glue::glue("{basepath}/env.R"))

index <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))

cache_path <- Sys.getenv("SIMULATION_CACHE_PATH")
task_id <- Sys.getenv("SLURM_ARRAY_TASK_ID")

if(cache_path == "") stop("Please set SIMULATION_CACHE_PATH environment variable.")
if(task_id == "") stop("Task id not set. Please set SLURM_ARRAY_TASK_ID, or run simulations through a Slurm job array, which will set this environment variable for you.")

P <- 5
simulations <- expand_grid(
  index = index,
  m = 50,
  N = c(1.5e3, 3e3, 4.5e3, 6e3)
) %>%
  mutate(
    seed = index * 1e5 + 1:n(),
    path = glue::glue("{cache_path}/{N}/{index}_{m}_{N}.rds"),
    data = pmap(list(seed, N, m), simulate, P = P, outcome_type = "binomial"),
    results = pmap(list(data, m, seed, index, path), estimator, P = P)
  )
