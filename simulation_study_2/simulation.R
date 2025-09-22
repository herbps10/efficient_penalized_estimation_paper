library(tidyverse)
library(CausalShrink)
library(glmnet)
library(tmle)

root <- rprojroot::is_git_root                                                                         
basepath <- root$find_file("simulation_study_2")

source(glue::glue("{basepath}/simulate.R"))
source(glue::glue("{basepath}/wrapper.R"))
source(glue::glue("{basepath}/env.R"))

cache_path <- Sys.getenv("SIMULATION_CACHE_PATH")
task_id <- Sys.getenv("SLURM_ARRAY_TASK_ID")

if(cache_path == "") stop("Please set SIMULATION_CACHE_PATH environment variable.")
if(task_id == "") stop("Task id not set. Please set SLURM_ARRAY_TASK_ID, or run simulations through a Slurm job array, which will set this environment variable for you.")

index <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))

filename <- glue::glue("{cache_path}/{index}.rds")

if(!file.exists(filename)) {
  SL.library <- c("SL.glm", "SL.glm.interaction", "SL.glmnet")

  simulations <- expand_grid(
    index = index,
    G = c(25),
    theta = c(0, 0.3, 1),
    beta = 1,
    N = c(4e3, 6e3, 8e3, 1e4),
    sigma = c(0.5, 1, 2, 4)
  ) %>%
    mutate(
      seed = index * 1e5 + 1:n(),
      data = pmap(list(seed, N, G, beta, theta, sigma), simulate),
      results = pmap(list(data, G), wrapper, SL.library = SL.library)
    ) %>%
    select(-data) %>%
    unnest(results)

  write_rds(simulations, filename)
}
