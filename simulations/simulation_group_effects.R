library(tidyverse)
library(CausalShrink)
library(glmnet)
library(tmle)

source("R/simulate_group_effects.R")
source("R/estimator_group_effects.R")

SL.library <- c("SL.glm", "SL.glm.interaction", "SL.glmnet")

index <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))

filename <- glue::glue("/gpfs/scratch/susmah01/PenalizedCausalInference/group_effects_cache/{index}.rds")

if(!file.exists(filename)) {
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
      results = pmap(list(data, G), estimator, SL.library = SL.library)
    ) %>%
    select(-data) %>%
    unnest(results)

  write_rds(simulations, filename)
}
