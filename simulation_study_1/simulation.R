library(tidyverse)
library(CausalShrink)
library(glmnet)

root <- rprojroot::is_git_root                                                                         
basepath <- root$find_file("simulation_study_1")

source(glue::glue("{basepath}/../R/estimator_pprof.R"))
source(glue::glue("{basepath}/simulate.R"))
source(glue::glue("{basepath}/wrapper.R"))
source(glue::glue("{basepath}/env.R"))

N_simulations <- 1
index = as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
simulations <- expand_grid(
  index = index,
  N = c(50, 100, 250, 500, 1000),
  D = c(100),
  sigma = c(0.5, 1, 3),
  theta = c(0.3),
  dgp = "sparse"
) 

for(N in unique(simulations$N)) {
  dir <- glue::glue("{cache_path}/{N}")
  if(!dir.exists(dir)) {
    dir.create(dir)
  }
}
 
filename <- glue::glue("{cache_path}/{index}.rds")
y <- "Y"
learners <- c("cv_glmnet")
simulations <- simulations %>%
  mutate(
    seed = index * 1e5 + 1:n()
  ) %>%
  mutate(
    data = pmap(list(seed, N, D, sigma, dgp, theta), simulate),
    x = map(data, \(data) setdiff(colnames(data$data), "Y")),
    fit = pmap(list(data, x, seed, N, D, sigma, dgp, theta), wrapper, y = y, learners = learners),
    ols = pmap(list(data, x), \(data, x) estimator_ols(data$data, x = x, y = y)),
    glmnet_l1 = pmap(list(data, x), \(data, x) estimator_glmnet(data$data, x = x, y = y, alpha = 1)),
    glmnet_l2 = pmap(list(data, x), \(data, x) estimator_glmnet(data$data, x = x, y = y, alpha = 0))
  )

write_rds(simulations, filename)
