library(tidyverse)

files <- Sys.glob("/gpfs/scratch/susmah01/PenalizedCausalInference/covariance_cache/*.rds")

results <- map_df(files, read_rds) %>% dplyr::bind_rows()

write_rds(results, file = "/gpfs/home/susmah01/PenalizedCausalInference/results/covariance_results.rds")
