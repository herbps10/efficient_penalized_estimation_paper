library(tidyverse)

root <- rprojroot::is_git_root                                                                         
basepath <- root$find_file("simulation_study_3")  

source(glue::glue("{basepath}/env.R"))

cache_path <- Sys.getenv("SIMULATION_CACHE_PATH")
results_path <- Sys.getenv("SIMULATION_RESULTS_PATH")

if(cache_path == "") stop("Please set SIMULATION_CACHE_PATH environment variable.")
if(results_path == "") stop("Please set SIMULATION_RESULTS_PATH environment variable.")

files <- Sys.glob(glue::glue("{cache_path}/*/*.rds"))

if(length(files) == 0) {
  cat("Warning: no cached simulation results found.\n\n")
} else {
  results <- map_df(files, read_rds) %>% dplyr::bind_rows()
  results_path <- glue::glue("{results_path}/simulation_results.rds")
  write_rds(results, file = results_path, compress = "gz")
  cat(glue::glue("Simulation results saved to {results_path}\n\n"))
}
