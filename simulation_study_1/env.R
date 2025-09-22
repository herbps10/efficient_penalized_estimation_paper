root <- rprojroot::is_git_root                                                                         
basepath <- root$find_file("simulation_study_1")  

Sys.setenv(SIMULATION_CACHE_PATH = system(glue::glue("source {basepath}/env.sh && echo $SIMULATION_CACHE_PATH"), intern = TRUE))
Sys.setenv(SIMULATION_RESULTS_PATH = system(glue::glue("source {basepath}/env.sh && echo $SIMULATION_RESULTS_PATH"), intern = TRUE))
