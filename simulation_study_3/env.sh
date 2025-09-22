#/bin/bash

# === Simulation Paths ===

# Path to the main R script that runs the simulation
export SIMULATION_STUDY_SCRIPT_PATH="/gpfs/home/susmah01/efficient_penalized_estimation_paper/simulation_study_3/simulation.R"

# Directory to cache intermediate results (e.g., per-task output)
export SIMULATION_CACHE_PATH="/gpfs/scratch/susmah01/efficient_penalized_estimation_paper/cache_3"

# Directory to store SLURM log files (stdout and stderr for each job)
export SIMULATION_LOG_PATH="/gpfs/scratch/susmah01/efficient_penalized_estimation_paper/log"

# Directory for final processed results (e.g., combined output)
export SIMULATION_RESULTS_PATH="/gpfs/home/susmah01/efficient_penalized_estimation_paper/simulation_study_3/results"

# === SLURM job configuration ===

# Name of the SLURM job (appears in job monitoring commands)
export JOB_NAME="sim3"

# SLURM partition to submit the job to (e.g., 'cpu', 'gpu')
export PARTITION=cpu_short

# Email address for job notifications (optional)
export MAIL_USER=herbert.susmann@nyulangone.org

# Number of CPUs to allocate per task
export CPUS_PER_TASK=1

# Memory per CPU (e.g., 2GB, 4000M)
export MEM_PER_CPU=5GB

# Time limit for each array task (format: HH:MM:SS)
export TIME=12:00:00

# SLURM array indices (e.g., 1-100 to run 100 parallel tasks)
export ARRAY=1-250
