#!/bin/bash
#SBATCH --job-name=SimulationStudy
##SBATCH --mail-user=user@test.com
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
##SBATCH --output=/log/path
#SBATCH --partition cpu_short
#SBATCH --array=1-100

module load r/4.3.2

source env.sh

Rscript $SIMULATION_STUDY_SCRIPT_PATH
