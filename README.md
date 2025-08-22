# Asymptotically Efficient Data-Adaptive Penalized Shrinkage Estimation with Application to Causal Inference

_Herbert P. Susmann, Yiting Li, Mara A. McAdams-DeMarco, and Iván Díaz._

This repository includes replication materials for the simulation studies presented in the paper.

The code depends on the package [`CausalShrink`](https://github.com/herbps10/CausalShrink). You can install `CausalShrink` using the `remotes` package:
```
remotes::install_github("herbps10/CausalShrink")
```

## Simulation Studies

The paper includes three simulation studies. The code for each simulation study is structured similarly, and is designed to be run on a parallel computing cluster using SLURM. Each simulation study is in its own folder (`simulation_study_1`, `simulation_study_2`, `simulation_study_3`) and has four files:

- `simulate.R`: contains a function for generating data from the simulation data-generating process.
- `simulation.R`: sets up and runs the simulation studies and saves results to a cache directory.
- `collect.R`: gathers the cached results once the simulation batch job completes and saves the final results to a file.
- `analyze.R`: loads the final results, summarizes them, and produces results tables for the paper.

The simulations are designed to run via a SLURM batch job. If you would like to run a single iteration in an interactive `R` session, you may run
```
Sys.setenv(SLURM_ARRAY_TASK_ID = 1)
source("simulation_study_1/simulation.R")
```

## Figures

Several illustrative figures for the paper are generated using the file `figures.R`. 

## Shared scripts
Additional scripts shared between the simulation studies are in the `R` folder.
