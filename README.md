# DynamicVaccineAllocationMod
Dynamic Vaccine Allocation Model

This model allocates a limited daily supply of vaccines, where the allocations are coupled with dynamics of disease transmissions. It was used in a recent XX paper (Put Citation Here).

For an example run of the optimal allocation over the full period, please see "run_approx_opt.py", which uses a dummy intermediate dataset "test input opt approxmation.csv". For an example run of the uniform allocation, please see "run_unif.R", which uses a dummy parameter dataset "test input parameter.csv". For an example run of the optimal allocation using myopic strategies, please see "run_myopic_opt.R", which generates intermediate dataset "test input opt approxmation.csv".

# Overall model


# Prerequesites
## Prerequesite softwares 
* Gurobi version 9.1.0.
* R version 4.0.3
* Python 3.9.0
## Prerequisite third-party R packages
* gurobi
* readxl
* tidyverse
* sfsmisc
* deSolve
* Matrix
* zoo

# Descriptions of the files
* run_unif.R: R script to generate results for the scenario without vaccines and the uniform allocation. The script calls vaccine_aloc_utlis.R to perform the allocation.
* run_myopic_opt.R: R script for the first step optimization. The script calls myopic allocations in vaccine_aloc_utlis.R to optimization allocatoin daily. It generates intermediate data for the second step of optimization.
* run_run_approx_opt.py: Python script for the second step optimization. The script uses the intermedate data from the first step to optimization the allocation over the full period. The script generates the optimal solutions with respect to the targeted risks, which are input as parameters.


