# DynamicVaccineAllocationMod
Dynamic Vaccine Allocation Model

This model allocates a limited daily supply of vaccines, where the allocations are coupled with dynamics of disease transmissions. It was used in a recent paper "Time-varying optimization of COVID-19 vaccine prioritization in the context of limited vaccination capacity".(Put Citation Here).

For an example run of the optimal allocation over the full period, please see "opt_approx_alloc.py", which uses a dummy intermediate dataset "test input opt approxmation.csv". For an example run of the uniform allocation, please see "base_unif_alloc.R", which uses a dummy parameter dataset "test input parameter.csv". For an example run of the optimal allocation using myopic strategies, please see "opt_myopic_alloc.R", which generates intermediate dataset "test input opt approxmation.csv".

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
* base_unif_alloc.R: R script to generate results for the scenario without vaccines and the uniform allocation. The script calls vaccine_aloc_utlis.R to perform the allocation.
* opt_myopic_alloc.R: R script for the first step optimization. The script calls myopic allocations in vaccine_aloc_utlis.R to optimize allocatoin daily. It generates intermediate data for the second step of optimization.
* opt_approx_alloc.py: Python script for the second step optimization. The script optimize allocation over the full period useing the intermedate data generated from the first step. The script generates the final optimal solutions with respect to the given targeted risks. The demo uses the death risk.  


