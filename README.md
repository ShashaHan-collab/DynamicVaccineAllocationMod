# DynamicVaccineAllocationMod
Dynamic Vaccine Allocation Model

This model allocates a limited daily supply of vaccines, where the allocations are coupled with dynamics of disease transmissions. It was used in a recent XX paper (Put Citation Here).

For an example run of the optimal allocation over the full period, please see "run_ approx_opt.py", which uses a dummy intermediate dataset "test input opt approxmation.csv". For an example run of the uniform allocation, please see "run_unif.R", which uses a dummy parameter dataset "test input parameter.csv". For an example run of the optimal allocation using myopic strategies, please see "run_ myopic_opt.py", which generates intermediate dataset "test input opt approxmation.csv".

# Overall model


# Prerequesites
## Prerequesite softwares 
* Item Gurobi version 9.1.0.
* Item R version 4.0.3
* Item Python 3.9.0
## Prerequisite third-party R packages
* Item gurobi
* Item readxl
* Item tidyverse
* Item sfsmisc
* Item deSolve
