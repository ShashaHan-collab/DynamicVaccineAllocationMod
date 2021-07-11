##################################################################################
rm(list = ls())  # resets R to fresh

set.seed(123456)
library(glue)
library(readxl)
require(tidyverse)
require("sfsmisc")
require("deSolve")

##################################################################################
setwd("~/DynamicVaccineAllocationMod-main")
dio <- "."
dout <- file.path(dio, "GitHubTest") #
source('vaccine_aloc_utlis.R') 
##################################################################################
# Set up the model parameters
##################################################################################

param <- list()
# Read demographic parameters from files
fp <- read_excel("GitHubTest/input/target_population_1216_python.xlsx", sheet = "population")
# Number of people in each age group
param$N <- unlist(fp[, "Total"])  
names(param$N) <- NULL
# Number of age groups
nage <- length(param$N)
param$nage = nage
# Number of tiers
param$niters = 2
# Number of people in each age group within the first tier 
param$T12 <- unlist(fp[, "Tier12"])
# Number of people in each age group within the second tier  
param$T3 <- unlist(fp[, "Tier3"])

# Relative susceptability
param$rel_sus <- unlist(fp[, "heter susceptability"]) 
param$rel_sus <- param$rel_sus/max(param$rel_sus)

# Read acceptance parameters from files
fp <- read_excel("GitHubTest/input/target_population_1216_python.xlsx", sheet = "acceptance")
## Potential vaccine acceptances for population in Tier 3
param$vc <- rep(1, nage)
# potential vaccine acceptances for population in the first tier
param$vc.tier <- 1

# Read risk parameters from files
fp <- read_excel("GitHubTest/input/disease_burden_1117_python.xlsx", sheet = "risk rates")
param$deathr <- unlist(fp[, "Deaths in infections"])
param$icur <- unlist(fp[, "ICU in infections"])
param$symp <- unlist(fp[, "Symptoms in infections"])
param$hosp <- unlist(fp[, "Hospitalized in infections"])

# Recover rate in days^{-1}  
gamma  = 1/5.5  
# A hypothetical pandemic strain of disease
R0    = 1.5          
# Vaccine efficacy for the 17 age groups
eff = c(rep(0.80*0.75,3),rep(0.80,9),rep(0.80*0.75,5)) 
# Read contact data from files
fp <- read_excel("GitHubTest/input/cm_china_17gr_baseline_python.xlsx", col_names  = TRUE)
C <- fp[,2:dim(fp)[2]]
names(C) <- NULL
C <- as.matrix(C)
C <- param$rel_sus*C 
rownames(C)
# Calaculate the probality of transimision per contact per time, beta
beta = cal_beta(C,param$N,R0,gamma)
# Study period
Timelim = 400
param$Timelim <- Timelim
param$beta <- beta
param$gamma <- gamma
param$eff <- eff
# Daily supply courses for two-dose vaccines
param$cpt =  4000000/2

# Exiting rate from vacciation state
param$w = 1/35
# Time to start vaccinations
param$epoch = 1  

# Optimal allocation minimizing daily symptomatic cases
out <- allocation_model(param,"minSymp")
svir.opt.symp <- out$svir
V.opts.count <- out$vaccine

# Output the results for optimization in the second step
output <- svir.opt.symp %>% select(time, paste0("I",1:nage,sep = "")) ## 
names(output) <- c("time", paste0("Group",1:nage,sep = ""))
write.csv(output, file = "GitHubTest/intermediate/test input opt approxmation.csv", row.names = FALSE)


# Optimal allocation minimizing daily infections
# out <- allocation_model(param,"minInfec")
# svir.opt.infec <- out$svir
# V.opt.count <- out$vaccine

# # Optimal allocation minimizing hospitalizations
# out <- allocation_model(param,"minHosp")
# svir.opt.hosp <- out$svir
# V.opth.count <- out$vaccine
# 
# # Optimal allocation minimizing ICUs
# out <- allocation_model(param,"minICU")
# svir.opt.icu <- out$svir
# V.opti.count <- out$vaccine
# 
# # Optimal allocation minimizing deaths
# out <- allocation_model(param,"minDeath")
# svir.opt.death <- out$svir
# V.optd.count <- out$vaccine






