##################################################################################
rm(list = ls())  # resets R to fresh

set.seed(123456)
library(glue)
library(readxl)
require(tidyverse)
require("sfsmisc")
require("deSolve")

##################################################################################
setwd("~/...")
dio <- "."
dout <- file.path(dio, "GitHubTest") #
source('vaccine_aloc_utlis.R') 
##################################################################################
# Set up the model parameters
##################################################################################

param <- list()
# Read demographic parameters from files
fp <- read_excel("GitHubTest/input/target_population_1209_python.xlsx", sheet = "population")
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
fp <- read_excel("GitHubTest/input/target_population_1209_python.xlsx", sheet = "acceptance")
# Potetential vaccine acceptances for general population 
param$vc <- rep(1, nage)
# potential vaccine acceptances for population in the first tier
param$vc.tier <- 1

# Read risk parameters from files
fp <- read_excel("GitHubTest/input/disease_burden-1117.xlsx", sheet = "risk rates")
param$deathr <- unlist(fp[, "Deaths in infections"])
param$icur <- unlist(fp[, "ICU in infections"])
param$symp <- unlist(fp[, "Symptoms in infections"])
param$hosp <- unlist(fp[, "Hospitalized in infections"])

# Recover rate in days^{-1}  
gamma  = 1/5.5  
# A hypothetical pandemic strain of disease
# R0 = 2.2 * alpha = 1.5, alpha = 0.685
R0    = 1.5          
# Vaccine efficacy for the 17 age groups
eff = c(rep(0.80*0.75,3),rep(0.80,9),rep(0.80*0.75,5)) 
# Read contact data from files
fp <- read_excel("GitHubTest/input/contact matrix/cm_china_17gr.xlsx", col_names  = TRUE)
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
###############################################################################
## The scenario without vaccine
###############################################################################
# Initial conditions
I_0 = as.matrix(rep(1, nage))   
S_0 = as.matrix(param$N - I_0)
R_0 = as.matrix(rep(0, nage))
V_0 = as.matrix(rep(0, nage))
U_0 = as.matrix(rep(0, nage))
v = as.matrix(rep(0, nage))
vparameters=c(gamma=gamma,beta=beta, eff = eff,v = v, w = param$w, C=C)
inits=c(S=S_0,V=V_0,U= U_0, I=I_0,R=R_0)
vt = seq(0,Timelim,1)
svirmodel = as.data.frame(lsoda(inits, vt, SIRfunc, vparameters))


###############################################################################
## The uniform allocation
###############################################################################
out <- allocation_model(param,"dynUnif")
svir.unif.infec <- out$svir
V.unif.count <- out$vaccine

# output the results for the two scenarios: baseline and the uniform
save(svirmodel, svir.unif.infec,V.unif.count, file = "GitHubTest/output/base_unif.RData")
