import numpy as np
import pandas as pd
from functools import reduce

import gurobipy as gp
from gurobipy import GRB

# Contact matrix
contact_mat = pd.read_excel('.../GitHubTest/input/cm_china_17gr_python.xlsx', index_col=0)
# Population segmentation
pop_data = pd.read_excel(open('.../GitHubTest/input/target_population_1209_python.xlsx', 'rb'),sheet_name='population', index_col=0)
pop = pop_data["Total"]
pop_tier = pop_data[{"Tier12","Tier3"}]
# Heterogenous susceptability
ra = pop_data["heter susceptability"]
ra = ra/max(ra)
# Vaccine accpetance
va = pd.read_excel(open('.../GitHubTest/input/target_population_1216_python.xlsx', 'rb'),sheet_name='acceptance12', index_col=0)
# Risk of disease burdens
risk_data = pd.read_excel(open('.../GitHubTest/input/disease_burden-1117_python.xlsx', 'rb'),sheet_name='risk rates', index_col=0)
# Targeted risk in optimization
risk = risk_data["Deaths in infections"]
# Dyanmics from the myopic optimization
targetI = pd.read_csv('.../GitHubTest/intermediate/test input opt approxmation.csv', index_col=0)

groups = list(pop_data.index)

# Period
T = 400 
days = range(0,T+1)
tiers = ["Tier12", "Tier3"]

ve = {"Group1":0.80*0.75, "Group2":0.80*0.75, "Group3":0.80*0.75, "Group4":0.80, "Group5":0.80, "Group6":0.80, "Group7":0.80, "Group8":0.80, "Group9":0.80, "Group10":0.90,
      "Group11":0.80, "Group12":0.80, "Group13":0.80*0.75, "Group14":0.80*0.75, "Group15":0.80*0.75, "Group16":0.80*0.75, "Group17":0.80*0.75}


beta = 0.0223921  # calcuated from running "run_unif.R".
gamma = 1/5.5
w = 1/35
max_capacity = 4000000/2 # daily supplies
epsilon = 0.4  

for groupJ in groups:
    for group in groups:
        contact_mat[groupJ][group] = contact_mat[groupJ][group] * ra[group]


vaCpt = gp.Model('Vaccination Capacity I')

stateS = vaCpt.addVars(days, groups, lb=0, name="S") # disease state S
stateI = vaCpt.addVars(days, groups, lb=0, name="I") # disease state I
stateU = vaCpt.addVars(days, groups, lb=0, name="U") # disease state U
stateV = vaCpt.addVars(days, groups, lb=0, name="V") # disease state V
alloc_tier = vaCpt.addVars(days, groups, tiers, lb=0, ub=max_capacity, name="Allocation") # quantity allocated based on tiers
alloc_cum = vaCpt.addVars(days, groups, lb=0, name="Allocation")
capacity = vaCpt.addVars(days, lb=0, ub=max_capacity, name="Capacity") # daily supplies
cpt = vaCpt.addVar(0.0, max_capacity, name="Cpt") # quantity manufactored
threshold = vaCpt.addVars(days, vtype=gp.GRB.BINARY, name="Threshold") # 



#1. Initial ODE
ProgressI0 = vaCpt.addConstrs((stateI[0, group] == 1 for group in groups), name="Initial_I0")
ProgressS0 = vaCpt.addConstrs((stateS[0, group] == pop[group] - stateI[0, group] for group in groups), name="Initial_S0")
ProgressU0 = vaCpt.addConstrs((stateU[0, group] == 0 for group in groups), name="Initial_U0")
ProgressV0 = vaCpt.addConstrs((stateV[0, group] == 0 for group in groups), name="Initial_V0")
Va_cumsum = vaCpt.addConstrs((alloc_cum[0, group] == 0 for group in groups), name="Initial_va_Tier12")


    
#2. ODE Progress
ProgressS = vaCpt.addConstrs((stateS[days[days.index(day) +1], group] >= stateS[day, group] - gp.quicksum(alloc_tier[day,group,tier] for tier in tiers) -
                             beta* stateS[day, group] * (gp.quicksum(contact_mat[groupJ][group] * targetI[groupJ][day]/pop[groupJ]
                             for groupJ in groups)) for group in groups for day in days if day != T), name="ProgressS")
ProgressV = vaCpt.addConstrs((stateV[days[days.index(day) +1], group] >= (stateV[day, group] + gp.quicksum(alloc_tier[day,group,tier] for tier in tiers)) * (1-w) -
                             stateV[day, group] * beta * (gp.quicksum(contact_mat[groupJ][group] * targetI[groupJ][day]/pop[groupJ]
                             for groupJ in groups)) for group in groups for day in days if day != T), name="ProgressV")
ProgressU = vaCpt.addConstrs((stateU[days[days.index(day) +1], group] >= stateU[day, group] + stateV[day, group]  * w * (1-ve[group]) - stateU[day, group]*
                             beta * (gp.quicksum(contact_mat[groupJ][group] * targetI[groupJ][day]/pop[groupJ]
                             for groupJ in groups)) for group in groups for day in days if day != T), name="ProgressU")
ProgressI = vaCpt.addConstrs((stateI[days[days.index(day) +1], group] >= stateI[day,group] *( 1-gamma) +
                             (stateS[day, group] + stateU[day, group] + stateV[day,group] ) *
                             beta * (gp.quicksum(contact_mat[groupJ][group] * targetI[groupJ][day]/pop[groupJ]
                             for groupJ in groups)) for group in groups for day in days if day != T), name="ProgressI")


#3. Approximation 
TargetDeath = vaCpt.addConstrs((gp.quicksum(contact_mat[groupJ][group] * stateI[day,groupJ]/pop[groupJ]
                             for groupJ in groups) - gp.quicksum(contact_mat[groupJ][group] * targetI[groupJ][day]/pop[groupJ]
                             for groupJ in groups) <= epsilon for group in groups for day in days),  name="TargetDeath")
TargetDeath2 = vaCpt.addConstrs((-gp.quicksum(contact_mat[groupJ][group] * stateI[day,groupJ]/pop[groupJ]
                             for groupJ in groups) + gp.quicksum(contact_mat[groupJ][group] * targetI[groupJ][day]/pop[groupJ]
                             for groupJ in groups) <= epsilon for group in groups for day in days),  name="TargetDeath2")
#4. Daily supply constraint
VaCap = vaCpt.addConstrs((gp.quicksum(alloc_tier[day,group,tier] for group in groups for tier in tiers)
                    <= max_capacity for day in days), name = "Capacity")

#5. Allocation Constraint
VaAlloc = vaCpt.addConstrs((gp.quicksum(alloc_tier[day,group,tier] for tier in tiers) <= stateS[day, group]
                            for group in groups for day in days), name = "ConstAllocation")
VaAlloc2 = vaCpt.addConstrs((gp.quicksum(alloc_tier[day,group,tier] for day in days) <= pop_tier[tier][group] * va[tier][group]
                            for group in groups for tier in tiers), name = "ConstAllocation2")
VaAlloc3 = vaCpt.addConstrs((alloc_cum[days[days.index(day) +1], group] == alloc_cum[day,group] + alloc_tier[day,group,"Tier12"]
                            for group in groups for day in days if day != T), name = "ConstAllocation3")
VaAlloc4 = vaCpt.addConstrs((alloc_tier[day,group,"Tier12"]  >= max_capacity * ((pop_tier["Tier12"][group] * va["Tier12"][group]))/sum(pop_tier["Tier12"][group] *
                            va["Tier12"][group] for group in groups) - threshold[day] *  max_capacity
                             for group in groups for day in days ), name = "ConstAllocation4")

#6. Tier Constraint
TierAlloc = vaCpt.addConstrs((gp.quicksum(alloc_tier[day,group,"Tier3"] for group in groups) <= threshold[day] *  (max_capacity + gp.quicksum(pop_tier["Tier12"][group] * va["Tier12"][group]
                            for group in groups)) for day in days), name = "ConstTier")
TierAlloc3 = vaCpt.addConstrs((gp.quicksum(alloc_tier[day,group,"Tier3"] for group in groups) <= (1 - threshold[day]) * (gp.quicksum(pop_tier["Tier12"][group] * va["Tier12"][group]
                            for group in groups)) + max_capacity +
                              gp.quicksum(alloc_cum[day,group] for group in groups) - gp.quicksum(pop_tier["Tier12"][group] * va["Tier12"][group] for group in groups) for day in days), name = "ConstTier3")

#0. Objective Function
obj = gp.quicksum((stateI[days.index(day) +1,group] - (1-gamma)*stateI[day,group])*risk[group] for group in groups for day in days if day !=T)

vaCpt.setObjective(obj, GRB.MINIMIZE)

#vaCpt.Params.BarHomogeneous=1
vaCpt.optimize()



# Output optimal solutions
rows = days
columns = groups.copy()
va_alloc12 = pd.DataFrame(columns=columns, index=rows, data=0.0)
va_alloc3 = pd.DataFrame(columns=columns, index=rows, data=0.0)
va_alloc = pd.DataFrame(columns=columns, index=rows, data=0.0)

for day, group, tier in alloc_tier.keys():
    if (abs(alloc_tier[day,group,tier].x) > 1e-6):
        if (tier in ['Tier12']):
            va_alloc12.loc[day, group] = np.round(alloc_tier[day,group,tier].x , 1)
        elif (tier in ['Tier3']):
            va_alloc3.loc[day, group] = np.round(alloc_tier[day,group,tier].x , 1)
for day in days:
    for group in groups:
        va_alloc.loc[day, group] = va_alloc12.loc[day, group] + va_alloc3.loc[day, group]
        
outputI = pd.DataFrame(columns=columns, index=rows, data=0.0)
for day, group in stateI.keys():
    if (abs(stateI[day, group].x) > 1e-6):
        outputI.loc[day, group] = np.round(stateI[day, group].x, 1)
outputI = outputI.rename(columns={"Group1": "I1", "Group2": "I2", "Group3": "I3", "Group4": "I4", "Group5": "I5", "Group6": "I6", "Group7": "I7", "Group8": "I8", "Group9": "I9", "Group10": "I10",
      "Group11": "I11", "Group12": "I12", "Group13": "I13", "Group14": "I14", "Group15": "I15", "Group16": "I16", "Group17": "I17"})

outputS = pd.DataFrame(columns=columns, index=rows, data=0.0)
for day, group in stateS.keys():
    if (abs(stateS[day, group].x) > 1e-6):
        outputS.loc[day, group] = np.round(stateS[day, group].x, 1)
outputS = outputS.rename(columns={"Group1": "S1", "Group2": "S2", "Group3": "S3", "Group4": "S4", "Group5": "S5", "Group6": "S6", "Group7": "S7", "Group8": "S8", "Group9": "S9", "Group10": "S10",
      "Group11": "S11", "Group12": "S12", "Group13": "S13", "Group14": "S14", "Group15": "S15", "Group16": "S16", "Group17": "S17"})

outputV = pd.DataFrame(columns=columns, index=rows, data=0.0)
for day, group in stateV.keys():
    if (abs(stateV[day, group].x) > 1e-6):
        outputV.loc[day, group] = np.round(stateV[day, group].x, 1)
outputV = outputV.rename(columns={"Group1": "V1", "Group2": "V2", "Group3": "V3", "Group4": "V4", "Group5": "V5", "Group6": "V6", "Group7": "V7", "Group8": "V8", "Group9": "V9", "Group10": "V10",
      "Group11": "V11", "Group12": "V12", "Group13": "V13", "Group14": "V14", "Group15": "V15", "Group16": "V16", "Group17": "V17"})

outputU = pd.DataFrame(columns=columns, index=rows, data=0.0)
for day, group in stateU.keys():
    if (abs(stateV[day, group].x) > 1e-6):
        outputU.loc[day, group] = np.round(stateU[day, group].x, 1)
outputU = outputU.rename(columns={"Group1": "U1", "Group2": "U2", "Group3": "U3", "Group4": "U4", "Group5": "U5", "Group6": "U6", "Group7": "U7", "Group8": "U8", "Group9": "U9", "Group10": "U10",
      "Group11": "U11", "Group12": "U12", "Group13": "U13", "Group14": "U14", "Group15": "U15", "Group16": "U16", "Group17": "U17"})


svirmodel = [outputS, outputV, outputU, outputI]
svir_merged = pd.concat(svirmodel, join='outer', axis=1)

pd.DataFrame.to_csv(svir_merged, 'GitHubTest/output/svirmodel.csv', sep=',', na_rep='.', index=False)
pd.DataFrame.to_csv(va_alloc, 'GitHubTest/output/allocation.csv', sep=',', na_rep='.', index=False)
