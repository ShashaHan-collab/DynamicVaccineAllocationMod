################################################################################
#### Plot Supplementary Fig. 7 ####
rm(list = ls())

library(tidyverse)
library(glue)
library(xlsx)
library(cowplot)
library(scales)

setwd("~/DynamicVaccineAllocationMod-main")
dout <- "Source code/output"

#### Panels a1 and b1 ##########################################################
# load data
opt_infec.gr.vc.range <- read.xlsx("Source data/data_Supplementary Fig. 7.xlsx", 
                                   sheetName = "Panels a1 and b1")

opt_infec.gr.vc.range <- opt_infec.gr.vc.range %>% 
  rename(vc1 = baseline.coverage.for.1st.priority.group, 
         vc1.max = maximum.coverage.for.1st.priority.group, 
         vc1.min = minimal.coverage.for.1st.priority.group, 
         vc2 = baseline.coverage.for.2nd.priority.group, 
         vc2.max = maximum.coverage.for.2nd.priority.group, 
         vc2.min = minimal.coverage.for.2nd.priority.group) %>% 
  mutate(var = factor(var, levels = c("Vaccine model", "Vaccine efficacious in preventing infection vs. disease", 
                                      "Relative infectiousness of asymptomatic individuals",  
                                      "Start of vaccination relative to epidemic onset", 
                                      "Additional efficacy in preventing disease", "Vaccine efficacy", 
                                      "Daily vaccination capacity", "Vaccine acceptance", "R")))


# Priority people aged 15-39 under minimizing infections #######################
opt_infec.gr.p1 <- ggplot(data = opt_infec.gr.vc.range) + 
  geom_bar(aes(var, vc1.max * 100), position = "identity", stat = "identity", fill = "#0B63A2") + 
  geom_bar(aes(var, vc1 * 100), position = "identity", stat = "identity", fill = "#CC79A7") + 
  geom_bar(aes(var, vc1.min * 100), position = "identity", stat = "identity", fill = "white") + 
  geom_hline(yintercept = opt_infec.gr.vc.range$vc1[1] * 100, color = "darkgray") + 
  labs(x = "", y = "Priority coverage (%)", 
       title = "Priority people aged 15-39\n under minimizing infections") + 
  scale_x_discrete(labels = c("Vaccine model", "Vaccine efficacious in\n preventing infection vs. disease", 
                              "Relative infectiousness of\n asymptomatic individuals",  
                              "Start of vaccination relative\n to epidemic onset", 
                              "Additional efficacy in\n preventing disease", "Vaccine efficacy", 
                              "Daily vaccination capacity", "Vaccine acceptance", "R")) + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10), 
                     labels = seq(0, 100, by = 10), position = "right") + 
  theme_classic() +
  theme(axis.line = element_blank()) +
  geom_segment(aes(x = 0.5, xend = 9.5, y = -Inf, yend = -Inf), color = "darkgray") + 
  geom_segment(x = Inf, xend = Inf, y = 0, yend = 100, color = "darkgray") + 
  coord_flip() + 
  theme(
    legend.position = c(0.95, 0.8),
    legend.title = element_blank(),
    legend.text = element_text(size = 11, color = "black"),
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 13, color = "black"),
    axis.ticks = element_line(color = "darkgray"), 
    axis.title = element_text(size = 13, color = "black", face = "bold")
  )
opt_infec.gr.p1


# Priority people aged 40-64 under minimizing infections #######################
opt_infec.gr.vc.range <- opt_infec.gr.vc.range %>% 
  mutate(var = as.character(var)) %>% 
  mutate(var = factor(var, levels = c("Vaccine model", "Vaccine efficacious in preventing infection vs. disease", 
                                      "Relative infectiousness of asymptomatic individuals",  
                                      "Start of vaccination relative to epidemic onset", 
                                      "Additional efficacy in preventing disease", "Vaccine efficacy", 
                                      "Daily vaccination capacity", "Vaccine acceptance", "R")))

opt_infec.gr.p2 <- ggplot(data = opt_infec.gr.vc.range) + 
  geom_bar(aes(var, vc2.max * 100), position = "identity", stat = "identity", fill = "#0B63A2") + 
  geom_bar(aes(var, vc2 * 100), position = "identity", stat = "identity", fill = "#CC79A7") + 
  geom_bar(aes(var, vc2.min * 100), position = "identity", stat = "identity", fill = "white") + 
  geom_hline(yintercept = opt_infec.gr.vc.range$vc2[1] * 100, color = "darkgray") + 
  labs(x = "", y = "Priority coverage (%)", 
       title = "Priority people aged 40-64\n under minimizing infections") + 
  scale_x_discrete(labels = c("Vaccine model", "Vaccine efficacious in\n preventing infection vs. disease", 
                              "Relative infectiousness of\n asymptomatic individuals",  
                              "Start of vaccination relative\n to epidemic onset", 
                              "Additional efficacy in\n preventing disease", "Vaccine efficacy", 
                              "Daily vaccination capacity", "Vaccine acceptance", "R")) + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10), 
                     labels = seq(0, 100, by = 10), position = "right") + 
  theme_classic() +
  theme(axis.line = element_blank()) +
  geom_segment(aes(x = 0.5, xend = 9.5, y = -Inf, yend = -Inf), color = "darkgray") + 
  geom_segment(x = Inf, xend = Inf, y = 0, yend = 100, color = "darkgray") + 
  coord_flip() + 
  theme(
    legend.position = c(0.95, 0.8),
    legend.title = element_blank(),
    legend.text = element_text(size = 11, color = "black"),
    plot.title = element_text(hjust = 0.5, margin = margin(0, 0, -0.5, 0, unit = "line")), 
    axis.text = element_text(size = 13, color = "black"),
    axis.ticks = element_line(color = "darkgray"), 
    axis.title = element_text(size = 13, color = "black", face = "bold")
  )
opt_infec.gr.p2


#### Panels a2 and b2 ##########################################################
# load data
opt_symp.gr.vc.range <- read.xlsx("Source data/data_Supplementary Fig. 7.xlsx", 
                                   sheetName = "Panels a2 and b2")

opt_symp.gr.vc.range <- opt_symp.gr.vc.range %>% 
  rename(vc1 = baseline.coverage.for.1st.priority.group, 
         vc1.max = maximum.coverage.for.1st.priority.group, 
         vc1.min = minimal.coverage.for.1st.priority.group, 
         vc2 = baseline.coverage.for.2nd.priority.group, 
         vc2.max = maximum.coverage.for.2nd.priority.group, 
         vc2.min = minimal.coverage.for.2nd.priority.group) %>% 
  mutate(var = factor(var, levels = c("Vaccine model", "Vaccine efficacious in preventing infection vs. disease", 
                                      "Relative infectiousness of asymptomatic individuals",  
                                      "Start of vaccination relative to epidemic onset", 
                                      "Additional efficacy in preventing disease", "Vaccine efficacy", 
                                      "Daily vaccination capacity", "Vaccine acceptance", "R")))


# Priority people aged 65+ under minimizing symptomatic cases ##################
opt_symp.gr.p1 <- ggplot(data = opt_symp.gr.vc.range) + 
  geom_bar(aes(var, vc1.max * 100), position = "identity", stat = "identity", fill = "#0B63A2") + 
  geom_bar(aes(var, vc1 * 100), position = "identity", stat = "identity", fill = "#CC79A7") + 
  geom_bar(aes(var, vc1.min * 100), position = "identity", stat = "identity", fill = "white") + 
  geom_hline(yintercept = opt_symp.gr.vc.range$vc1[1] * 100, color = "darkgray") + 
  labs(x = "", y = "Priority coverage (%)", 
       title = "Priority people aged 65+\n under minimizing symptoamtic cases") + 
  scale_x_discrete(labels = c("Vaccine model", "Vaccine efficacious in\n preventing infection vs. disease", 
                              "Relative infectiousness of\n asymptomatic individuals",  
                              "Start of vaccination relative\n to epidemic onset", 
                              "Additional efficacy in\n preventing disease", "Vaccine efficacy", 
                              "Daily vaccination capacity", "Vaccine acceptance", "R")) + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10), 
                     labels = seq(0, 100, by = 10), position = "right") + 
  theme_classic() +
  theme(axis.line = element_blank()) +
  geom_segment(aes(x = 0.5, xend = 9.5, y = -Inf, yend = -Inf), color = "darkgray") + 
  geom_segment(x = Inf, xend = Inf, y = 0, yend = 100, color = "darkgray") + 
  coord_flip() + 
  theme(
    legend.position = c(0.95, 0.8),
    legend.title = element_blank(),
    legend.text = element_text(size = 11, color = "black"),
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 13, color = "black"),
    axis.ticks = element_line(color = "darkgray"), 
    axis.title = element_text(size = 13, color = "black", face = "bold")
  )
opt_symp.gr.p1


# Priority people aged 40-64 under minimizing symptomatic cases ##################
opt_symp.gr.vc.range <- opt_symp.gr.vc.range %>% 
  mutate(var = as.character(var)) %>% 
  mutate(var = factor(var, levels = c("Vaccine model", "Vaccine efficacious in preventing infection vs. disease", 
                                      "Relative infectiousness of asymptomatic individuals",  
                                      "Start of vaccination relative to epidemic onset", 
                                      "Additional efficacy in preventing disease", "Vaccine efficacy", 
                                      "Daily vaccination capacity", "Vaccine acceptance", "R")))

opt_symp.gr.p2 <- ggplot(data = opt_symp.gr.vc.range) + 
  geom_bar(aes(var, vc2.max * 100), position = "identity", stat = "identity", fill = "#0B63A2") + 
  geom_bar(aes(var, vc2 * 100), position = "identity", stat = "identity", fill = "#CC79A7") + 
  geom_bar(aes(var, vc2.min * 100), position = "identity", stat = "identity", fill = "white") + 
  geom_hline(yintercept = opt_symp.gr.vc.range$vc2[1] * 100, color = "darkgray") + 
  labs(x = "", y = "Priority coverage (%)", 
       title = "Priority people aged 40-64\n under minimizing symptomatic cases") + 
  scale_x_discrete(labels = c("Vaccine model", "Vaccine efficacious in\n preventing infection vs. disease", 
                              "Relative infectiousness of\n asymptomatic individuals",  
                              "Start of vaccination relative\n to epidemic onset", 
                              "Additional efficacy in\n preventing disease", "Vaccine efficacy", 
                              "Daily vaccination capacity", "Vaccine acceptance", "R")) + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10), 
                     labels = seq(0, 100, by = 10), position = "right") + 
  theme_classic() +
  theme(axis.line = element_blank()) +
  geom_segment(aes(x = 0.5, xend = 9.5, y = -Inf, yend = -Inf), color = "darkgray") + 
  geom_segment(x = Inf, xend = Inf, y = 0, yend = 100, color = "darkgray") + 
  coord_flip() + 
  theme(
    legend.position = c(0.95, 0.8),
    legend.title = element_blank(),
    legend.text = element_text(size = 11, color = "black"),
    plot.title = element_text(hjust = 0.5, margin = margin(0, 0, -0.5, 0, unit = "line")), 
    axis.text = element_text(size = 13, color = "black"),
    axis.ticks = element_line(color = "darkgray"), 
    axis.title = element_text(size = 13, color = "black", face = "bold")
  )
opt_symp.gr.p2


#### Panels a3 and b3 ##########################################################
# load data
opt_hosp.gr.vc.range <- read.xlsx("Source data/data_Supplementary Fig. 7.xlsx", 
                                  sheetName = "Panels a3 and b3")

opt_hosp.gr.vc.range <- opt_hosp.gr.vc.range %>% 
  rename(vc1 = baseline.coverage.for.1st.priority.group, 
         vc1.max = maximum.coverage.for.1st.priority.group, 
         vc1.min = minimal.coverage.for.1st.priority.group, 
         vc2 = baseline.coverage.for.2nd.priority.group, 
         vc2.max = maximum.coverage.for.2nd.priority.group, 
         vc2.min = minimal.coverage.for.2nd.priority.group) %>% 
  mutate(var = factor(var, levels = c("Vaccine model", "Vaccine efficacious in preventing infection vs. disease", 
                                      "Relative infectiousness of asymptomatic individuals",  
                                      "Start of vaccination relative to epidemic onset", 
                                      "Additional efficacy in preventing disease", "Vaccine efficacy", 
                                      "Daily vaccination capacity", "Vaccine acceptance", "R")))


# Priority people aged 65+ under minimizing hospitalizations ###################
opt_hosp.gr.p1 <- ggplot(data = opt_hosp.gr.vc.range) + 
  geom_bar(aes(var, vc1.max * 100), position = "identity", stat = "identity", fill = "#0B63A2") + 
  geom_bar(aes(var, vc1 * 100), position = "identity", stat = "identity", fill = "#CC79A7") + 
  geom_bar(aes(var, vc1.min * 100), position = "identity", stat = "identity", fill = "white") + 
  geom_hline(yintercept = opt_hosp.gr.vc.range$vc1[1] * 100, color = "darkgray") + 
  labs(x = "", y = "Priority coverage (%)", 
       title = "Priority people aged 65+\n under minimizing hospitalizations") + 
  scale_x_discrete(labels = c("Vaccine model", "Vaccine efficacious in\n preventing infection vs. disease", 
                              "Relative infectiousness of\n asymptomatic individuals",  
                              "Start of vaccination relative\n to epidemic onset", 
                              "Additional efficacy in\n preventing disease", "Vaccine efficacy", 
                              "Daily vaccination capacity", "Vaccine acceptance", "R")) + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10), 
                     labels = seq(0, 100, by = 10), position = "right") + 
  theme_classic() +
  theme(axis.line = element_blank()) +
  geom_segment(aes(x = 0.5, xend = 9.5, y = -Inf, yend = -Inf), color = "darkgray") + 
  geom_segment(x = Inf, xend = Inf, y = 0, yend = 100, color = "darkgray") + 
  coord_flip() + 
  theme(
    legend.position = c(0.95, 0.8),
    legend.title = element_blank(),
    legend.text = element_text(size = 11, color = "black"),
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 13, color = "black"),
    axis.ticks = element_line(color = "darkgray"), 
    axis.title = element_text(size = 13, color = "black", face = "bold")
  )
opt_hosp.gr.p1


# Priority people aged 40-64 under minimizing hospitalizations #################
opt_hosp.gr.vc.range <- opt_hosp.gr.vc.range %>% 
  mutate(var = as.character(var)) %>% 
  mutate(var = factor(var, levels = c("Vaccine model", "Vaccine efficacious in preventing infection vs. disease", 
                                      "Relative infectiousness of asymptomatic individuals",  
                                      "Start of vaccination relative to epidemic onset", 
                                      "Additional efficacy in preventing disease", "Vaccine efficacy", 
                                      "Daily vaccination capacity", "Vaccine acceptance", "R")))

opt_hosp.gr.p2 <- ggplot(data = opt_hosp.gr.vc.range) + 
  geom_bar(aes(var, vc2.max * 100), position = "identity", stat = "identity", fill = "#0B63A2") + 
  geom_bar(aes(var, vc2 * 100), position = "identity", stat = "identity", fill = "#CC79A7") + 
  geom_bar(aes(var, vc2.min * 100), position = "identity", stat = "identity", fill = "white") + 
  geom_hline(yintercept = opt_hosp.gr.vc.range$vc2[1] * 100, color = "darkgray") + 
  labs(x = "", y = "Priority coverage (%)", 
       title = "Priority people aged 40-64\n under minimizing hospitalizations") + 
  scale_x_discrete(labels = c("Vaccine model", "Vaccine efficacious in\n preventing infection vs. disease", 
                              "Relative infectiousness of\n asymptomatic individuals",  
                              "Start of vaccination relative\n to epidemic onset", 
                              "Additional efficacy in\n preventing disease", "Vaccine efficacy", 
                              "Daily vaccination capacity", "Vaccine acceptance", "R")) + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10), 
                     labels = seq(0, 100, by = 10), position = "right") + 
  theme_classic() +
  theme(axis.line = element_blank()) +
  geom_segment(aes(x = 0.5, xend = 9.5, y = -Inf, yend = -Inf), color = "darkgray") + 
  geom_segment(x = Inf, xend = Inf, y = 0, yend = 100, color = "darkgray") + 
  coord_flip() + 
  theme(
    legend.position = c(0.95, 0.8),
    legend.title = element_blank(),
    legend.text = element_text(size = 11, color = "black"),
    plot.title = element_text(hjust = 0.5, margin = margin(0, 0, -0.5, 0, unit = "line")), 
    axis.text = element_text(size = 13, color = "black"),
    axis.ticks = element_line(color = "darkgray"), 
    axis.title = element_text(size = 13, color = "black", face = "bold")
  )
opt_hosp.gr.p2


#### Panels a4 and b4 ##########################################################
# load data
opt_icu.gr.vc.range <- read.xlsx("Source data/data_Supplementary Fig. 7.xlsx", 
                                  sheetName = "Panels a4 and b4")

opt_icu.gr.vc.range <- opt_icu.gr.vc.range %>% 
  rename(vc1 = baseline.coverage.for.1st.priority.group, 
         vc1.max = maximum.coverage.for.1st.priority.group, 
         vc1.min = minimal.coverage.for.1st.priority.group, 
         vc2 = baseline.coverage.for.2nd.priority.group, 
         vc2.max = maximum.coverage.for.2nd.priority.group, 
         vc2.min = minimal.coverage.for.2nd.priority.group) %>% 
  mutate(var = factor(var, levels = c("Vaccine model", "Vaccine efficacious in preventing infection vs. disease", 
                                      "Relative infectiousness of asymptomatic individuals",  
                                      "Start of vaccination relative to epidemic onset", 
                                      "Additional efficacy in preventing disease", "Vaccine efficacy", 
                                      "Daily vaccination capacity", "Vaccine acceptance", "R")))


# Priority people aged 65+ under minimizing ICU ################################
opt_icu.gr.p1 <- ggplot(data = opt_icu.gr.vc.range) + 
  geom_bar(aes(var, vc1.max * 100), position = "identity", stat = "identity", fill = "#0B63A2") + 
  geom_bar(aes(var, vc1 * 100), position = "identity", stat = "identity", fill = "#CC79A7") + 
  geom_bar(aes(var, vc1.min * 100), position = "identity", stat = "identity", fill = "white") + 
  geom_hline(yintercept = opt_icu.gr.vc.range$vc1[1] * 100, color = "darkgray") + 
  labs(x = "", y = "Priority coverage (%)", 
       title = "Priority people aged 65+\n under minimizing ICUs") + 
  scale_x_discrete(labels = c("Vaccine model", "Vaccine efficacious in\n preventing infection vs. disease", 
                              "Relative infectiousness of\n asymptomatic individuals",  
                              "Start of vaccination relative\n to epidemic onset", 
                              "Additional efficacy in\n preventing disease", "Vaccine efficacy", 
                              "Daily vaccination capacity", "Vaccine acceptance", "R")) + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10), 
                     labels = seq(0, 100, by = 10), position = "right") + 
  theme_classic() +
  theme(axis.line = element_blank()) +
  geom_segment(aes(x = 0.5, xend = 9.5, y = -Inf, yend = -Inf), color = "darkgray") + 
  geom_segment(x = Inf, xend = Inf, y = 0, yend = 100, color = "darkgray") + 
  coord_flip() + 
  theme(
    legend.position = c(0.95, 0.8),
    legend.title = element_blank(),
    legend.text = element_text(size = 11, color = "black"),
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 13, color = "black"),
    axis.ticks = element_line(color = "darkgray"), 
    axis.title = element_text(size = 13, color = "black", face = "bold")
  )
opt_icu.gr.p1


# Priority people aged 40-64 under minimizing ICU ##############################
opt_icu.gr.vc.range <- opt_icu.gr.vc.range %>% 
  mutate(var = as.character(var)) %>% 
  mutate(var = factor(var, levels = c("Vaccine model", "Vaccine efficacious in preventing infection vs. disease", 
                                      "Relative infectiousness of asymptomatic individuals",  
                                      "Start of vaccination relative to epidemic onset", 
                                      "Additional efficacy in preventing disease", "Vaccine efficacy", 
                                      "Daily vaccination capacity", "Vaccine acceptance", "R")))

opt_icu.gr.p2 <- ggplot(data = opt_icu.gr.vc.range) + 
  geom_bar(aes(var, vc2.max * 100), position = "identity", stat = "identity", fill = "#0B63A2") + 
  geom_bar(aes(var, vc2 * 100), position = "identity", stat = "identity", fill = "#CC79A7") + 
  geom_bar(aes(var, vc2.min * 100), position = "identity", stat = "identity", fill = "white") + 
  geom_hline(yintercept = opt_icu.gr.vc.range$vc2[1] * 100, color = "darkgray") + 
  labs(x = "", y = "Priority coverage (%)", 
       title = "Priority people aged 40-64\n under minimizing ICUs") + 
  scale_x_discrete(labels = c("Vaccine model", "Vaccine efficacious in\n preventing infection vs. disease", 
                              "Relative infectiousness of\n asymptomatic individuals",  
                              "Start of vaccination relative\n to epidemic onset", 
                              "Additional efficacy in\n preventing disease", "Vaccine efficacy", 
                              "Daily vaccination capacity", "Vaccine acceptance", "R")) + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10), 
                     labels = seq(0, 100, by = 10), position = "right") + 
  theme_classic() +
  theme(axis.line = element_blank()) +
  geom_segment(aes(x = 0.5, xend = 9.5, y = -Inf, yend = -Inf), color = "darkgray") + 
  geom_segment(x = Inf, xend = Inf, y = 0, yend = 100, color = "darkgray") + 
  coord_flip() + 
  theme(
    legend.position = c(0.95, 0.8),
    legend.title = element_blank(),
    legend.text = element_text(size = 11, color = "black"),
    plot.title = element_text(hjust = 0.5, margin = margin(0, 0, -0.5, 0, unit = "line")), 
    axis.text = element_text(size = 13, color = "black"),
    axis.ticks = element_line(color = "darkgray"), 
    axis.title = element_text(size = 13, color = "black", face = "bold")
  )
opt_icu.gr.p2


#### Panels a5 and b5 ##########################################################
# load data
opt_death.gr.vc.range <- read.xlsx("Source data/data_Supplementary Fig. 7.xlsx", 
                                 sheetName = "Panels a5 and b5")

opt_death.gr.vc.range <- opt_death.gr.vc.range %>% 
  rename(vc1 = baseline.coverage.for.1st.priority.group, 
         vc1.max = maximum.coverage.for.1st.priority.group, 
         vc1.min = minimal.coverage.for.1st.priority.group, 
         vc2 = baseline.coverage.for.2nd.priority.group, 
         vc2.max = maximum.coverage.for.2nd.priority.group, 
         vc2.min = minimal.coverage.for.2nd.priority.group) %>% 
  mutate(var = factor(var, levels = c("Vaccine model", "Vaccine efficacious in preventing infection vs. disease", 
                                      "Relative infectiousness of asymptomatic individuals",  
                                      "Start of vaccination relative to epidemic onset", 
                                      "Additional efficacy in preventing disease", "Vaccine efficacy", 
                                      "Daily vaccination capacity", "Vaccine acceptance", "R")))


# Priority people aged 65+ under minimizing deaths #############################
opt_death.gr.p1 <- ggplot(data = opt_death.gr.vc.range) + 
  geom_bar(aes(var, vc1.max * 100), position = "identity", stat = "identity", fill = "#0B63A2") + 
  geom_bar(aes(var, vc1 * 100), position = "identity", stat = "identity", fill = "#CC79A7") + 
  geom_bar(aes(var, vc1.min * 100), position = "identity", stat = "identity", fill = "white") + 
  geom_hline(yintercept = opt_death.gr.vc.range$vc1[1] * 100, color = "darkgray") + 
  labs(x = "", y = "Priority coverage (%)", 
       title = "Priority people aged 65+\n under minimizing deaths") + 
  scale_x_discrete(labels = c("Vaccine model", "Vaccine efficacious in\n preventing infection vs. disease", 
                              "Relative infectiousness of\n asymptomatic individuals",  
                              "Start of vaccination relative\n to epidemic onset", 
                              "Additional efficacy in\n preventing disease", "Vaccine efficacy", 
                              "Daily vaccination capacity", "Vaccine acceptance", "R")) + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10), 
                     labels = seq(0, 100, by = 10), position = "right") + 
  theme_classic() +
  theme(axis.line = element_blank()) +
  geom_segment(aes(x = 0.5, xend = 9.5, y = -Inf, yend = -Inf), color = "darkgray") + 
  geom_segment(x = Inf, xend = Inf, y = 0, yend = 100, color = "darkgray") + 
  coord_flip() + 
  theme(
    legend.position = c(0.95, 0.8),
    legend.title = element_blank(),
    legend.text = element_text(size = 11, color = "black"),
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 13, color = "black"),
    axis.ticks = element_line(color = "darkgray"), 
    axis.title = element_text(size = 13, color = "black", face = "bold")
  )
opt_death.gr.p1


# Priority people aged 40-64 under minimizing death ##############################
opt_death.gr.vc.range <- opt_death.gr.vc.range %>% 
  mutate(var = as.character(var)) %>% 
  mutate(var = factor(var, levels = c("Vaccine model", "Vaccine efficacious in preventing infection vs. disease", 
                                      "Relative infectiousness of asymptomatic individuals",  
                                      "Start of vaccination relative to epidemic onset", 
                                      "Additional efficacy in preventing disease", "Vaccine efficacy", 
                                      "Daily vaccination capacity", "Vaccine acceptance", "R")))

opt_death.gr.p2 <- ggplot(data = opt_death.gr.vc.range) + 
  geom_bar(aes(var, vc2.max * 100), position = "identity", stat = "identity", fill = "#0B63A2") + 
  geom_bar(aes(var, vc2 * 100), position = "identity", stat = "identity", fill = "#CC79A7") + 
  geom_bar(aes(var, vc2.min * 100), position = "identity", stat = "identity", fill = "white") + 
  geom_hline(yintercept = opt_death.gr.vc.range$vc2[1] * 100, color = "darkgray") + 
  labs(x = "", y = "Priority coverage (%)", 
       title = "Priority people aged 40-64\n under minimizing deaths") + 
  scale_x_discrete(labels = c("Vaccine model", "Vaccine efficacious in\n preventing infection vs. disease", 
                              "Relative infectiousness of\n asymptomatic individuals",  
                              "Start of vaccination relative\n to epidemic onset", 
                              "Additional efficacy in\n preventing disease", "Vaccine efficacy", 
                              "Daily vaccination capacity", "Vaccine acceptance", "R")) + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10), 
                     labels = seq(0, 100, by = 10), position = "right") + 
  theme_classic() +
  theme(axis.line = element_blank()) +
  geom_segment(aes(x = 0.5, xend = 9.5, y = -Inf, yend = -Inf), color = "darkgray") + 
  geom_segment(x = Inf, xend = Inf, y = 0, yend = 100, color = "darkgray") + 
  coord_flip() + 
  theme(
    legend.position = c(0.95, 0.8),
    legend.title = element_blank(),
    legend.text = element_text(size = 11, color = "black"),
    plot.title = element_text(hjust = 0.5, margin = margin(0, 0, -0.5, 0, unit = "line")), 
    axis.text = element_text(size = 13, color = "black"),
    axis.ticks = element_line(color = "darkgray"), 
    axis.title = element_text(size = 13, color = "black", face = "bold")
  )
opt_death.gr.p2


# combine above 10 plots into 2 rows
fig.com <- plot_grid(opt_infec.gr.p1,  
                     opt_symp.gr.p1 + 
                       theme(axis.text.y = element_blank()), 
                     opt_hosp.gr.p1 + 
                       theme(axis.text.y = element_blank()), 
                     opt_icu.gr.p1 + 
                       theme(axis.text.y = element_blank()), 
                     opt_death.gr.p1 + 
                       theme(axis.text.y = element_blank()), 
                     opt_infec.gr.p2 + labs(y = ""), 
                     opt_symp.gr.p2 + labs(y = "") + 
                       theme(axis.text.y = element_blank()),
                     opt_hosp.gr.p2 + labs(y = "") + 
                       theme(axis.text.y = element_blank()),
                     opt_icu.gr.p2 + labs(y = "") + 
                       theme(axis.text.y = element_blank()),
                     opt_death.gr.p2 + labs(y = "") + 
                       theme(axis.text.y = element_blank()), 
                     nrow = 2, axis = "l", 
                     rel_widths = c(1.7, 1, 1, 1, 1, 1, 1, 1, 1, 1), 
                     label_x = c(0.33, 0, 0, 0, 0, 0.33, 0, 0, 0, 0), 
                     labels = c("a1", "a2", "a3", "a4", "a5", "b1", "b2", "b3", "b4", "b5"))
fig.com

outfile <- glue("{dout}/Supplementary Fig. 7.tif")
tiff(outfile, width = 20, height = 10, unit = "in", res = 300, compression = "lzw")
print(fig.com)
dev.off()