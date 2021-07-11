################################################################################
#### Plot Fig. 3 in main text ####
rm(list = ls())

library(tidyverse)
library(glue)
library(xlsx)
library(cowplot)
library(scales)

setwd("~/DynamicVaccineAllocationMod-main")
dout <- "Source code/output"

#### Panel a ###################################################################
# load data
ap.novax.cpt <- read.xlsx("Source data/data_Fig. 3.xlsx", sheetName = "Panel a")

# plot death
ap.novax.cpt.newDeath <- ap.novax.cpt %>% 
  rename(capacity = daily.vaccination.capacity..millions.) %>% 
  mutate(policy = factor(policy, levels = c("Uniform", "Optimal Infection", 
                                            "Optimal Symp", "Optimal Hosp", 
                                            "Optimal ICU", "Optimal Death")))

p.r1c5 <- ggplot(ap.novax.cpt.newDeath) + 
  geom_line(aes(x = capacity, y = averted.prop * 100, color = policy), size = 1) + 
  geom_point(aes(x = capacity, y = averted.prop * 100, color = policy, shape = policy), size = 2) + 
  labs(x = "Daily vaccination capacity (millions)", y = "Averted proportion of deaths (%)", color = "Allocation strategy", 
       shape = "Allocation strategy") + 
  scale_x_continuous(limits = c(1, 3.5), breaks = seq(1, 3.5, by = 0.5)) + 
  scale_y_continuous(limits = c(0, 104), breaks = seq(0, 100, by = 20)) + 
  scale_color_manual(values = c("#FBE4CD", "#FC4E07", "#6698FF", "#00AFBB", "#E7B800", "#CC79A7")) + 
  scale_shape_manual(values = c(19, 0, 1, 2, 3, 4)) + 
  theme_classic() + 
  theme(axis.line = element_blank()) +
  geom_segment(aes(x = 1, xend = 3.5, y = -Inf, yend = -Inf), color = "darkgray") +
  geom_segment(aes(y = 0, yend = 100, x = -Inf, xend = -Inf), color = "darkgray") +
  theme(
    legend.position = c(0.75, 0.25),
    legend.title = element_text(size = 13, color = "black"),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.text = element_text(size = 13, color = "black"),
    axis.ticks = element_line(color = "darkgray"), 
    axis.title = element_text(size = 13, color = "black", face = "bold")
  )
p.r1c5



#### Panel b ###################################################################
# load data
ap.novax.ve <- read.xlsx("Source data/data_Fig. 3.xlsx", sheetName = "Panel b")

# plot death
ap.novax.ve.newDeath <- ap.novax.ve %>% 
  rename(ve = vaccine.efficacy) %>% 
  mutate(policy = factor(policy, levels = c("Uniform", "Optimal Infection", 
                                            "Optimal Symptomatic", "Optimal Hospitalized", 
                                            "Optimal ICU", "Optimal Death")))

p.r2c5 <- ggplot(ap.novax.ve.newDeath) + 
  geom_line(aes(x = ve * 100, y = averted.prop * 100, color = policy), size = 1) + 
  geom_point(aes(x = ve * 100, y = averted.prop * 100, color = policy, shape = policy), size = 2) + 
  labs(x = "Vaccine efficacy (%)", y = "Averted proportion of deaths (%)", color = "Allocation strategy", 
       shape = "Allocation strategy") + 
  scale_x_continuous(limits = c(60, 90), breaks = seq(50, 100, by = 10)) + 
  scale_y_continuous(limits = c(0, 104), breaks = seq(0, 100, by = 20)) + 
  scale_color_manual(values = c("#FBE4CD", "#FC4E07", "#6698FF", "#00AFBB", "#E7B800", "#CC79A7")) + 
  scale_shape_manual(values = c(19, 0, 1, 2, 3, 4)) + 
  theme_classic() + 
  theme(axis.line = element_blank()) +
  geom_segment(aes(x = 60, xend = 90, y = -Inf, yend = -Inf), color = "darkgray") +
  geom_segment(aes(y = 0, yend = 100, x = -Inf, xend = -Inf), color = "darkgray") +
  theme(
    legend.position = c(0.75, 0.25),
    legend.title = element_text(size = 13, color = "black"),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.text = element_text(size = 13, color = "black"),
    axis.ticks = element_line(color = "darkgray"), 
    axis.title = element_text(size = 13, color = "black", face = "bold")
  )
p.r2c5


#### Panel c ###################################################################
# load data
ap.novax.ageVE <- read.xlsx("Source data/data_Fig. 3.xlsx", sheetName = "Panel c")

# plot death
ap.novax.ageVE.newDeath <- ap.novax.ageVE %>% 
  rename(ageVE = efficacy.by.age) %>% 
  mutate(ageVE = factor(ageVE, levels = c("Heterogeneous", "Homogeneous"))) %>% 
  mutate(policy = factor(policy, levels = c("Uniform", "Optimal Infection", 
                                            "Optimal Symptomatic", "Optimal Hospitalized", 
                                            "Optimal ICU", "Optimal Death")))

p.r3c5 <- ggplot(ap.novax.ageVE.newDeath) + 
  geom_line(aes(x = as.integer(ageVE), y = averted.prop * 100, color = policy), size = 1) + 
  geom_point(aes(x = as.integer(ageVE), y = averted.prop * 100, color = policy, shape = policy), size = 2) + 
  labs(x = "Efficacy by age", y = "Averted proportion of infections (%)", color = "Allocation strategy", 
       shape = "Allocation strategy") + 
  scale_x_continuous(limits = c(0.8, 2.2), breaks = seq(1, 2, by = 1), 
                     labels = c("Heterogeneous", "Homogeneous")) + 
  scale_y_continuous(limits = c(0, 104), breaks = seq(0, 100, by = 20)) + 
  scale_color_manual(values = c("#FBE4CD", "#FC4E07", "#6698FF", "#00AFBB", "#E7B800", "#CC79A7")) + 
  scale_shape_manual(values = c(19, 0, 1, 2, 3, 4)) + 
  theme_classic() + 
  theme(axis.line = element_blank()) +
  geom_segment(aes(x = 1, xend = 2, y = -Inf, yend = -Inf), color = "darkgray") + 
  geom_segment(aes(y = 0, yend = 100, x = -Inf, xend = -Inf), color = "darkgray") +
  theme(
    legend.position = c(0.75, 0.25),
    legend.title = element_text(size = 13, color = "black"),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.text = element_text(size = 13, color = "black"),
    axis.ticks = element_line(color = "darkgray"), 
    axis.title = element_text(size = 13, color = "black", face = "bold")
  )
p.r3c5


#### Panel d ###################################################################
# load data
ap.novax.va <- read.xlsx("Source data/data_Fig. 3.xlsx", sheetName = "Panel d")

# plot death
ap.novax.va.newDeath <- ap.novax.va %>% 
  rename(va = vaccine.acceptance) %>% 
  mutate(va = factor(va, levels = c("Whole population", "Chinese survey", "Global survey"))) %>% 
  mutate(policy = factor(policy, levels = c("Uniform", "Optimal Infection", 
                                            "Optimal Symp", "Optimal Hosp", 
                                            "Optimal ICU", "Optimal Death")))

p.r4c5 <- ggplot(ap.novax.va.newDeath) + 
  geom_line(aes(x = as.integer(va), y = averted.prop * 100, color = policy), size = 1) + 
  geom_point(aes(x = as.integer(va), y = averted.prop * 100, color = policy, shape = policy), size = 2) + 
  labs(x = "Vaccine acceptance", y = "Averted proportion of deaths (%)", color = "Allocation strategy", 
       shape = "Allocation strategy") + 
  scale_x_continuous(limits = c(1, 3), breaks = seq(1, 3, by = 1), 
                     labels = c("Whole population", "Chinese survey", "Global survey")) + 
  scale_y_continuous(limits = c(0, 104), breaks = seq(0, 100, by = 20)) + 
  scale_color_manual(values = c("#FBE4CD", "#FC4E07", "#6698FF", "#00AFBB", "#E7B800", "#CC79A7")) + 
  scale_shape_manual(values = c(19, 0, 1, 2, 3, 4)) + 
  theme_classic() + 
  theme(axis.line = element_blank()) +
  geom_segment(aes(x = 1, xend = 3, y = -Inf, yend = -Inf), color = "darkgray") +
  geom_segment(aes(y = 0, yend = 100, x = -Inf, xend = -Inf), color = "darkgray") +
  theme(
    legend.position = c(0.75, 0.25),
    legend.title = element_text(size = 13, color = "black"),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.text = element_text(size = 13, color = "black"),
    axis.ticks = element_line(color = "darkgray"), 
    axis.title = element_text(size = 13, color = "black", face = "bold")
  )
p.r4c5


#### Panel e ###################################################################
# load data
ap.novax.ve_symp <- read.xlsx("Source data/data_Fig. 3.xlsx", sheetName = "Panel e")

# plot death
ap.novax.ve_symp.newDeath <- ap.novax.ve_symp %>% 
  rename(ve_s = additional.efficacy.in.preventing.disease) %>% 
  mutate(policy = factor(policy, levels = c("Uniform", "Optimal Infection", 
                                            "Optimal Symptomatic", "Optimal Hospitalized", 
                                            "Optimal ICU", "Optimal Death")))

p.r5c5 <- ggplot(ap.novax.ve_symp.newDeath) + 
  geom_line(aes(x = ve_s * 100, y = averted.prop * 100, color = policy), size = 1) + 
  geom_point(aes(x = ve_s * 100, y = averted.prop * 100, color = policy, shape = policy), size = 2) + 
  labs(x = "Additional efficacy in\n preventing disease (%)", y = "Averted proportion of deaths (%)", color = "Allocation strategy", 
       shape = "Allocation strategy") + 
  scale_x_continuous(limits = c(0, 75), breaks = seq(0, 75, by = 25)) + 
  scale_y_continuous(limits = c(0, 104), breaks = seq(0, 100, by = 20)) + 
  scale_color_manual(values = c("#FBE4CD", "#FC4E07", "#6698FF", "#00AFBB", "#E7B800", "#CC79A7")) + 
  scale_shape_manual(values = c(19, 0, 1, 2, 3, 4)) + 
  theme_classic() + 
  theme(axis.line = element_blank()) +
  geom_segment(aes(x = 0, xend = 75, y = -Inf, yend = -Inf), color = "darkgray") +
  geom_segment(aes(y = 0, yend = 100, x = -Inf, xend = -Inf), color = "darkgray") +
  theme(
    legend.position = c(0.75, 0.25),
    legend.title = element_text(size = 13, color = "black"),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.text = element_text(size = 13, color = "black"),
    axis.ticks = element_line(color = "darkgray"), 
    axis.title = element_text(size = 13, color = "black", face = "bold")
  )
p.r5c5


#### Panel f ###################################################################
# load data
ap.novax.Prev <- read.xlsx("Source data/data_Fig. 3.xlsx", sheetName = "Panel f")

# plot death
ap.novax.Prev.newDeath <- ap.novax.Prev %>% 
  rename(Prev = vaccine.efficacious.in.preventing) %>% 
  mutate(Prev = factor(Prev, levels = c("Infection", "Disease"))) %>% 
  mutate(policy = factor(policy, levels = c("Uniform", "Optimal Infection", 
                                            "Optimal Symptomatic", "Optimal Hospitalized", 
                                            "Optimal ICU", "Optimal Death")))

p.r6c5 <- ggplot(ap.novax.Prev.newDeath) + 
  geom_line(aes(x = as.integer(Prev), y = averted.prop * 100, color = policy), size = 1) + 
  geom_point(aes(x = as.integer(Prev), y = averted.prop * 100, color = policy, shape = policy), size = 2) + 
  labs(x = "Vaccine efficacious\n in preventing", y = "Averted proportion of infections (%)", color = "Allocation strategy", 
       shape = "Allocation strategy") + 
  scale_x_continuous(limits = c(1, 2), breaks = seq(1, 2, by = 1), 
                     labels = c("Infection", "Disease")) + 
  scale_y_continuous(limits = c(0, 104), breaks = seq(0, 100, by = 20)) + 
  scale_color_manual(values = c("#FBE4CD", "#FC4E07", "#6698FF", "#00AFBB", "#E7B800", "#CC79A7")) + 
  scale_shape_manual(values = c(19, 0, 1, 2, 3, 4)) + 
  theme_classic() + 
  theme(axis.line = element_blank()) +
  geom_segment(aes(x = 1, xend = 2, y = -Inf, yend = -Inf), color = "darkgray") + 
  geom_segment(aes(y = 0, yend = 100, x = -Inf, xend = -Inf), color = "darkgray") +
  theme(
    legend.position = c(0.75, 0.25),
    legend.title = element_text(size = 13, color = "black"),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.text = element_text(size = 13, color = "black"),
    axis.ticks = element_line(color = "darkgray"), 
    axis.title = element_text(size = 13, color = "black", face = "bold")
  )
p.r6c5


#### Panel g ###################################################################
# load data
ap.novax.Campaign <- read.xlsx("Source data/data_Fig. 3.xlsx", sheetName = "Panel g")

# plot death
ap.novax.Campaign.newDeath <- ap.novax.Campaign %>% 
  rename(delay = start.of.vaccination.relative.to.epidemic.onset..days.) %>% 
  mutate(policy = factor(policy, levels = c("Uniform", "Optimal Infection", 
                                            "Optimal Symptomatic", "Optimal Hospitalized", 
                                            "Optimal ICU", "Optimal Death")))

p.r7c5 <- ggplot(ap.novax.Campaign.newDeath) + 
  geom_line(aes(x = delay, y = averted.prop * 100, color = policy), size = 1) + 
  geom_point(aes(x = delay, y = averted.prop * 100, color = policy, shape = policy), size = 2) + 
  labs(x = "Start of vaccination relative\n to epidemic onset (days)", y = "Averted proportion of infections (%)", color = "Allocation strategy", 
       shape = "Allocation strategy") + 
  scale_x_continuous(limits = c(-90, 30), breaks = seq(-90, 30, by = 30)) + 
  scale_y_continuous(limits = c(0, 104), breaks = seq(0, 100, by = 20)) + 
  scale_color_manual(values = c("#FBE4CD", "#FC4E07", "#6698FF", "#00AFBB", "#E7B800", "#CC79A7")) + 
  scale_shape_manual(values = c(19, 0, 1, 2, 3, 4)) + 
  theme_classic() + 
  theme(axis.line = element_blank()) +
  geom_segment(aes(x = -90, xend = 30, y = -Inf, yend = -Inf), color = "darkgray") +
  geom_segment(aes(y = 0, yend = 100, x = -Inf, xend = -Inf), color = "darkgray") +
  theme(
    legend.position = c(0.75, 0.25),
    legend.title = element_text(size = 13, color = "black"),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.text = element_text(size = 13, color = "black"),
    axis.ticks = element_line(color = "darkgray"), 
    axis.title = element_text(size = 13, color = "black", face = "bold")
  )
p.r7c5


#### Panel h ###################################################################
# load data
ap.novax.include <- read.xlsx("Source data/data_Fig. 3.xlsx", sheetName = "Panel h")

# plot death
ap.novax.include.newDeath <- ap.novax.include %>% 
  rename(interval = vaccine.offered.to) %>% 
  mutate(interval = factor(interval, levels = c("All", "No recent infection", "Never infected"))) %>% 
  mutate(policy = factor(policy, levels = c("Uniform", "Optimal Infection", 
                                            "Optimal Symptomatic", "Optimal Hospitalized", 
                                            "Optimal ICU", "Optimal Death")))

p.r8c5 <- ggplot(ap.novax.include.newDeath) + 
  geom_line(aes(x = as.integer(interval), y = averted.prop * 100, color = policy), size = 1) + 
  geom_point(aes(x = as.integer(interval), y = averted.prop * 100, color = policy, shape = policy), size = 2) + 
  labs(x = "Vaccine offered to\n", y = "Averted proportion of deaths (%)", color = "Allocation strategy", 
       shape = "Allocation strategy") + 
  scale_x_continuous(limits = c(1, 3), breaks = seq(1, 3, by = 1), 
                     labels = c("All", "No recent infection", "Never infected")) + 
  scale_y_continuous(limits = c(0, 104), breaks = seq(0, 100, by = 20)) + 
  scale_color_manual(values = c("#FBE4CD", "#FC4E07", "#6698FF", "#00AFBB", "#E7B800", "#CC79A7")) + 
  scale_shape_manual(values = c(19, 0, 1, 2, 3, 4)) + 
  theme_classic() + 
  theme(axis.line = element_blank()) +
  geom_segment(aes(x = 1, xend = 3, y = -Inf, yend = -Inf), color = "darkgray") + 
  geom_segment(aes(y = 0, yend = 100, x = -Inf, xend = -Inf), color = "darkgray") +
  theme(
    legend.position = c(0.75, 0.25),
    legend.title = element_text(size = 13, color = "black"),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.text = element_text(size = 13, color = "black"),
    axis.ticks = element_line(color = "darkgray"), 
    axis.title = element_text(size = 13, color = "black", face = "bold")
  )
p.r8c5


#### Panel i ###################################################################
# load data
ap.novax.R <- read.xlsx("Source data/data_Fig. 3.xlsx", sheetName = "Panel i")

# plot death
ap.novax.R.newDeath <- ap.novax.R %>% 
  mutate(policy = factor(policy, levels = c("Uniform", "Optimal Infection", 
                                            "Optimal Symptomatic", "Optimal Hospitalized", 
                                            "Optimal ICU", "Optimal Death")))

p.r9c5 <- ggplot(ap.novax.R.newDeath) + 
  geom_line(aes(x = R, y = averted.prop * 100, color = policy), size = 1) + 
  geom_point(aes(x = R, y = averted.prop * 100, color = policy, shape = policy), size = 2) + 
  labs(x = "R\n", y = "Averted proportion of deaths (%)", color = "Allocation strategy", 
       shape = "Allocation strategy") + 
  scale_x_continuous(limits = c(1.25, 2), breaks = seq(1.25, 2, by = 0.25), 
                     labels = seq(1.25, 2, by = 0.25)) + 
  scale_y_continuous(limits = c(0, 104), breaks = seq(0, 100, by = 20)) + 
  scale_color_manual(values = c("#FBE4CD", "#FC4E07", "#6698FF", "#00AFBB", "#E7B800", "#CC79A7")) + 
  scale_shape_manual(values = c(19, 0, 1, 2, 3, 4)) + 
  theme_classic() + 
  theme(axis.line = element_blank()) +
  geom_segment(aes(x = 1.25, xend = 2, y = -Inf, yend = -Inf), color = "darkgray") +
  geom_segment(aes(y = 0, yend = 100, x = -Inf, xend = -Inf), color = "darkgray") +
  theme(
    legend.position = c(0.25, 0.25),
    legend.title = element_text(size = 13, color = "black"),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.text = element_text(size = 13, color = "black"),
    axis.ticks = element_line(color = "darkgray"), 
    axis.title = element_text(size = 13, color = "black", face = "bold")
  )
p.r9c5


#### Panel j ###################################################################
# load data
ap.novax.mixing <- read.xlsx("Source data/data_Fig. 3.xlsx", sheetName = "Panel j")

# plot death
ap.novax.mixing.newDeath <- ap.novax.mixing %>% 
  rename(mixing = age.mixing.patterns) %>% 
  mutate(mixing = factor(mixing, levels = c("pre-pandemic", "pandemic"))) %>% 
  mutate(policy = factor(policy, levels = c("Uniform", "Optimal Infection", 
                                            "Optimal Symptomatic", "Optimal Hospitalized", 
                                            "Optimal ICU", "Optimal Death")))

p.r10c5 <- ggplot(ap.novax.mixing.newDeath) + 
  geom_line(aes(x = as.integer(mixing), y = averted.prop * 100, color = policy), size = 1) + 
  geom_point(aes(x = as.integer(mixing), y = averted.prop * 100, color = policy, shape = policy), size = 2) + 
  labs(x = "Age-mixing patterns\n", y = "Averted proportion of infections (%)", color = "Allocation strategy", 
       shape = "Allocation strategy") + 
  scale_x_continuous(limits = c(1, 2), breaks = seq(1, 2, by = 1), 
                     labels = c("pre-pandemic", "pandemic")) + 
  scale_y_continuous(limits = c(0, 104), breaks = seq(0, 100, by = 20)) + 
  scale_color_manual(values = c("#FBE4CD", "#FC4E07", "#6698FF", "#00AFBB", "#E7B800", "#CC79A7")) + 
  scale_shape_manual(values = c(19, 0, 1, 2, 3, 4)) + 
  theme_classic() + 
  theme(axis.line = element_blank()) +
  geom_segment(aes(x = 1, xend = 2, y = -Inf, yend = -Inf), color = "darkgray") + 
  geom_segment(aes(y = 0, yend = 100, x = -Inf, xend = -Inf), color = "darkgray") +
  theme(
    legend.position = c(0.75, 0.25),
    legend.title = element_text(size = 13, color = "black"),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.text = element_text(size = 13, color = "black"),
    axis.ticks = element_line(color = "darkgray"), 
    axis.title = element_text(size = 13, color = "black", face = "bold")
  )
p.r10c5


#### Panel k ###################################################################
# load data
ap.novax.infec <- read.xlsx("Source data/data_Fig. 3.xlsx", sheetName = "Panel k")

# plot death
ap.novax.infec.newDeath <- ap.novax.infec %>% 
  rename(infec = relative.infectiousness.of.asymptomatic.individuals) %>% 
  mutate(infec = factor(infec, levels = c("1", "0.5"))) %>% 
  mutate(policy = factor(policy, levels = c("Uniform", "Optimal Infection", 
                                            "Optimal Symptomatic", "Optimal Hospitalized", 
                                            "Optimal ICU", "Optimal Death")))

p.r11c5 <- ggplot(ap.novax.infec.newDeath) + 
  geom_line(aes(x = as.integer(infec), y = averted.prop * 100, color = policy), size = 1) + 
  geom_point(aes(x = as.integer(infec), y = averted.prop * 100, color = policy, shape = policy), size = 2) + 
  labs(x = "Relative infectiousness of\n asymptomatic individuals", y = "Averted proportion of infections (%)", color = "Allocation strategy", 
       shape = "Allocation strategy") + 
  scale_x_continuous(limits = c(1, 2), breaks = seq(1, 2, by = 1), 
                     labels = c("1", "0.5")) + 
  scale_y_continuous(limits = c(0, 104), breaks = seq(0, 100, by = 20)) + 
  scale_color_manual(values = c("#FBE4CD", "#FC4E07", "#6698FF", "#00AFBB", "#E7B800", "#CC79A7")) + 
  scale_shape_manual(values = c(19, 0, 1, 2, 3, 4)) + 
  theme_classic() + 
  theme(axis.line = element_blank()) +
  geom_segment(aes(x = 1, xend = 2, y = -Inf, yend = -Inf), color = "darkgray") + 
  geom_segment(aes(y = 0, yend = 100, x = -Inf, xend = -Inf), color = "darkgray") +
  theme(
    legend.position = c(0.75, 0.25),
    legend.title = element_text(size = 13, color = "black"), 
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.text = element_text(size = 13, color = "black"),
    axis.ticks = element_line(color = "darkgray"), 
    axis.title = element_text(size = 13, color = "black", face = "bold")
  )
p.r11c5


#### Panel l ###################################################################
# load data
ap.novax.report <- read.xlsx("Source data/data_Fig. 3.xlsx", sheetName = "Panel l")

# plot death
ap.novax.report.newDeath <- ap.novax.report %>% 
  rename(report = case.reporting) %>% 
  mutate(report = factor(report, levels = c("perfect", "noisy", "noisy+delay"))) %>% 
  mutate(policy = factor(policy, levels = c("Uniform", "Optimal Infection", 
                                            "Optimal Symptomatic", "Optimal Hospitalized", 
                                            "Optimal ICU", "Optimal Death")))

p.r12c5 <- ggplot(ap.novax.report.newDeath) + 
  geom_line(aes(x = as.integer(report), y = averted.prop * 100, color = policy), size = 1) + 
  geom_point(aes(x = as.integer(report), y = averted.prop * 100, color = policy, shape = policy), size = 2) + 
  labs(x = "Case reporting\n", y = "Averted proportion of infections (%)", color = "Allocation strategy", 
       shape = "Allocation strategy") + 
  scale_x_continuous(limits = c(1, 3), breaks = seq(1, 3, by = 1), 
                     labels = c("perfect", "noisy", "noisy+delay")) + 
  scale_y_continuous(limits = c(0, 104), breaks = seq(0, 100, by = 20)) + 
  scale_color_manual(values = c("#FBE4CD", "#FC4E07", "#6698FF", "#00AFBB", "#E7B800", "#CC79A7")) + 
  scale_shape_manual(values = c(19, 0, 1, 2, 3, 4)) + 
  theme_classic() + 
  theme(axis.line = element_blank()) +
  geom_segment(aes(x = 1, xend = 3, y = -Inf, yend = -Inf), color = "darkgray") + 
  geom_segment(aes(y = 0, yend = 100, x = -Inf, xend = -Inf), color = "darkgray") +
  theme(
    legend.position = c(0.75, 0.25),
    legend.title = element_text(size = 13, color = "black"),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.text = element_text(size = 13, color = "black"),
    axis.ticks = element_line(color = "darkgray"), 
    axis.title = element_text(size = 13, color = "black", face = "bold")
  )
p.r12c5


# combine panels by outcome
fig.com.death <- plot_grid(p.r1c5 + theme(legend.position = "none"), 
                           NULL, 
                           p.r2c5 + theme(legend.position = "none") + labs(y = ""), 
                           NULL, 
                           p.r3c5 + theme(legend.position = "none") + labs(y = ""), 
                           NULL, 
                           p.r4c5 + theme(legend.position = "none") + labs(y = ""), 
                           NULL, 
                           p.r5c5 + theme(legend.position = "none"), 
                           NULL, 
                           p.r6c5 + theme(legend.position = "none") + labs(y = ""), 
                           NULL,
                           p.r7c5 + theme(legend.position = "none") + labs(y = ""),  
                           NULL, 
                           p.r8c5 + theme(legend.position = "none") + labs(y = ""),  
                           NULL, 
                           p.r9c5 + theme(legend.position = "none"), 
                           NULL, 
                           p.r10c5 + theme(legend.position = "none") + labs(y = ""), 
                           NULL,
                           p.r11c5 + theme(legend.position = "none") + labs(y = ""),  
                           NULL, 
                           p.r12c5 + theme(legend.position = "none") + labs(y = ""),
                           nrow = 3, axis = "l", 
                           labels = c("a", "", "b", "", "c", "", "d", "", 
                                      "e", "", "f", "", "g", "", "h", "", 
                                      "i", "", "j", "", "k", "", "l"), 
                           label_x = c(0.13, 0, 0.13, 0, 0.13, 0, 0.13, 0, 
                                       0.13, 0, 0.13, 0, 0.13, 0, 0.13, 0, 
                                       0.13, 0, 0.13, 0, 0.13, 0, 0.13), 
                           rel_widths = c(1, -0.05, 1, -0.05, 1, -0.02, 1, 0.1, 
                                          1, -0.05, 1, -0.05, 1, -0.05, 1, 0.1, 
                                          1, -0.05, 1, -0.05, 1, -0.05, 1))
fig.com.death

# draw the legend
legend <- get_legend(p.r1c5)
fig.com.death1 <- ggdraw(fig.com.death) + 
  draw_plot(legend, -0.56, 0.55, 1, 1)
fig.com.death1

outfile <- glue("{dout}/Fig. 3.tif")
tiff(outfile, width = 16, height = 12, unit = "in", res = 300, compression = "lzw")
print(fig.com.death1)
dev.off()
