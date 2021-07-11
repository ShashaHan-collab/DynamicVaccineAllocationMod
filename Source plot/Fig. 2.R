################################################################################
#### Plot Fig. 2 in main text ####
rm(list = ls())

library(tidyverse)
library(glue)
library(xlsx)
library(cowplot)
library(scales)

setwd("~/DynamicVaccineAllocationMod-main")
dout <- "Source code/output"

# load data
total <- read.xlsx("Source data/data_Fig. 2.xlsx", sheetIndex = 1)

# compare total outcomes across policies #######################################
total.long <- total %>%
  pivot_longer(starts_with("new"), names_to = "outcome", values_to = "total") %>%
  mutate(
    outcome = factor(outcome, levels = c("newI", "newSymp", "newHosp", "newICU", "newDeath")), 
    policy = factor(policy, levels = c("No Vaccine", "Uniform", "Optimal Infection", "Optimal Symptomatic", "Optimal Hospitalized", "Optimal ICU", "Optimal Death"))
  )

total.long$policy <- plyr::revalue(total.long$policy, 
                                   c("Optimal Symptomatic" = "Optimal Symp", 
                                     "Optimal Hospitalized" = "Optimal Hosp"))

# lab1, lab2, broken, lab3, lab4
rescale.y <- function(y, lab1, lab2, lab3, lab4) {
  y.new <- lab2 + (lab2 - lab1) + (y - lab3) / (lab4 - lab3) * (lab2 - lab1)
  return(y.new)
}

# plot infection
total.newI <- total.long %>% 
  filter(outcome %in% c("newI")) %>% 
  mutate(total1 = ifelse(total > 25e+07, rescale.y(total, 20e+07, 25e+07, 70e+07, 71e+07), total))

tot.p1 <- ggplot(total.newI, aes(x = outcome, y = total1 / 1e+06, fill = policy)) +
  geom_bar(stat = "identity", position = "dodge", color = NA, size = 0.2) +
  geom_text(aes(label = format(round(total / 1e+06, 2), nsmall = 2)), 
            position = position_dodge(width = 0.9), vjust = -0.25, hjust = 0.5
  ) +
  labs(x = "", y = "Infections (1,000,000)") +
  scale_x_discrete(labels = c("Scenarios")) +
  scale_y_continuous(limits = c(0, 350), breaks = seq(0, 350, by = 50), 
                     labels = c(seq(0, 250, by = 50), 700, 710)) + 
  scale_fill_manual(values = c("#F79660", "#FBE4CD", "#CBE3C1", "#A4D4B0", 
                               "#77C4BC", "#2CB5B3", "#4DA9C7")) + 
  theme_classic() + 
  theme(axis.line = element_blank()) +
  geom_segment(aes(x = 0.5, xend = 1.5, y = -Inf, yend = -Inf), color = "darkgray") + 
  geom_segment(x = -Inf, xend = -Inf, y = 0, yend = 272, color = "darkgray") + 
  geom_segment(x = 0.375, xend = 0.425, y = 269, yend = 275, color = "darkgray") + 
  geom_segment(x = 0.375, xend = 0.425, y = 275, yend = 281, color = "darkgray") + 
  geom_segment(x = -Inf, xend = -Inf, y = 278, yend = 350, color = "darkgray") + 
  coord_cartesian(clip = "off") + 
  theme(
    # legend.position = "top",
    legend.position = c(0.7, 0.78), 
    legend.title = element_blank(),
    # legend.text = element_text(size = 13, color = "black"),
    legend.text = element_text(size = 11, color = "black"),
    axis.text.x = element_text(size = 13, color = "black"),
    axis.ticks.x = element_line(color = "darkgray"), 
    axis.text.y = element_text(size = 13, color = "black"),
    axis.ticks.y = element_line(color = "darkgray"), 
    axis.title.y = element_text(size = 13, color = "black", face = "bold")
  )
tot.p1


# plot symptomatic case
total.newSymp <- total.long %>% 
  filter(outcome %in% c("newSymp"))

tot.p2 <- ggplot(total.newSymp, aes(x = outcome, y = total / 1e+06, fill = policy)) +
  geom_bar(stat = "identity", position = "dodge", color = NA, size = 0.2) +
  geom_text(aes(label = format(round(total / 1e+06, 2), nsmall = 2)),
            position = position_dodge(width = 0.9), vjust = -0.25, hjust = 0.6
  ) +
  labs(x = "", y = "Symptomatic cases (1,000,000)") +
  scale_x_discrete(labels = c("Scenarios")) +
  scale_y_continuous(limits = c(0, 200), breaks = seq(0, 200, by = 50), 
                     labels = seq(0, 200, by = 50)) + 
  scale_fill_manual(values = c("#F79660", "#FBE4CD", "#CBE3C1", "#A4D4B0", 
                               "#77C4BC", "#2CB5B3","#4DA9C7")) + 
  theme_classic() +
  theme(axis.line = element_blank()) +
  geom_segment(aes(x = 0.5, xend = 1.5, y = -Inf, yend = -Inf), color = "darkgray") + 
  geom_segment(x = -Inf, xend = -Inf, y = 0, yend = 200, color = "darkgray") + 
  coord_cartesian(clip = "off") + 
  theme(
    legend.position = c(0.75, 0.8),
    legend.title = element_blank(),
    legend.text = element_text(size = 11, color = "black"),
    axis.text.x = element_text(size = 13, color = "black"),
    axis.ticks.x = element_line(color = "darkgray"), 
    axis.text.y = element_text(size = 13, color = "black"),
    axis.ticks.y = element_line(color = "darkgray"), 
    axis.title.y = element_text(size = 13, color = "black", face = "bold")
  )
tot.p2


# plot hospitalization
total.newHosp <- total.long %>% 
  filter(outcome %in% c("newHosp"))

tot.p3 <- ggplot(total.newHosp, aes(x = outcome, y = total / 1e+06, fill = policy)) +
  geom_bar(stat = "identity", position = "dodge", color = NA, size = 0.2) +
  geom_text(aes(label = format(round(total / 1e+06, 2), nsmall = 2)),
            position = position_dodge(width = 0.9), vjust = -0.25, hjust = 0.55
  ) +
  labs(x = "", y = "Hospitalizations (1,000,000)") +
  scale_x_discrete(labels = c("Scenarios")) +
  scale_y_continuous(limits = c(0, 200), breaks = seq(0, 200, by = 50), 
                     labels = seq(0, 200, by = 50)) + 
  scale_fill_manual(values = c("#F79660", "#FBE4CD", "#CBE3C1", "#A4D4B0", 
                               "#77C4BC", "#2CB5B3","#4DA9C7")) + 
  theme_classic() +
  theme(axis.line = element_blank()) +
  geom_segment(x = 0.5, xend = 1.5, y = -Inf, yend = -Inf, color = "darkgray") + 
  geom_segment(x = -Inf, xend = -Inf, y = 0, yend = 200, color = "darkgray") + 
  coord_cartesian(clip = "off") + 
  theme(
    legend.position = c(0.75, 0.8),
    legend.title = element_blank(),
    legend.text = element_text(size = 11, color = "black"),
    axis.text.x = element_text(size = 13, color = "black"),
    axis.ticks.x = element_line(color = "darkgray"), 
    axis.text.y = element_text(size = 13, color = "black"),
    axis.ticks.y = element_line(color = "darkgray"), 
    axis.title.y = element_text(size = 13, color = "black", face = "bold")
  )
tot.p3


# plot ICU
total.newICU <- total.long %>% 
  filter(outcome %in% c("newICU"))

tot.p4 <- ggplot(total.newICU, aes(x = outcome, y = total / 1e+06, fill = policy)) +
  geom_bar(stat = "identity", position = "dodge", color = NA, size = 0.2) +
  geom_text(aes(label = format(round(total / 1e+06, 2), nsmall = 2)),
            position = position_dodge(width = 0.9), vjust = -0.25, hjust = 0.55
  ) +
  labs(x = "", y = "ICUs (1,000,000)") +
  scale_x_discrete(labels = c("Scenarios")) +
  scale_y_continuous(limits = c(0, 7), breaks = seq(0, 7, by = 1), 
                     labels = seq(0, 7, by = 1)) + 
  scale_fill_manual(values = c("#F79660", "#FBE4CD", "#CBE3C1", "#A4D4B0", 
                               "#77C4BC", "#2CB5B3","#4DA9C7")) + 
  theme_classic() +
  theme(axis.line = element_blank()) +
  geom_segment(x = 0.5, xend = 1.5, y = -Inf, yend = -Inf, color = "darkgray") + 
  geom_segment(x = -Inf, xend = -Inf, y = 0, yend = 7, color = "darkgray") + 
  coord_cartesian(clip = "off") + 
  theme(
    legend.position = c(0.75, 0.8),
    legend.title = element_blank(),
    legend.text = element_text(size = 11, color = "black"),
    axis.text.x = element_text(size = 13, color = "black"),
    axis.ticks.x = element_line(color = "darkgray"), 
    axis.text.y = element_text(size = 13, color = "black"),
    axis.ticks.y = element_line(color = "darkgray"), 
    axis.title.y = element_text(size = 13, color = "black", face = "bold")
  )
tot.p4


# plot death
total.newDeath <- total.long %>% 
  filter(outcome %in% c("newDeath"))

tot.p5 <- ggplot(total.newDeath, aes(x = outcome, y = total / 1e+06, fill = policy)) +
  geom_bar(stat = "identity", position = "dodge", color = NA, size = 0.2) +
  geom_text(aes(label = format(round(total / 1e+06, 2), nsmall = 2)),
            position = position_dodge(width = 0.9), vjust = -0.25, hjust = 0.55
  ) +
  labs(x = "", y = "Deaths (1,000,000)") +
  scale_x_discrete(labels = c("Scenarios")) +
  scale_y_continuous(limits = c(0, 7), breaks = seq(0, 7, by = 1), 
                     labels = seq(0, 7, by = 1)) + 
  scale_fill_manual(values = c("#F79660", "#FBE4CD", "#CBE3C1", "#A4D4B0", 
                               "#77C4BC", "#2CB5B3","#4DA9C7")) + 
  theme_classic() +
  theme(axis.line = element_blank()) +
  geom_segment(aes(x = 0.5, xend = 1.5, y = -Inf, yend = -Inf), color = "darkgray") + 
  geom_segment(x = -Inf, xend = -Inf, y = 0, yend = 7, color = "darkgray") + 
  coord_cartesian(clip = "off") + 
  theme(
    legend.position = c(0.75, 0.8),
    legend.title = element_blank(),
    legend.text = element_text(size = 11, color = "black"),
    axis.text.x = element_text(size = 13, color = "black"),
    axis.ticks.x = element_line(color = "darkgray"), 
    axis.text.y = element_text(size = 13, color = "black"),
    axis.ticks.y = element_line(color = "darkgray"), 
    axis.title.y = element_text(size = 13, color = "black", face = "bold")
  )
tot.p5


# plot proportion of averted outcomes for each policy ##########################
# averted proportions of optimal infection policy compared with no vax
ap.novax.infec <- total %>%
  filter(policy %in% c("No Vaccine", "Optimal Infection")) %>%
  pivot_longer(starts_with("new"), names_to = "outcome", values_to = "total") %>%
  pivot_wider(names_from = "policy", values_from = "total") %>%
  mutate(averted = `No Vaccine` - `Optimal Infection`) %>%
  mutate(averted.prop = averted / `No Vaccine`) %>%
  mutate(outcome = factor(outcome, levels = c("newI", "newSymp", "newHosp", "newICU", "newDeath")))

# averted proportions optimal symptomatic policy compared with no vax
ap.novax.symp <- total %>%
  filter(policy %in% c("No Vaccine", "Optimal Symptomatic")) %>%
  pivot_longer(starts_with("new"), names_to = "outcome", values_to = "total") %>%
  pivot_wider(names_from = "policy", values_from = "total") %>%
  mutate(averted = `No Vaccine` - `Optimal Symptomatic`) %>%
  mutate(averted.prop = averted / `No Vaccine`) %>%
  mutate(outcome = factor(outcome, levels = c("newI", "newSymp", "newHosp", "newICU", "newDeath")))

# averted proportions of optimal hospitalized policy compared with no vax
ap.novax.hosp <- total %>%
  filter(policy %in% c("No Vaccine", "Optimal Hospitalized")) %>%
  pivot_longer(starts_with("new"), names_to = "outcome", values_to = "total") %>%
  pivot_wider(names_from = "policy", values_from = "total") %>%
  mutate(averted = `No Vaccine` - `Optimal Hospitalized`) %>%
  mutate(averted.prop = averted / `No Vaccine`) %>%
  mutate(outcome = factor(outcome, levels = c("newI", "newSymp", "newHosp", "newICU", "newDeath")))

# averted proportions of optimal ICU policy compared with no vax
ap.novax.icu <- total %>%
  filter(policy %in% c("No Vaccine", "Optimal ICU")) %>%
  pivot_longer(starts_with("new"), names_to = "outcome", values_to = "total") %>%
  pivot_wider(names_from = "policy", values_from = "total") %>%
  mutate(averted = `No Vaccine` - `Optimal ICU`) %>%
  mutate(averted.prop = averted / `No Vaccine`) %>%
  mutate(outcome = factor(outcome, levels = c("newI", "newSymp", "newHosp", "newICU", "newDeath")))

# averted proportions of optimal death policy compared with no vax
ap.novax.death <- total %>%
  filter(policy %in% c("No Vaccine", "Optimal Death")) %>%
  pivot_longer(starts_with("new"), names_to = "outcome", values_to = "total") %>%
  pivot_wider(names_from = "policy", values_from = "total") %>%
  mutate(averted = `No Vaccine` - `Optimal Death`) %>%
  mutate(averted.prop = averted / `No Vaccine`) %>%
  mutate(outcome = factor(outcome, levels = c("newI", "newSymp", "newHosp", "newICU", "newDeath")))

# averted proportions of uniform policy compared with no vax
ap.novax.unif <- total %>%
  filter(policy %in% c("No Vaccine", "Uniform")) %>%
  pivot_longer(starts_with("new"), names_to = "outcome", values_to = "total") %>%
  pivot_wider(names_from = "policy", values_from = "total") %>%
  mutate(averted = `No Vaccine` - `Uniform`) %>%
  mutate(averted.prop = averted / `No Vaccine`) %>%
  mutate(outcome = factor(outcome, levels = c("newI", "newSymp", "newHosp", "newICU", "newDeath")))

# line plot of averted proportions for each policy #############################
ap.novax.infec <- ap.novax.infec %>% 
  select(outcome, averted.prop) %>% 
  mutate(policy = "Optimal Infection")

ap.novax.symp <- ap.novax.symp %>% 
  select(outcome, averted.prop) %>% 
  mutate(policy = "Optimal Symptomatic")

ap.novax.hosp <- ap.novax.hosp %>% 
  select(outcome, averted.prop) %>% 
  mutate(policy = "Optimal Hospitalized")

ap.novax.icu <- ap.novax.icu %>% 
  select(outcome, averted.prop) %>% 
  mutate(policy = "Optimal ICU")

ap.novax.death <- ap.novax.death %>% 
  select(outcome, averted.prop) %>% 
  mutate(policy = "Optimal Death")

ap.novax.unif <- ap.novax.unif %>% 
  select(outcome, averted.prop) %>% 
  mutate(policy = "Uniform")

ap.novax <- rbind(ap.novax.infec, ap.novax.symp, ap.novax.hosp, 
                  ap.novax.icu, ap.novax.death, ap.novax.unif) %>% 
  mutate(
    outcome = factor(outcome, levels = c("newI", "newSymp", "newHosp", "newICU", "newDeath")), 
    policy = factor(policy, levels = c("Uniform", "Optimal Infection", "Optimal Symptomatic", "Optimal Hospitalized", "Optimal ICU", "Optimal Death"))
  )

ap.novax$policy <- plyr::revalue(ap.novax$policy, 
                                 c("Optimal Symptomatic" = "Optimal Symp", 
                                   "Optimal Hospitalized" = "Optimal Hosp"))

ap.novax.lp <- ggplot(ap.novax, aes(x = outcome, y = averted.prop * 100)) +
  geom_line(aes(x = as.integer(outcome), y = averted.prop * 100, color = policy), 
            alpha = 1, size = 0.5) + 
  geom_point(aes(color = policy, shape = policy), size = 2) + 
  labs(x = "", y = "Averted proportion (%)") +
  scale_x_discrete(labels = c("Infection", "Symp", "Hosp", "ICU", "Death")) +
  scale_y_continuous(limits = c(60, 100), breaks = seq(60, 100, by = 5), 
                     labels = seq(60, 100, by = 5)) + 
  scale_color_manual(values = c("#FBE4CD", "#CBE3C1", "#A4D4B0",
                                "#77C4BC", "#2CB5B3","#4DA9C7")) + 
  scale_shape_manual(values = c(19, 0, 1, 2, 3, 4)) + 
  theme_classic() +
  theme(axis.line = element_blank()) +
  geom_segment(aes(x = 0.5, xend = 5.5, y = -Inf, yend = -Inf), color = "darkgray") +
  geom_segment(aes(y = 60, yend = 100, x = -Inf, xend = -Inf), color = "darkgray") + 
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) + 
  theme(
    legend.position = c(0.5, 0.92),
    legend.title = element_blank(), 
    legend.background = element_rect(fill = "transparent", colour = "transparent"), 
    legend.text = element_text(size = 11, color = "black"),
    axis.text.x = element_text(size = 13, color = "black"),
    axis.ticks.x = element_line(color = "darkgray"), 
    axis.text.y = element_text(size = 13, color = "black"),
    axis.ticks.y = element_line(color = "darkgray"), 
    axis.title.y = element_text(size = 13, color = "black", face = "bold")
  )
ap.novax.lp

# combine above 6 plots into two rows
prow1 <- plot_grid(tot.p1, 
                   NULL, 
                   tot.p2 + theme(legend.position = "none"), 
                   NULL, 
                   tot.p3 + theme(legend.position = "none"), 
                   nrow = 1, axis = "lr", 
                   rel_widths = c(1, -0.05, 1, -0.05, 1), 
                   labels = c("a", "", "b", "", "c"))
prow1

prow2 <- plot_grid(tot.p4 + theme(legend.position = "none"), 
                   NULL,
                   tot.p5 + theme(legend.position = "none"),
                   NULL, 
                   ap.novax.lp, 
                   nrow = 1, axis = "lr", 
                   rel_widths = c(1, -0.05, 1, -0.05, 1), 
                   labels = c("d", "", "e", "", "f"))
prow2

tot.ap <- plot_grid(prow1, NULL, prow2,
                    ncol = 1, rel_heights = c(1, -0.05, 1))
tot.ap

outfile <- glue("{dout}/Fig. 2.tif")
tiff(outfile, width = 15, height = 10, unit = "in", res = 300, compression = "lzw")
print(tot.ap)
dev.off()