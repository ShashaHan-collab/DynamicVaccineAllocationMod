################################################################################
#### Plot Fig. 1 in main text ####
rm(list = ls())

library(tidyverse)
library(glue)
library(xlsx)
library(cowplot)
library(scales)

setwd("~/DynamicVaccineAllocationMod-main")
dout <- "Source code/output"

# load data
age_gr4.vc <- read.xlsx("Source data/data_Fig. 1.xlsx", sheetIndex = 1)

age_gr4.vc <- age_gr4.vc %>% 
  mutate(age.gr = factor(age.gr, levels = c(
    "Kids (0-14)", "Young (15-39)",
    "Senior (40-64)", "Old (65+)"
  )))

# vaccine coverage of each policy by 4 aggregated age groups ###################
# plot vaccine coverage of optimal infection policy
vc_gr4.opt <- age_gr4.vc %>%
  filter(policy == "Optimal Infection")

# check vaccine coverage of kids
vc_gr4.kids <- vc_gr4.opt %>%
  filter(age.gr == "Kids (0-14)")

vc_gr4.senior <- vc_gr4.opt %>%
  filter(age.gr == "Senior (40-64)")
# day to start optimal allocation
time.opt <- which(vc_gr4.senior$amount == 0)[1] - 1

mark.idx <- c(1, seq(50, 350, by = 50), 400)
mark.vc_gr4.opt <- vc_gr4.opt %>% 
  filter(time %in% mark.idx)

vc_gr4.p1 <- ggplot(vc_gr4.opt) + 
  geom_ribbon(aes(x = time, ymax = coverage * 100), ymin = 0, fill = "#fcedd9", color = NA, 
              alpha = 0.8, data = subset(vc_gr4.opt, age.gr == "Young (15-39)" & time >= time.opt)) + 
  geom_ribbon(aes(x = time, ymax = coverage * 100), ymin = 0, fill = "#dceef2", color = NA, 
              alpha = 0.8, data = subset(vc_gr4.opt, age.gr == "Senior (40-64)" & time >= time.opt)) + 
  geom_ribbon(aes(x = time, ymax = coverage * 100), ymin = 0, fill = "#e3e2e0", color = NA, 
              alpha = 0.8, data = subset(vc_gr4.opt, age.gr == "Kids (0-14)" & time >= time.opt)) + 
  geom_ribbon(aes(x = time, ymax = coverage * 100), ymin = 0, fill = "#f5e0d6", color = NA, 
              alpha = 0.8, data = subset(vc_gr4.opt, age.gr == "Old (65+)" & time >= time.opt)) + 
  geom_line(aes(x = time, y = coverage * 100, color = age.gr), size = 0.5) + 
  geom_point(data = mark.vc_gr4.opt, aes(x = time, y = coverage * 100, color = age.gr, shape = age.gr), 
             size = 2) + 
  labs(x = "Days", y = "Vaccinated proportion (%)", title = "Minimizing infections") +
  scale_x_continuous(breaks = seq(0, 400, by = 50)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  scale_color_manual(values = c(
    "Kids (0-14)" = "#736e67",
    "Young (15-39)" = "#eca146",
    "Senior (40-64)" = "#5db1c5",
    "Old (65+)" = "#d2693a"
  ), 
  labels = c(
    "Kids (0-14)" = "<15",
    "Young (15-39)" = "15-39",
    "Senior (40-64)" = "40-64",
    "Old (65+)" = "65+"), 
  name = "Age (years)") + 
  scale_shape_manual(values = c(0, 1, 2, 4), 
                     labels = c(
                       "Kids (0-14)" = "<15",
                       "Young (15-39)" = "15-39",
                       "Senior (40-64)" = "40-64",
                       "Old (65+)" = "65+"), 
                     name = "Age (years)") + 
  theme_classic() +
  theme(axis.line = element_blank()) +
  geom_segment(aes(x = 1, xend = 400, y = -Inf, yend = -Inf), color = "darkgray") +
  geom_segment(aes(y = 0, yend = 100, x = -Inf, xend = -Inf), color = "darkgray") +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 13, color = "black"), 
    legend.key.width = unit(1, 'cm'), 
    plot.background = element_rect(fill = "transparent", color = NA),
    plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(0, 0, -0.5, 0, unit = "line")), 
    axis.text.x = element_text(size = 13, color = "black"),
    axis.ticks.x = element_line(color = "darkgray"), 
    axis.title.x = element_text(size = 13, color = "black", face = "bold"),
    axis.text.y = element_text(size = 13, color = "black"),
    axis.ticks.y = element_line(color = "darkgray"), 
    axis.title.y = element_text(size = 13, color = "black", face = "bold")
  )
vc_gr4.p1


# plot vaccine coverage of optimal symptomatic policy
vc_gr4.opt <- age_gr4.vc %>%
  filter(policy == "Optimal Symptomatic")

mark.vc_gr4.opt <- vc_gr4.opt %>% 
  filter(time %in% mark.idx)

vc_gr4.p2 <- ggplot(vc_gr4.opt) + 
  geom_ribbon(aes(x = time, ymax = coverage * 100, fill = age.gr), ymin = 0, color = NA, 
              alpha = 0.8, data = subset(vc_gr4.opt, age.gr == "Old (65+)" & time >= time.opt)) + 
  geom_ribbon(aes(x = time, ymax = coverage * 100, fill = age.gr), ymin = 0, color = NA, 
              alpha = 0.8, data = subset(vc_gr4.opt, age.gr == "Senior (40-64)" & time >= time.opt)) + 
  geom_ribbon(aes(x = time, ymax = coverage * 100, fill = age.gr), ymin = 0, color = NA, 
              alpha = 0.8, data = subset(vc_gr4.opt, age.gr == "Young (15-39)" & time >= time.opt)) + 
  geom_ribbon(aes(x = time, ymax = coverage * 100, fill = age.gr), ymin = 0, color = NA, 
              alpha = 0.8, data = subset(vc_gr4.opt, age.gr == "Kids (0-14)" & time >= time.opt)) + 
  geom_line(aes(x = time, y = coverage * 100, color = age.gr), size = 0.5) + 
  geom_point(data = mark.vc_gr4.opt, aes(x = time, y = coverage * 100, color = age.gr, shape = age.gr), 
             size = 2) + 
  labs(x = "Days", y = "Vaccinated proportion (%)", title = "Minimizing symptomatic cases") +
  scale_x_continuous(breaks = seq(0, 400, by = 50)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  scale_color_manual(values = c(
    "Kids (0-14)" = "#736e67",
    "Young (15-39)" = "#eca146",
    "Senior (40-64)" = "#5db1c5",
    "Old (65+)" = "#d2693a"
  ), name = "Age group") + 
  scale_fill_manual(values = c(
    "Kids (0-14)" = "#e3e2e0",
    "Young (15-39)" = "#fcedd9",
    "Senior (40-64)" = "#dceef2",
    "Old (65+)" = "#f5e0d6"
  ), name = "Age group") + 
  scale_shape_manual(values = c(0, 1, 2, 4), 
                     labels = c(
                       "Kids (0-14)" = "<15",
                       "Young (15-39)" = "15-39",
                       "Senior (40-64)" = "40-64",
                       "Old (65+)" = "65+"), 
                     name = "Age (years)") +
  theme_classic() +
  theme(axis.line = element_blank()) +
  geom_segment(aes(x = 0, xend = 400, y = -Inf, yend = -Inf), color = "darkgray") +
  geom_segment(aes(y = 0, yend = 100, x = -Inf, xend = -Inf), color = "darkgray") +
  theme(
    legend.position = c(0.125, 0.85),
    legend.title = element_text(size = 13, color = "black"),
    plot.background = element_rect(fill = "transparent", color = NA),
    plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(0, 0, -0.5, 0, unit = "line")), 
    axis.text.x = element_text(size = 13, color = "black"),
    axis.ticks.x = element_line(color = "darkgray"), 
    axis.title.x = element_text(size = 13, color = "black", face = "bold"),
    axis.text.y = element_text(size = 13, color = "black"),
    axis.ticks.y = element_line(color = "darkgray"), 
    axis.title.y = element_text(size = 13, color = "black", face = "bold")
  )
vc_gr4.p2


# plot vaccine coverage of optimal hospitalized policy
vc_gr4.opt <- age_gr4.vc %>%
  filter(policy == "Optimal Hospitalized")

mark.vc_gr4.opt <- vc_gr4.opt %>% 
  filter(time %in% mark.idx)

vc_gr4.p3 <- ggplot(vc_gr4.opt) + 
  geom_ribbon(aes(x = time, ymax = coverage * 100, fill = age.gr), ymin = 0, color = NA, 
              alpha = 0.8, data = subset(vc_gr4.opt, age.gr == "Old (65+)" & time >= time.opt)) + 
  geom_ribbon(aes(x = time, ymax = coverage * 100, fill = age.gr), ymin = 0, color = NA, 
              alpha = 0.8, data = subset(vc_gr4.opt, age.gr == "Senior (40-64)" & time >= time.opt)) + 
  geom_ribbon(aes(x = time, ymax = coverage * 100, fill = age.gr), ymin = 0, color = NA, 
              alpha = 0.8, data = subset(vc_gr4.opt, age.gr == "Young (15-39)" & time >= time.opt)) + 
  geom_ribbon(aes(x = time, ymax = coverage * 100, fill = age.gr), ymin = 0, color = NA, 
              alpha = 0.8, data = subset(vc_gr4.opt, age.gr == "Kids (0-14)" & time >= time.opt)) + 
  geom_line(aes(x = time, y = coverage * 100, color = age.gr), size = 0.5) + 
  geom_point(data = mark.vc_gr4.opt, aes(x = time, y = coverage * 100, color = age.gr, shape = age.gr), 
             size = 2) + 
  labs(x = "Days", y = "Vaccinated proportion (%)", title = "Minimizing hospitalizations") +
  scale_x_continuous(limits = c(0, 415), breaks = seq(0, 400, by = 50)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  scale_color_manual(values = c(
    "Kids (0-14)" = "#736e67",
    "Young (15-39)" = "#eca146",
    "Senior (40-64)" = "#5db1c5",
    "Old (65+)" = "#d2693a"
  ), name = "Age group") + 
  scale_fill_manual(values = c(
    "Kids (0-14)" = "#e3e2e0",
    "Young (15-39)" = "#fcedd9",
    "Senior (40-64)" = "#dceef2",
    "Old (65+)" = "#f5e0d6"
  ), name = "Age group") + 
  scale_shape_manual(values = c(0, 1, 2, 4), 
                     labels = c(
                       "Kids (0-14)" = "<15",
                       "Young (15-39)" = "15-39",
                       "Senior (40-64)" = "40-64",
                       "Old (65+)" = "65+"), 
                     name = "Age (years)") +
  theme_classic() +
  theme(axis.line = element_blank()) +
  geom_segment(aes(x = 0, xend = 400, y = -Inf, yend = -Inf), color = "darkgray") +
  geom_segment(aes(y = 0, yend = 100, x = -Inf, xend = -Inf), color = "darkgray") +
  theme(
    legend.position = c(0.125, 0.85),
    legend.title = element_text(size = 13, color = "black"),
    plot.background = element_rect(fill = "transparent", color = NA),
    plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(0, 0, -0.5, 0, unit = "line")), 
    axis.text.x = element_text(size = 13, color = "black"),
    axis.ticks.x = element_line(color = "darkgray"), 
    axis.title.x = element_text(size = 13, color = "black", face = "bold"),
    axis.text.y = element_text(size = 13, color = "black"),
    axis.ticks.y = element_line(color = "darkgray"), 
    axis.title.y = element_text(size = 13, color = "black", face = "bold")
  )
vc_gr4.p3


# plot vaccine coverage of optimal ICU policy
vc_gr4.opt <- age_gr4.vc %>%
  filter(policy == "Optimal ICU")

mark.vc_gr4.opt <- vc_gr4.opt %>% 
  filter(time %in% mark.idx)

vc_gr4.p4 <- ggplot(vc_gr4.opt) + 
  geom_ribbon(aes(x = time, ymax = coverage * 100, fill = age.gr), ymin = 0, color = NA, 
              alpha = 0.8, data = subset(vc_gr4.opt, age.gr == "Old (65+)" & time >= time.opt)) + 
  geom_ribbon(aes(x = time, ymax = coverage * 100, fill = age.gr), ymin = 0, color = NA, 
              alpha = 0.8, data = subset(vc_gr4.opt, age.gr == "Senior (40-64)" & time >= time.opt)) + 
  geom_ribbon(aes(x = time, ymax = coverage * 100, fill = age.gr), ymin = 0, color = NA, 
              alpha = 0.8, data = subset(vc_gr4.opt, age.gr == "Young (15-39)" & time >= time.opt)) + 
  geom_ribbon(aes(x = time, ymax = coverage * 100, fill = age.gr), ymin = 0, color = NA, 
              alpha = 0.8, data = subset(vc_gr4.opt, age.gr == "Kids (0-14)" & time >= time.opt)) + 
  geom_line(aes(x = time, y = coverage * 100, color = age.gr), size = 0.5) + 
  geom_point(data = mark.vc_gr4.opt, aes(x = time, y = coverage * 100, color = age.gr, shape = age.gr), 
             size = 2) + 
  labs(x = "Days", y = "Vaccinated proportion (%)", title = "Minimizing ICUs") +
  scale_x_continuous(breaks = seq(0, 400, by = 50)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  scale_color_manual(values = c(
    "Kids (0-14)" = "#736e67",
    "Young (15-39)" = "#eca146",
    "Senior (40-64)" = "#5db1c5",
    "Old (65+)" = "#d2693a"
  ), name = "Age group") + 
  scale_fill_manual(values = c(
    "Kids (0-14)" = "#e3e2e0",
    "Young (15-39)" = "#fcedd9",
    "Senior (40-64)" = "#dceef2",
    "Old (65+)" = "#f5e0d6"
  ), name = "Age group") + 
  scale_shape_manual(values = c(0, 1, 2, 4), 
                     labels = c(
                       "Kids (0-14)" = "<15",
                       "Young (15-39)" = "15-39",
                       "Senior (40-64)" = "40-64",
                       "Old (65+)" = "65+"), 
                     name = "Age (years)") +
  theme_classic() +
  theme(axis.line = element_blank()) +
  geom_segment(aes(x = 0, xend = 400, y = -Inf, yend = -Inf), color = "darkgray") +
  geom_segment(aes(y = 0, yend = 100, x = -Inf, xend = -Inf), color = "darkgray") +
  theme(
    legend.position = c(0.125, 0.85),
    legend.title = element_text(size = 13, color = "black"),
    plot.background = element_rect(fill = "transparent", color = NA),
    plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(0, 0, -0.5, 0, unit = "line")), 
    axis.text.x = element_text(size = 13, color = "black"),
    axis.ticks.x = element_line(color = "darkgray"), 
    axis.title.x = element_text(size = 13, color = "black", face = "bold"),
    axis.text.y = element_text(size = 13, color = "black"),
    axis.ticks.y = element_line(color = "darkgray"), 
    axis.title.y = element_text(size = 13, color = "black", face = "bold")
  )
vc_gr4.p4


# plot vaccine coverage of optimal death policy
vc_gr4.opt <- age_gr4.vc %>%
  filter(policy == "Optimal Death")

mark.vc_gr4.opt <- vc_gr4.opt %>% 
  filter(time %in% mark.idx)

vc_gr4.p5 <- ggplot(vc_gr4.opt) + 
  geom_ribbon(aes(x = time, ymax = coverage * 100, fill = age.gr), ymin = 0, color = NA, 
              alpha = 0.8, data = subset(vc_gr4.opt, age.gr == "Old (65+)" & time >= time.opt)) + 
  geom_ribbon(aes(x = time, ymax = coverage * 100, fill = age.gr), ymin = 0, color = NA, 
              alpha = 0.8, data = subset(vc_gr4.opt, age.gr == "Senior (40-64)" & time >= time.opt)) + 
  geom_ribbon(aes(x = time, ymax = coverage * 100, fill = age.gr), ymin = 0, color = NA, 
              alpha = 0.8, data = subset(vc_gr4.opt, age.gr == "Young (15-39)" & time >= time.opt)) + 
  geom_ribbon(aes(x = time, ymax = coverage * 100, fill = age.gr), ymin = 0, color = NA, 
              alpha = 0.8, data = subset(vc_gr4.opt, age.gr == "Kids (0-14)" & time >= time.opt)) + 
  geom_line(aes(x = time, y = coverage * 100, color = age.gr), size = 0.5) + 
  geom_point(data = mark.vc_gr4.opt, aes(x = time, y = coverage * 100, color = age.gr, shape = age.gr), 
             size = 2) + 
  labs(x = "Days", y = "Vaccinated proportion (%)", title = "Minimizing deaths") + 
  scale_x_continuous(breaks = seq(0, 400, by = 50)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  scale_color_manual(values = c(
    "Kids (0-14)" = "#736e67",
    "Young (15-39)" = "#eca146",
    "Senior (40-64)" = "#5db1c5",
    "Old (65+)" = "#d2693a"
  ), name = "Age group") + 
  scale_fill_manual(values = c(
    "Kids (0-14)" = "#e3e2e0",
    "Young (15-39)" = "#fcedd9",
    "Senior (40-64)" = "#dceef2",
    "Old (65+)" = "#f5e0d6"
  ), name = "Age group") + 
  scale_shape_manual(values = c(0, 1, 2, 4), 
                     labels = c(
                       "Kids (0-14)" = "<15",
                       "Young (15-39)" = "15-39",
                       "Senior (40-64)" = "40-64",
                       "Old (65+)" = "65+"), 
                     name = "Age (years)") +
  theme_classic() +
  theme(axis.line = element_blank()) +
  geom_segment(aes(x = 0, xend = 400, y = -Inf, yend = -Inf), color = "darkgray") +
  geom_segment(aes(y = 0, yend = 100, x = -Inf, xend = -Inf), color = "darkgray") +
  theme(
    legend.position = c(0.125, 0.85),
    legend.title = element_text(size = 13, color = "black"),
    plot.background = element_rect(fill = "transparent", color = NA),
    plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(0, 0, -0.5, 0, unit = "line")), 
    axis.text.x = element_text(size = 13, color = "black"),
    axis.ticks.x = element_line(color = "darkgray"), 
    axis.title.x = element_text(size = 13, color = "black", face = "bold"),
    axis.text.y = element_text(size = 13, color = "black"),
    axis.ticks.y = element_line(color = "darkgray"), 
    axis.title.y = element_text(size = 13, color = "black", face = "bold")
  )
vc_gr4.p5


# plot vaccine coverage of uniform policy
vc_gr4.opt <- age_gr4.vc %>% 
  filter(policy == "Uniform")

mark.vc_gr4.opt <- vc_gr4.opt %>% 
  filter(time %in% mark.idx)

vc_gr4.p6 <- ggplot(vc_gr4.opt) + 
  geom_ribbon(aes(x = time, ymax = coverage * 100, fill = age.gr), ymin = 0, color = NA, 
              alpha = 0.8, data = subset(vc_gr4.opt, time >= time.opt))+
  geom_line(aes(x = time, y = coverage * 100, color = age.gr), size = 0.5) + 
  geom_point(data = mark.vc_gr4.opt, aes(x = time, y = coverage * 100, color = age.gr, shape = age.gr), 
             size = 2) + 
  labs(x = "Days", y = "Vaccinated proportion (%)", title = "Uniform strategy") +
  scale_x_continuous(limits = c(0, 415), breaks = seq(0, 400, by = 50)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  scale_color_manual(values = c(
    "Kids (0-14)" = "#736e67",
    "Young (15-39)" = "#eca146",
    "Senior (40-64)" = "#5db1c5",
    "Old (65+)" = "#d2693a"
  ), name = "Age group") + 
  scale_fill_manual(values = c(
    "Kids (0-14)" = "#e3e2e0",
    "Young (15-39)" = "#fcedd9",
    "Senior (40-64)" = "#dceef2",
    "Old (65+)" = "#f5e0d6"
  ), name = "Age group") + 
  scale_shape_manual(values = c(0, 1, 2, 4), 
                     labels = c(
                       "Kids (0-14)" = "<15",
                       "Young (15-39)" = "15-39",
                       "Senior (40-64)" = "40-64",
                       "Old (65+)" = "65+"), 
                     name = "Age (years)") +
  theme_classic() +
  theme(axis.line = element_blank()) +
  geom_segment(aes(x = 0, xend = 400, y = -Inf, yend = -Inf), color = "darkgray") +
  geom_segment(aes(y = 0, yend = 100, x = -Inf, xend = -Inf), color = "darkgray") +
  theme(
    legend.position = c(0.125, 0.85),
    legend.title = element_text(size = 13, color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(0, 0, -0.5, 0, unit = "line")), 
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.text.x = element_text(size = 13, color = "black"),
    axis.ticks.x = element_line(color = "darkgray"), 
    axis.title.x = element_text(size = 13, color = "black", face = "bold"),
    axis.text.y = element_text(size = 13, color = "black"),
    axis.ticks.y = element_line(color = "darkgray"), 
    axis.title.y = element_text(size = 13, color = "black", face = "bold")
  )
vc_gr4.p6

# combine above 6 plots into one figure
prow <- plot_grid(vc_gr4.p1 + theme(legend.position = "none") + labs(x = ""),
                  NULL,
                  vc_gr4.p2 + theme(legend.position = "none") + labs(x = "", y = ""),
                  NULL,
                  vc_gr4.p3 + theme(legend.position = "none") + labs(x = "", y = ""),
                  NULL,
                  vc_gr4.p4 + theme(legend.position = "none"),
                  NULL,
                  vc_gr4.p5 + theme(legend.position = "none") + labs(y = ""),
                  NULL,
                  vc_gr4.p6 + theme(legend.position = "none") + labs(y = ""), 
                  rel_widths = c(1, -0.05, 1, -0.05, 1, -0.05, 1, -0.05, 1, -0.05, 1), 
                  label_x = c(0.1, 0, 0.1, 0, 0.1, 0, 0.1, 0, 0.1, 0, 0.1), 
                  nrow = 2, labels = c("a", "", "b", "", "c", "", "d", "", "e", "", "f"))
prow

legend <- get_legend(vc_gr4.p1)

vc_gr4.p7 <- plot_grid(legend, NULL, prow, ncol = 1, 
                       rel_heights = c(0.1, -0.02, 1))
vc_gr4.p7

outfile <- glue("{dout}/Fig. 1.tif")
tiff(outfile, width = 15, height = 10, unit = "in", res = 300, compression = "lzw")
print(vc_gr4.p7)
dev.off()
