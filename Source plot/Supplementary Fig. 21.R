################################################################################
#### Plot Supplementary Fig. 20 ####
rm(list = ls())

library(tidyverse)
library(glue)
library(xlsx)
library(cowplot)
library(reshape2)

setwd("~/DynamicVaccineAllocationMod-main")
dout <- "Source code/output"

hm.palette <- colorRampPalette(c("white", "yellow", "red", "black"), bias = 4)

#### Panel a ###################################################################
# load data
raw <- read.xlsx("Source data/data_Supplementary Fig. 21.xlsx", 
                                   sheetName = "Panel a")
cnt_mat <- raw[, -1] %>% 
  as.matrix()
row.names(cnt_mat) <- raw[, 1]
colnames(cnt_mat) <- raw[, 1]
cm.china.17gr <- list()
cm.china.17gr[[1]] <- cnt_mat

# show the baseline period contact matrix for Shanghai -------------------------
df <- melt(cm.china.17gr, varnames = c("age_participant", "age_contact"), 
           value.name = "contacts")
max <- 10
interval <- 2
p1 <- ggplot(df) + 
  geom_tile(aes(x = age_participant, y = age_contact, fill = pmin(contacts, max))) +
  labs(x = "Age of participant", y = "Age of contact", title = "Pre-pandemic") + 
  scale_fill_gradientn("Contacts", colours = hm.palette(20), 
                       limits = c(0, max), breaks = c(seq(0, max, interval)), 
                       labels = c(0, 2, 4, 6, 8, "10+")) +  
  scale_x_discrete(expand = c(0, 0), labels = c("0-4", "5-9", "10-14", "15-19", 
                                                "20-24", "25-29", "30-34", "35-39", 
                                                "40-44", "45-49", "50-54", "55-59", 
                                                "60-64", "65-69", "70-74", "75-79", "80+")) + 
  scale_y_discrete(expand = c(0, 0), labels = c("0-4", "5-9", "10-14", "15-19", 
                                                "20-24", "25-29", "30-34", "35-39", 
                                                "40-44", "45-49", "50-54", "55-59", 
                                                "60-64", "65-69", "70-74", "75-79", "80+")) + 
  guides(fill = guide_colorbar(draw.ulim = TRUE, draw.llim = TRUE, 
                               frame.colour = NA, ticks.colour = "darkgray"))+
  coord_equal() + 
  theme(panel.border = element_rect(colour = "darkgray", fill = NA, size = 0.5), 
        plot.title = element_text(hjust = 0.5), 
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.ticks.x = element_line(color = "darkgray"), 
        axis.ticks.y = element_line(color = "darkgray"), 
        axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, size = 6),
        axis.text.y = element_text(size = 6),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))
p1


#### Panel b ###################################################################
# load data
raw <- read.xlsx("Source data/data_Supplementary Fig. 21.xlsx", 
                 sheetName = "Panel b")
cnt_mat <- raw[, -1] %>% 
  as.matrix()
row.names(cnt_mat) <- raw[, 1]
colnames(cnt_mat) <- raw[, 1]
cm.china.17gr <- list()
cm.china.17gr[[1]] <- cnt_mat

# show the postlockdown contact matrix for Shanghai ----------------------------
df <- melt(cm.china.17gr, varnames = c("age_participant", "age_contact"), 
           value.name = "contacts")
max <- 10
interval <- 2
p2 <- ggplot(df) + 
  geom_tile(aes(x = age_participant, y = age_contact, fill = pmin(contacts, max))) +
  labs(x = "Age of participant", y = "Age of contact", title = "Pandemic") +
  # geom_tile(aes(y = age_participant, x = age_contact, fill = pmin(contacts, max))) + 
  # labs(y = "Age of participant", x = "Age of contact", title = "School-closing") + 
  scale_fill_gradientn("Contacts", colours = hm.palette(20), 
                       limits = c(0, max), breaks = c(seq(0, max, interval)), 
                       labels = c(0, 2, 4, 6, 8, "10+")) +  
  scale_x_discrete(expand = c(0, 0), labels = c("0-4", "5-9", "10-14", "15-19", 
                                                "20-24", "25-29", "30-34", "35-39", 
                                                "40-44", "45-49", "50-54", "55-59", 
                                                "60-64", "65-69", "70-74", "75-79", "80+")) + 
  scale_y_discrete(expand = c(0, 0), labels = c("0-4", "5-9", "10-14", "15-19", 
                                                "20-24", "25-29", "30-34", "35-39", 
                                                "40-44", "45-49", "50-54", "55-59", 
                                                "60-64", "65-69", "70-74", "75-79", "80+")) + 
  guides(fill = guide_colorbar(draw.ulim = TRUE, draw.llim = TRUE, 
                               frame.colour = NA, ticks.colour = "darkgray"))+
  coord_equal() + 
  theme(panel.border = element_rect(colour = "darkgray", fill = NA, size = 0.5), 
        plot.title = element_text(hjust = 0.5), 
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.ticks.x = element_line(color = "darkgray"), 
        axis.ticks.y = element_line(color = "darkgray"), 
        axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, size = 6),
        axis.text.y = element_text(size = 6),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))
p2


#### Panel c ###################################################################
# load data
raw <- read.xlsx("Source data/data_Supplementary Fig. 21.xlsx", 
                 sheetName = "Panel c")
cnt_mat <- raw[, -1] %>% 
  as.matrix()
row.names(cnt_mat) <- raw[, 1]
colnames(cnt_mat) <- raw[, 1]
cm.china.17gr <- list()
cm.china.17gr[[1]] <- cnt_mat

# show the adjusted contact matrix for Shanghai --------------------------------
df <- melt(cm.china.17gr, varnames = c("age_participant", "age_contact"), 
           value.name = "contacts")
max <- 10
interval <- 2
p3 <- ggplot(df) + 
  geom_tile(aes(x = age_participant, y = age_contact, fill = pmin(contacts, max))) +
  labs(x = "Age of participant", y = "Age of contact", title = "Infectiousness-adjusted") + 
  scale_fill_gradientn("Contacts", colours = hm.palette(20), 
                       limits = c(0, max), breaks = c(seq(0, max, interval)), 
                       labels = c(0, 2, 4, 6, 8, "10+")) +  
  scale_x_discrete(expand = c(0, 0), labels = c("0-4", "5-9", "10-14", "15-19", 
                                                "20-24", "25-29", "30-34", "35-39", 
                                                "40-44", "45-49", "50-54", "55-59", 
                                                "60-64", "65-69", "70-74", "75-79", "80+")) + 
  scale_y_discrete(expand = c(0, 0), labels = c("0-4", "5-9", "10-14", "15-19", 
                                                "20-24", "25-29", "30-34", "35-39", 
                                                "40-44", "45-49", "50-54", "55-59", 
                                                "60-64", "65-69", "70-74", "75-79", "80+")) + 
  guides(fill = guide_colorbar(draw.ulim = TRUE, draw.llim = TRUE, 
                               frame.colour = NA, ticks.colour = "darkgray"))+
  coord_equal() + 
  theme(panel.border = element_rect(colour = "darkgray", fill = NA, size = 0.5), 
        plot.title = element_text(hjust = 0.5), 
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.ticks.x = element_line(color = "darkgray"), 
        axis.ticks.y = element_line(color = "darkgray"), 
        axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, size = 6),
        axis.text.y = element_text(size = 6),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))
p3

# combine above three contact matrix plots into one figure
prow <- plot_grid(p1 + theme(legend.position = "none"), 
                  NULL, 
                  p2 + theme(legend.position = "none") + labs(y = ""),
                  NULL,
                  p3 + theme(legend.position = "none") + labs(y = ""),
                  NULL,
                  nrow = 1, axis = "l", 
                  rel_widths = c(1, -0.08, 1, -0.08, 1, 0.25), 
                  labels = c("a", "", "b", "", "c", ""), 
                  label_x = c(0.12, 0, 0.12, 0, 0.12, 0), 
                  label_y = c(0.18, 0, 0.18, 0, 0.18, 0))
prow

# now add in the legend
legend <- get_legend(p3)
pcom <- prow + 
  draw_grob(legend, 0.71, 0.04, 0.5, 1)

outfile <- glue("{dout}/Supplementary Fig. 21.tif")
tiff(outfile, width = 8.5, height = 3, unit = "in", res = 300, compression = "lzw")
print(pcom)
dev.off()
