# Pete Nelson, PhD
# Department of Water Resources
# project: salmon migration
# purpose: explore timing of CCV Chinook juv outmigration, all runs combined
# assumes: data acquisition.R, start your day.R
# created: 2022-08-15
# last modified: 2023-07-14

# short cuts to previously developed data files:
# allows you to generate figures & analyses without all the data massage
chipps <- read_csv("data/chipps.csv") # THE Chipps trawl data for Chinook
chipps_ann <- read_csv("data/chipps_ann.csv") # A50 data for 1976-2020 w year types
yeartypes <- read_csv("data/yearassignments.csv")

# set-up ####
library(tidyverse)
library(ggplot2)
library(gridExtra) # assemble multiple plots on a page

# colors from drought report
pal_drought <- c("D" = "#FDE333", "N" = "#53CC67","W" = "#00588B")
pal_yrtype <- c("Critical" = "#FDE333", "Dry" = "#53CC67", "Below Normal" = "#009B95","Above Normal" = "#00588B", "Wet" = "#4B0055")

# labels from Rosie's "Integrated data set.xlsx"
labels_yrtype <- c("Critical", "Dry", "Below Normal", "Above Normal", "Wet")
labels_drought <- c("Dry", "Neutral", "Wet")

## exploratory figures ######
### accum curves #####
ggplot(chipps, aes(x = wdoy, y = ann_percent, group = water_yr,
                             color = as.factor(water_yr))) +
  geom_line(show.legend = FALSE, size = 1) +
  coord_cartesian(xlim = c(0, 360)) +
  theme_bw() +
  ggtitle("Chinook Salmon Accumulation Curves: Chipps Island Trawl") +
  ylab("Proportion of Annual Chinook Salmon Catch") +
  xlab("Day of Water Year (Oct-Sep)")

ggplot(chipps, aes(x = wdoy, y = ann_percent, group = water_yr,
                             color = as.factor(water_yr))) +
  geom_line(show.legend = TRUE, size = 1) +
  coord_cartesian(xlim = c(0, 360)) +
  theme_bw() +
  ggtitle("Chinook Salmon Accumulation Curves: Chipps Island Trawl") +
  ylab("Proportion of Annual Chinook Salmon Catch") +
  xlab("Day of Water Year (Oct-Sep)")

### basic scatterplot #####
ggplot(chipps_ann, aes(x = water_yr, y = a50_wdoy)) +
  geom_point() +
  geom_smooth(method = lm) # consider method = loess

### A50 1976-2021 ####
fig1 <- ggplot(chipps_ann, aes(x = water_yr, y = a50_wdoy,
                       color = yr_type)) +
  geom_point(size = 6) +
  theme_bw() +
  scale_color_manual(values = pal_yrtype,
                     name = "year type",
                     labels = labels_yrtype) +
  ggtitle("Run Types Combined") +
  xlab("Water Year") +
  ylab("A50 (day of water year)") +
  geom_smooth(aes(x = water_yr, y = a50_wdoy), inherit.aes = FALSE) +
  geom_rug()

fig1

fig1a <- ggplot(chipps_ann, aes(x = water_yr, y = a50_wdoy,
                       color = yr_type)) +
  geom_point(size = 6) +
  theme_bw() +
  scale_color_manual(values = pal_yrtype,
                     name = "year type",
                     labels = labels_yrtype) +
  ggtitle("Run Types Combined") +
  xlab("Water Year") +
  ylab("A50 (day of water year)") +
  geom_smooth(method = lm, se = F, fullrange = F) +
  geom_rug()

fig1a

# split out different year types as facets
fig1a_fac <- fig1a + facet_wrap(~ yr_type, nrow = 1) + 
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ggtitle(NULL) # wo this defaults to "Run Types Combined"

grid.arrange(fig1, fig1a_fac, nrow = 2)

# add color+ to scatter, based on year assignments
ggplot(chipps_ann, aes(x = water_yr, y = a50_wdoy,
                                color = drought)) +
  geom_point(size = 6) +
  theme_bw() +
  scale_color_manual(values = pal_drought,
                     name = "year type",
                     labels = labels_drought) +
  ggtitle("Timing of Chinook Outmigration") +
  xlab("Water Year") +
  ylab("A50 (day of water year)")

ggplot(chipps_ann, aes(x = water_yr, y = a50_wdoy,
                       color = yr_type)) +
  geom_point(size = 6) +
  theme_bw() +
  scale_color_manual(values = pal_yrtype,
                     name = "year type",
                     labels = labels_yrtype) +
  ggtitle("Timing of Chinook Outmigration") +
  xlab("Water Year") +
  ylab("A50 (day of water year)")

ggplot(chipps_ann, aes(x = water_yr, y = a50_wdoy,
                       color = drought)) +
  geom_point(size = 6) +
  theme_bw() +
  scale_color_manual(values = pal_drought,
                     name = "year type",
                     labels = labels_drought) +
  ggtitle("Timing of Chinook Outmigration") +
  xlab("Water Year") +
  ylab("A50 (day of water year)") +
  geom_smooth(method = lm, se = F, fullrange = F) +
  geom_rug()

ggplot(chipps_ann, aes(x = water_yr, y = a50_wdoy,
                       color = yr_type)) +
  geom_point(size = 6) +
  theme_bw() +
  scale_color_manual(values = pal_yrtype,
                     name = "year type",
                     labels = labels_yrtype) +
  ggtitle("Timing of Chinook Outmigration") +
  xlab("Water Year") +
  ylab("A50 (day of water year)") +
  geom_smooth(method = lm, se = F, fullrange = F) +
  geom_rug()

ggplot(chipps_ann, aes(x = water_yr, y = a50_wdoy,
                       color = drought)) +
  geom_point(size = 6) +
  theme_bw() +
  scale_color_manual(values = pal_drought,
                     name = "year type",
                     labels = c("Dry", "Neutral", "Wet")) +
  ggtitle("Timing of Chinook Outmigration") +
  xlab("Water Year") +
  ylab("A50 (day of water year)") +
  geom_smooth(aes(x = water_yr, y = a50_wdoy), inherit.aes = FALSE) +
  geom_rug()

ggplot(chipps_ann, aes(x = water_yr, y = a50_wdoy,
                       color = drought)) +
  geom_point(size = 6) +
  theme_bw() +
  scale_color_manual(values = pal_drought,
                     name = "year type",
                     labels = c("Dry", "Neutral", "Wet")) +
  ggtitle("Timing of Chinook Outmigration") +
  xlab("Water Year") +
  ylab("A50 (day of water year)") +
  geom_smooth(aes(x = water_yr, y = a50_wdoy), method = lm, inherit.aes = FALSE) +
  geom_rug()

ggplot(chipps_ann, aes(x = water_yr, y = a50_wdoy,
                       color = yr_type)) +
  geom_point(size = 6) +
  theme_bw() +
  scale_color_manual(values = pal_yrtype,
                     name = "year type",
                     labels = labels_yrtype) +
  ggtitle("Timing of Chinook Outmigration") +
  xlab("Water Year") +
  ylab("A50 (day of water year)") +
  geom_smooth(aes(x = water_yr, y = a50_wdoy), method = lm, inherit.aes = FALSE) +
  geom_rug()

# miscellaneous code ####
library(gridExtra)
# grid.arrange(fig1, fig2, fig3, fig4)
