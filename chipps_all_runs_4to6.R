# Pete Nelson, PhD
# Department of Water Resources
# project: salmon migration
# purpose: explore timing of CCV Chinook juv outmigration, all runs combined
# assumes: data acquisition.R, start your day.R
# created: 2022-08-15
# last modified: 2023-07-14

# purpose: explore timing of CCV Chinook juv outmigration, all runs combined, but limited to data from Apr-Jun

# access data ####

yeartypes <- read_csv("data/yearassignments.csv")
trawls_CHN <- read_csv("data/trawls_CHN.csv") 

# load libraries ####
library(dataRetrieval) # addWaterYear()
library(tidyverse)
library(lubridate)
library(ggplot2)
library(gridExtra)

# limit to April-June #####
# limit tows to April through June, inclusive
# naming convention "tXXXXX" refers to fact that data are truncated, limiting
# obs to Apr-Jun
ttrawls_CHN <- trawls_CHN %>% 
  filter(month(sample_date) >= "4" & month(sample_date) <= "6")

# fun() day of water year ####
# used to determine the day of the water year (numeric)
wy_day_new = function(x, start.month = 10L){
  start.yr = year(x) - (month(x) < start.month)
  start.date = make_date(start.yr, start.month, 1L)
  as.integer(x - start.date + 1L)
}

# calc cum totals ####
# and cpue
tchipps <- ttrawls_CHN %>%
  rename(Date = sample_date) %>% # req to addWaterYear()
  addWaterYear() %>%  # new column (water year for each tow)
  rename(date = Date, 
         water_yr = waterYear,
         station = station_code,
         temp = water_temp,
         turb = turbidity,
         vol = volume,
         count = CHISAL,
         cpue = CPUE) %>% 
  group_by(water_yr) %>% # allows for cumulative stats
  mutate(doy = yday(date), # day of each Julian year
         wdoy = wy_day_new(date), # day of each water year
         ann_catch = cumsum(count), # cumulative catch per annum
         ann_cpue = cumsum(cpue), # cumulative cpue per annum
         ann_percent = ann_catch/max(ann_catch, # percent of total annual catch
                                     na.rm = TRUE)) %>% 
  group_by(year_wk) %>% # group by the week of the year
  mutate(wk_catch = cumsum(count), # cumulative catch per week
         wk_cpue = cumsum(cpue), # cumulative cpue per week
         wk_percent = wk_catch/max(wk_catch, # percent of total weekly catch
                                   na.rm = TRUE)) %>% 
  select(station, water_yr, year_wk, date, doy, wdoy,
         temp, turb, vol, count, cpue,
         ann_catch, ann_cpue, ann_percent,
         wk_catch, wk_cpue, wk_percent) # select desired variables (columns)

tchipps <- left_join(tchipps, yeartypes, # joins annual drought condition to each tow
                    by = c("water_yr" = "year"))
ungroup(tchipps)

# identify a50s ####
# (day of 50% annual accumulation)
# and the dates and day-of-year (numeric) 
tchipps_a50 <- tchipps %>% 
  filter(ann_percent >= 0.5) %>% 
  group_by(drought, water_yr) %>% 
  summarise(a50_date = min(date),
            a50_doy= min(doy),
            a50_wdoy = min(wdoy)) %>% 
  ungroup() %>% 
  arrange(water_yr)

## add year type ####
tchipps_ann <- left_join(tchipps_a50[,2:5],
                        yeartypes,
                        by = c("water_yr" = "year"))

# re-order year type factor levels to logical sequence
tchipps_ann$yr_type <- 
  factor(tchipps_ann$yr_type,
         levels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet"))

rm(chipps, 
   fig1, fig1a, fig1a_fac,
   trawls_CHN, ttrawls_CHN,
   wy_day_new,
   yeartypes)

# quick comparison
tail(chipps_ann, 10)
tail(tchipps_ann, 10)

# exploratory figures ######
# colors from drought report
pal_drought <- c("D" = "#FDE333", "N" = "#53CC67","W" = "#00588B")
pal_yrtype <- c("Critical" = "#FDE333", "Dry" = "#53CC67", "Below Normal" = "#009B95","Above Normal" = "#00588B", "Wet" = "#4B0055")

# labels from Rosie's "Integrated data set.xlsx"
labels_yrtype <- c("Critical", "Dry", "Below Normal", "Above Normal", "Wet")
labels_drought <- c("Dry", "Neutral", "Wet")

## accum curves #####
ggplot(tchipps, aes(x = wdoy, y = ann_percent, group = water_yr,
                             color = as.factor(water_yr))) +
  geom_line(show.legend = FALSE, size = 1) +
  coord_cartesian(xlim = c(0, 360)) +
  theme_bw() +
  ggtitle("Chinook Salmon Accumulation Curves: Chipps Island Trawl",
          subtitle = "April - June tows only") +
  ylab("Proportion of Annual Chinook Salmon Catch") +
  xlab("Day of Water Year (Oct-Sep)")

ggplot(tchipps, aes(x = wdoy, y = ann_percent, group = water_yr,
                             color = as.factor(water_yr))) +
  geom_line(show.legend = TRUE, size = 1) +
  coord_cartesian(xlim = c(0, 360)) +
  theme_bw() +
  ggtitle("Chinook Salmon Accumulation Curves: Chipps Island Trawl",
          subtitle = "April - June tows only") +
  ylab("Proportion of Annual Chinook Salmon Catch") +
  xlab("Day of Water Year (Oct-Sep)")

## basic scatterplot #####
ggplot(tchipps_ann, aes(x = water_yr, y = a50_wdoy)) +
  geom_point() +
  geom_smooth(method = lm) # consider method = loess

# compare the a50s in the full vs Apr-Jun data sets
comp_chipps_ann <- left_join(chipps_ann,
                             tchipps_ann[,c(1,4)],
                             by = "water_yr") %>% 
  rename(a50_wdoy = a50_wdoy.x) %>% 
  relocate(a50_wdoy.y, .after = a50_wdoy) %>% 
  rename(ta50_wdoy = a50_wdoy.y)

ggplot(comp_chipps_ann, aes(x = water_yr, y = value)) +
  geom_point(aes(y = a50_wdoy, col = "full"), shape = 1) +
    geom_point(aes(y = ta50_wdoy, col = "Apr-Jun"), shape = 3) +
  xlab("Water Year") + ylab("A50 (Day of the Water-Year)") +
  ggtitle("Chinook Salmon Peak Outmigration") +
  theme_bw() +
  scale_color_manual(values = c("full" = "black", "Apr-Jun" = "red"),
                     name = "sampling")

## A50 1976-2021 ####
fig1t <- ggplot(tchipps_ann, aes(x = water_yr, y = a50_wdoy,
                       color = yr_type)) +
  geom_point(size = 6) +
  theme_bw() +
  scale_color_manual(values = pal_yrtype,
                     name = "year type",
                     labels = labels_yrtype) +
  ggtitle("Run Types Combined",
          subtitle = "April - June tows only") +
  xlab("Water Year") +
  ylab("A50 (day of water year)") +
  geom_smooth(aes(x = water_yr, y = a50_wdoy), inherit.aes = FALSE) +
  geom_rug()

fig1t

fig1at <- ggplot(tchipps_ann, aes(x = water_yr, y = a50_wdoy,
                       color = yr_type)) +
  geom_point(size = 6) +
  theme_bw() +
  scale_color_manual(values = pal_yrtype,
                     name = "year type",
                     labels = labels_yrtype) +
  ggtitle("Run Types Combined",
          subtitle = "April - June tows only") +
  xlab("Water Year") +
  ylab("A50 (day of water year)") +
  geom_smooth(method = lm, se = F, fullrange = F) +
  geom_rug()

fig1at

# split out different year types as facets
fig1at_fac <- fig1at + facet_wrap(~ yr_type, nrow = 1) + 
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ggtitle(NULL) # wo this defaults to "Run Types Combined"

grid.arrange(fig1t, fig1at_fac, nrow = 2)

# add color+ to scatter, based on year assignments
ggplot(tchipps_ann, aes(x = water_yr, y = a50_wdoy,
                                color = drought)) +
  geom_point(size = 6) +
  theme_bw() +
  scale_color_manual(values = pal_drought,
                     name = "year type",
                     labels = labels_drought) +
  ggtitle("Timing of Chinook Outmigration",
          subtitle = "April - June tows only") +
  xlab("Water Year") +
  ylab("A50 (day of water year)")

ggplot(tchipps_ann, aes(x = water_yr, y = a50_wdoy,
                       color = yr_type)) +
  geom_point(size = 6) +
  theme_bw() +
  scale_color_manual(values = pal_yrtype,
                     name = "year type",
                     labels = labels_yrtype) +
  ggtitle("Timing of Chinook Outmigration",
          subtitle = "April - June tows only") +
  xlab("Water Year") +
  ylab("A50 (day of water year)")

ggplot(tchipps_ann, aes(x = water_yr, y = a50_wdoy,
                       color = drought)) +
  geom_point(size = 6) +
  theme_bw() +
  scale_color_manual(values = pal_drought,
                     name = "year type",
                     labels = labels_drought) +
  ggtitle("Timing of Chinook Outmigration",
          subtitle = "April - June tows only") +
  xlab("Water Year") +
  ylab("A50 (day of water year)") +
  geom_smooth(method = lm, se = F, fullrange = F) +
  geom_rug()

ggplot(tchipps_ann, aes(x = water_yr, y = a50_wdoy,
                       color = yr_type)) +
  geom_point(size = 6) +
  theme_bw() +
  scale_color_manual(values = pal_yrtype,
                     name = "year type",
                     labels = labels_yrtype) +
  ggtitle("Timing of Chinook Outmigration",
          subtitle = "April - June tows only") +
  xlab("Water Year") +
  ylab("A50 (day of water year)") +
  geom_smooth(method = lm, se = F, fullrange = F) +
  geom_rug()

ggplot(tchipps_ann, aes(x = water_yr, y = a50_wdoy,
                       color = drought)) +
  geom_point(size = 6) +
  theme_bw() +
  scale_color_manual(values = pal_drought,
                     name = "year type",
                     labels = c("Dry", "Neutral", "Wet")) +
  ggtitle("Timing of Chinook Outmigration",
          subtitle = "April - June tows only") +
  xlab("Water Year") +
  ylab("A50 (day of water year)") +
  geom_smooth(aes(x = water_yr, y = a50_wdoy), inherit.aes = FALSE) +
  geom_rug()

ggplot(tchipps_ann, aes(x = water_yr, y = a50_wdoy,
                       color = drought)) +
  geom_point(size = 6) +
  theme_bw() +
  scale_color_manual(values = pal_drought,
                     name = "year type",
                     labels = c("Dry", "Neutral", "Wet")) +
  ggtitle("Timing of Chinook Outmigration",
          subtitle = "April - June tows only") +
  xlab("Water Year") +
  ylab("A50 (day of water year)") +
  geom_smooth(aes(x = water_yr, y = a50_wdoy), method = lm, inherit.aes = FALSE) +
  geom_rug()

ggplot(tchipps_ann, aes(x = water_yr, y = a50_wdoy,
                       color = yr_type)) +
  geom_point(size = 6) +
  theme_bw() +
  scale_color_manual(values = pal_yrtype,
                     name = "year type",
                     labels = labels_yrtype) +
  ggtitle("Timing of Chinook Outmigration",
          subtitle = "April - June tows only") +
  xlab("Water Year") +
  ylab("A50 (day of water year)") +
  geom_smooth(aes(x = water_yr, y = a50_wdoy), method = lm, inherit.aes = FALSE) +
  geom_rug()

# miscellaneous code ####
library(gridExtra)
# grid.arrange(fig1, fig2, fig3, fig4)
