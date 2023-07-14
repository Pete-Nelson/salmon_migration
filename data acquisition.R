# Pete Nelson, PhD
# Department of Water Resources
# project: salmon migration
# purpose: acquire trawl data from the Delta Juvenile Fish Monitoring Program
# assumes: modified version of edi.244.11.r
# created: 2022-08-15
# last modified: 2023-07-14

library(tidyverse)
library(tsibble) # yearweek()
library(dataRetrieval) # addWaterYear()
library(lubridate) # yday()
library(janitor) 

# day of water year function ----
# used to determine the day of the water year (numeric)
wy_day_new = function(x, start.month = 10L){
  start.yr = year(x) - (month(x) < start.month)
  start.date = make_date(start.yr, start.month, 1L)
  as.integer(x - start.date + 1L)
}

# access year type data ------
# year type assignments based on 
# https://rdrr.io/cran/waterYearType/man/water_year_indices.html
yeartypes <- read_csv("data/yearassignments.csv")

# access Delta Juvenile Fish Monitoring Program data -----

source("edi.244.11.R")
# Use to access data OR access stored data files in the data folder:
dt1 <- read_rds("data/dt1.rds")
dt2 <- read_rds("data/dt2.rds")
dt3 <- read_rds("data/dt3.rds")
dt4 <- read_rds("data/dt4.rds")
dt5 <- read_rds("data/dt5.rds")

ls() 
# remove all but what you need
rm(list = ls()[!(ls() %in% c("dt1", "dt2", "dt3", "dt4", "dt5",
                             "wy_day_new", "yeartypes"))])

# re-name and save copies of the data
write_csv(dt1, "data/1976-2001_DJFMP_trawl_fish_and_water_quality_data.csv")
write_csv(dt2, "data/2002-2021_DJFMP_trawl_fish_and_water_quality_data.csv")

dt1 <- read_csv("data/1976-2001_DJFMP_trawl_fish_and_water_quality_data.csv")
dt2 <- read_csv("data/2002-2021_DJFMP_trawl_fish_and_water_quality_data.csv")

## combine dfs and select variables #####
# not all variables useful (?!)
trawls <- bind_rows(dt1, dt2) %>%
  select(Location, RegionCode, StationCode, SampleDate, 
         MethodCode, GearConditionCode,
         WaterTemp, Turbidity, Secchi, SpecificConductance,
         TowNumber, TowDuration, FlowDebris,
         FlowmeterStart, FlowmeterEnd, FlowmeterDifference, Volume, 
         IEPFishCode, CommonName, MarkCode, StageCode,
         ForkLength, RaceByLength, TagCode, RaceByTag, ArchivalID, GeneticID,
         Probability1, GeneticID2, Probability2, GeneticTest, Count)
# revise ^ to indicate those variables to be EXcluded? maybe easier to evaluate

# many variables need to be modified (eg changed to 'factor')
trawls <- trawls %>% 
  mutate(across(c(1:3, 5:6), factor)) %>% 
  clean_names() # convert variable names to snake_case

# check data w
str(trawls) 
  
# save trawl data, working copy (shortcut)
write_csv(trawls, "data/trawl data.csv")

rm(dt1, dt2) # objects no longer needed

## pivot CHN data ######
# select MWTR records, Chipps Island, and trawls w volume 
# and calculate CPUE (#/volume trawled)
# note: counts include CHN that are not juveniles though the
# number is quite low
# remove volume = NA and keep below 50000
# constrain tow_duration btwn 15 & 22 minutes

dim(trawls %>% filter(iep_fish_code == "CHISAL" & fork_length >= 250))
dim(trawls %>% filter(iep_fish_code == "CHISAL" & fork_length < 250))
dim(trawls %>% filter(iep_fish_code == "CHISAL" & fork_length <= 55))
# CHN >= 250 mm are 0.00095 of the total CHN (465/494,875 2022-04-28)
# CHN <= 55 mm (n=38,934) are 0.079 of the total CHN

dim(trawls) # 1,406,627 rows x 32 columns

## calc cpue ####
# revised to include all location data; filter accordingly in later code!
trawls_CHN <- trawls %>% 
  filter(method_code == "MWTR" & 
           volume != "NA" & # 914560 w NAs removed
           volume < "50000") %>% # 905976 w volume >= 50000 removed
  # remove records of CHN >= 250 mm FL
  filter(!(iep_fish_code == "CHISAL" & fork_length >= 250)) %>% 
  # limit tow_duration to between 15 & 22 minutes
  filter(between(tow_duration, 15, 22)) %>% 
  pivot_wider(id_cols = c(location, station_code, sample_date, tow_number, volume,
                          gear_condition_code, water_temp, turbidity),
              names_from = iep_fish_code,
              values_from = count,
              values_fn = sum,
              values_fill = 0) %>% 
  mutate(year_wk = yearweek(sample_date),
         CPUE = CHISAL/volume) %>% 
  select(-c(RAITRO:YELBUL)) %>% 
  arrange(sample_date, tow_number)

# tow_duration and volume ranges cover some ridiculous extremes but the meta data
# available at EDI provide little helpful information on how to select a reasonable range

write_csv(trawls_CHN, "data/trawls_CHN.csv")

## location-specific data ######
# objective: control for effort
# limit to specific locations (Sherwood, Clarksburg, Chipps & Benicia)
# add doy (day-of-year for calendar & water year), calc cumulative
# catch, cumulative CPUE, and % catch
# and join year type data (ie drought conditions for each year)
# rows now represent weeks, not tows!

### Sherwood Harbor data ----
# no trawls conducted during wy 1990
sherwood <- trawls_CHN %>%
  rename(Date = sample_date) %>% # 'Date' req for addWaterYear()
  addWaterYear() %>%  # new column (water year for each tow)
  rename(date = Date,
         water_yr = waterYear,
         station = station_code,
         temp = water_temp,
         turb = turbidity,
         vol = volume,
         count = CHISAL,
         cpue = CPUE) %>%
  filter(location == "Sherwood Harbor") %>%
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
# add drought variables
sherwood <- left_join(sherwood, yeartypes, # joins annual drought condition to each tow
                      by = c("water_yr" = "year"))
ungroup(sherwood)
# save filtered data (shortcut) to data folder in wd
write_csv(sherwood, "data/sherwood.csv")

#### sherwood A50s -----
# 50% accumulation dates

sherwood_a50 <- sherwood %>% 
  filter(ann_percent >= 0.5) %>% 
  group_by(drought, water_yr) %>% 
  summarise(a50_date = min(date),
            a50_doy= min(doy),
            a50_wdoy = min(wdoy)) %>% 
  ungroup() %>% 
  arrange(water_yr)

sherwood_ann <- left_join(sherwood_a50[,2:5],
                          yeartypes,
                          by = c("water_yr" = "year"))

# re-order year type factor levels to logical sequence
sherwood_ann$yr_type <- 
  factor(sherwood_ann$yr_type,
         levels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet"))

write_csv(sherwood_a50, "data/sherwood_a50.csv")
write_csv(sherwood_ann, "data/sherwood_ann.csv")

### Chipps Island data ----
chipps <- trawls_CHN %>%
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
  filter(location == "Chipps Island" & water_yr < "2022") %>% 
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
# add drought variables
chipps <- left_join(chipps, yeartypes, # joins annual drought condition to each tow
                    by = c("water_yr" = "year"))
ungroup(chipps)
# save filtered data (shortcut) to data folder in wd
write_csv(chipps, "data/chipps.csv")

#### chipps A50s -----
# 50% accumulation dates

chipps_a50 <- chipps %>% 
  filter(ann_percent >= 0.5) %>% 
  group_by(drought, water_yr) %>% 
  summarise(a50_date = min(date),
            a50_doy= min(doy),
            a50_wdoy = min(wdoy)) %>% 
  ungroup() %>% 
  arrange(water_yr)

chipps_ann <- left_join(chipps_a50[,2:5],
                        yeartypes,
                        by = c("water_yr" = "year"))

# re-order year type factor levels to logical sequence
chipps_ann$yr_type <- 
  factor(chipps_ann$yr_type,
         levels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet"))

write_csv(chipps_a50, "data/chipps_a50.csv")
write_csv(chipps_ann, "data/chipps_ann.csv")

### Benicia data ----
# trawls conducted 1999, 2001 and 2008 only; no value!
### Clarksburg data ----
# trawls conducted 1976 and 1977 only, no value

# access genetically determined race ID data ----
# Although...these data look pretty useless!

# Buttermore, E., J. Israel, K. Reece, and S.M. Blankenship. 2021. Chipps Island trawl, Delta Juvenile Fish Monitoring Program, Genetic Determination of Population of Origin 2017-2021 ver 1. Environmental Data Initiative. https://doi.org/10.6073/pasta/f93fed9aa841ffa971aeded3872e0917 (Accessed 2023-07-14).
source("edi.1055.1.r")
rm(infile1, inUrl1)

# note: adds new object (dt1) that is DIFFERENT from previous object w same name!
# shouldn't cause a problem, but be aware...

dt <- as_tibble(dt1) %>% clean_names() %>% arrange(sample_date, genetic_id)
str(dt)
dt %>% group_by(genetic_id) %>% summarise(n = n())

rm(dt1)

summary(dt$sample_date)

## add variables to genetic data #####
# no use to this exercise...
gdat <- dt %>% 
  rename(Date = sample_date) %>% 
  addWaterYear() %>% # new variable
  rename(date = Date,
         water_yr = waterYear,
         fl = fork_length,
         doy = julian,
         gid = genetic_id,
         p = pos_prob,
         ladid = length_by_date,
         fid = field_id) %>% 
  mutate(doy = yday(date),
         wdoy = wy_day_new(date))

gdat %>% group_by(water_yr, gid) %>% summarise(count = n())

## save gdat df to data folder in wd #####
write.csv(gdat, "data/gdat.csv")

# miscellaneous code ####
## labels from Rosie's "Integrated data set.xlsx" ####
labels_yrtype <- c("Critical", "Dry", "Below Normal", "Above Normal", "Wet")
labels_drought <- c("Dry", "Neutral", "Wet")

## colors from drought report ####
pal_drought <- c("D" = "#FDE333", "N" = "#53CC67","W" = "#00588B")
pal_yrtype <- c("Critical" = "#FDE333", "Dry" = "#53CC67",
                "Below Normal" = "#009B95","Above Normal" = "#00588B",
                "Wet" = "#4B0055")

## (w)doy for 2022 #####
wy2022 <- seq(ymd("2021-10-01"), ymd("2022-09-30"), by = "days")
wy2022 <- as_tibble_col(wy2022)
colnames(wy2022) <- "dates"
wy2022 <- wy2022 %>% mutate(doy = yday(dates), wdoy = wy_day_new(dates))

## centrarchids ####
centrarchid_codes <- read_csv("centrarchid_codes.csv")

centrarchids <- pivot_wider(trawls, names_from = iep_fish_code, values_from = count,
                            values_fn = sum, values_fill = 0) %>% 
  select(location, station_code, sample_date, method_code, volume,
         UNIMIC, BLUEGI, BLACRA, UNIPOM, UNICEN, GRESUN, LARBAS,
         PUMPKI, REDBAS,SMABAS, SPOBAS, SACPER, WARMOT, WHICRA) %>% 
  mutate(year_wk = yearweek(sample_date),
         setID = paste(location, station_code, sample_date)) %>% 
  relocate(year_wk, location, station_code, sample_date, setID) %>% 
  arrange(year_wk, location, sample_date)

centrarchids %>% group_by(year_wk) %>% 
  summarise(n = n(),
            count = sum(UNIMIC, BLUEGI, BLACRA, UNIPOM, UNICEN, GRESUN, LARBAS,
                        PUMPKI, REDBAS,SMABAS, SPOBAS, SACPER, WARMOT, WHICRA))

## clear R environment ####
rm(list = ls()[!(ls() %in% c("dt1", "dt2", "dt3", "dt4", "dt5",
                             "dt",
                             "chipps", "chipps_a50", "chipps_ann",
                             "sherwood", "sherwood_a50", "sherwood_ann",
                             "trawls_CHN",
                             "pal_drought", "pal_yrtype",
                             "wy_day_new", "yeartypes"))])

# or for a very clean slate...
rm(list = ls(all.names = TRUE)) 

# SCRATCH PAPER -------------------------------
# files for experimenting w code:
# 76 rows from "1976-2001_DJFMP_trawl_fish_and_water_quality_data.xlsx" w some mods to the data
# "test" has 19 unique tows
test <- read_csv("data/test.csv") 
# simplified version of above w 6 tows, 1 w zero catch
test2 <- read_xlsx("data/test2.xlsx")
test2$count <- as.numeric(test2$count)
# version of test2 where 1 tow has no volume data
test3 <- read_xlsx("data/test2VolwNAs.xlsx")

test_w <- test %>% 
  pivot_wider(id_cols = c(station_code, sample_date, method_code, tow_number, volume),
              names_from = iep_fish_code,
              values_from = count,
              values_fn = sum,
              values_fill = 0) %>% 
  mutate(CPUE = CHISAL/volume) %>% 
  select(-c(AMESHA:THRSTI)) %>%
  arrange(sample_date, tow_number)

test_w <- test2 %>% 
  pivot_wider(id_cols = c(station_code, sample_date, tow_number, volume),
              names_from = iep_fish_code,
              values_from = count,
              values_fn = sum,
              values_fill = 0) %>% 
  mutate(CPUE = CHISAL/volume) %>% 
  arrange(sample_date, tow_number)

test_w <- test3 %>% 
  pivot_wider(id_cols = c(station_code, sample_date, tow_number, volume),
              names_from = iep_fish_code,
              values_from = count,
              values_fn = sum,
              values_fill = 0) %>% 
  mutate(CPUE = CHISAL/volume) %>% 
  arrange(sample_date, tow_number)

test_w <- trawls %>% 
  pivot_wider(id_cols = c(station_code, sample_date, tow_number, volume,
                          method_code, gear_condition_code, water_temp, turbidity,
                          fork_length),
              names_from = iep_fish_code,
              values_from = count,
              values_fn = sum,
              values_fill = 0) %>% 
  mutate(CPUE = CHISAL/volume) %>% 
  select(-c(AMESHA:STRBAS, SPLITT:LEOSHA)) %>% 
  arrange(sample_date, tow_number)

# original code for trawls_CHN
# this does not correctly identify unique tows (no id_cols = )--DON'T USE!
trawls_CHN <- pivot_wider(trawls, names_from = iep_fish_code, values_from = count, 
                         values_fn = sum, values_fill = 0) %>%
  select(location, station_code, sample_date, method_code, water_temp, turbidity, tow_number, 
         tow_duration, volume, fork_length, RaceByLength, CHISAL) %>% 
  mutate(year_wk = yearweek(sample_date),
         CPUE = CHISAL/volume) %>% 
  arrange(sample_date, tow_number)