# Pete Nelson, PhD
# Department of Water Resources
# project: salmon migration
# purpose: set up for work
# assumes: data acquisition.R
# created: 2022-08-15
# last modified: 2023-07-14

# set up for work ####

library(tidyverse)
library(readxl)
library(gridExtra) # assemble multiple plots on a page

# clear R environment #### 
# except for key files

ls()
rm(list = ls()[!(ls() %in% c("pal_drought", "pal_yrtype",
                            "labels_yrtype", "labels_drought",
                            "yeartypes",
                            "do_water_year", "wy_day_new",
                            "trawls_CHN",
                            "chipps", "chipps_ann"))])

# load critical files: ####

## Chipps Island trawl data ####
## rows=unique trawls w or w/o CHN (ie includes trawls w 0 CHN), 
# per trawl CHN cpue (count/volume), etc
# trawls w no vol data excluded
chipps <- read_csv("data/chipps.csv") # equiv w salmon identified by run-type available (eg "chipps_wr.csv")
chipps_ann <- read_csv("data/chipps_ann.csv") # rows=water year w a50 date, (w)doy, annual drought measures

## labels and colors for figures ####
## labels from drought report
labels_yrtype <- c("Critical", "Dry", "Below Normal", "Above Normal", "Wet")
labels_drought <- c("Dry", "Neutral", "Wet")

## colors from drought report
pal_drought <- c("D" = "#FDE333", "N" = "#53CC67","W" = "#00588B")
pal_yrtype <- c("Critical" = "#FDE333", "Dry" = "#53CC67",
                "Below Normal" = "#009B95","Above Normal" = "#00588B",
                "Wet" = "#4B0055")

# other files -----

# Files that you shouldn't normally need, and may have been stripped:

# raw 'trawls' files
trawls <- read_csv("data/trawl data.csv") # ALL trawl data but not all variables

# this includes adult Chinook (n likely small but unknown) and should be modified...
trawls_CHN <- read_csv("data/trawls_CHN.csv")

yeartypes <- read_csv("data/yearassignments.csv") # should've been joined to dfs above
