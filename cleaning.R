##################################################
#              Polya Urns Data Cleaning
#              Laura Caron
#              Columbia University
#         This version: January 21, 2022
##################################################

##################################################
#                    Set up
#           Loads the packages we need
##################################################
#library(renv)
renv::restore()

library(tidyverse)
library(readxl)

##################################################
#                 Import data
# 2021 dataset downloaded 
# from https://ncses.nsf.gov/pubs/nsf21321/downloads
##################################################
bachelor <- read_xlsx("Data/nsf21321-data-tables-tables/nsf21321-tab005-001.xlsx", range="A5:N42")
doctoral <- read_xlsx("Data/nsf21321-data-tables-tables/nsf21321-tab007-001.xlsx", range="A4:L115")

##################################################
#                 Cleaning
##################################################
bachelor$`...1` <- as.numeric(bachelor$`...1`) 
bachelor <- bachelor %>%
  mutate(rownum = row_number(), 
         female = ifelse(rownum <= 25 & rownum >= 15, 1, 0)) %>%
  filter(rownum >= 15) %>%
  filter(!is.na(`...1`)) %>%
  select(-rownum)

colnames(bachelor)[1:3] <- c("Year", "All fields", "S&E")
colnames(bachelor)[13:14] <- c("Engineering", "Non-S&E")

bachelor <- pivot_longer(bachelor, names_to = "Field", cols = colnames(bachelor)[c(-1,-15)])

doctoral <- doctoral %>%
  mutate(rownum = row_number(), 
         female = ifelse(rownum <= 74 & rownum >= 39, 1, 0)) %>%
  filter(rownum >= 39) %>%
  filter(!is.na(`2008`)) %>%
  select(-rownum)

doctoral <- pivot_longer(doctoral, names_to = "Year", cols=colnames(doctoral)[c(-1, -13)])
colnames(doctoral)[1] <- "Field"

##################################################
#               Save new version
##################################################

save(bachelor, file="Data/clean_nsf2021_bachelor.Rdata")
save(doctoral, file="Data/clean_nsf2021_bachelor.Rdata")