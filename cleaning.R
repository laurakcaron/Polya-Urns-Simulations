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
data <- read_xlsx("Data/nsf21321-data-tables-tables/nsf21321-tab005-001.xlsx", range="A5:N42")

##################################################
#                 Cleaning
##################################################
data$`...1` <- as.numeric(data$`...1`) 
data <- data %>%
  mutate(rownum = row_number(), 
         female = ifelse(rownum <= 25 & rownum >= 15, 1, 0)) %>%
  filter(rownum >= 15) %>%
  filter(!is.na(`...1`))

colnames(data)[1:3] <- c("Year", "All fields", "SE")
colnames(data)[13:14] <- c("Engineering", "NonSE")

##################################################
#               Save new version
##################################################

save(data, file="Data/clean_nsf2021_bachelor")