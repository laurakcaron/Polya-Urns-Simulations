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
library(openxlsx)

##################################################
#                 Import data
# 2021 dataset downloaded 
# from https://ncses.nsf.gov/pubs/nsf21321/downloads
##################################################
bachelor <- read.xlsx("Data/nsf21321-data-tables-tables/nsf21321-tab005-001.xlsx", startRow = 3)
colnames(bachelor) <- c("Year", "All fields", "S&E", "All sciences",	"Agricultural sciences",	"Biological sciences",	"Computer sciences",	"Earth, atmospheric, and ocean sciences",	"Mathematics and statistics",	"Physical sciences",	"Psychology",	"Social sciences", "Engineering", "Non-S&E")
doctoral <- read.xlsx("Data/nsf21321-data-tables-tables/nsf21321-tab007-001.xlsx", startRow=4)

cswep <- read.xlsx("Data/ICPSR_37118/DS0003/37118-0003-Zipped_package/Table1_clean.xlsx", sheet="cleaned")

##################################################
#                 Cleaning
##################################################
bachelor <- bachelor %>%
  mutate(rownum = row_number(), 
         female = ifelse(rownum <= 22 & rownum >= 12, 1, 0)) %>%
  filter(rownum >= 12) %>%
  select(-rownum)

bachelor <- pivot_longer(bachelor, names_to = "Field", cols = colnames(bachelor)[c(-1,-15)])

doctoral <- doctoral %>%
  mutate(rownum = row_number(), 
         female = ifelse(rownum <= 74 & rownum >= 39, 1, 0)) %>%
  filter(rownum >= 39) %>%
  filter(!is.na(`2008`)) %>%
  select(-rownum)

doctoral <- pivot_longer(doctoral, names_to = "Year", cols=colnames(doctoral)[c(-1, -13)])
doctoral$Year <- as.numeric(doctoral$Year)
colnames(doctoral)[1] <- "Field"

cswep <- cswep %>%
  mutate(rownum = row_number(), 
         female = ifelse(rownum <= 12 & rownum >= 2, 1, 0)) %>%
  filter(rownum >= 2) %>%
  select(-rownum)

cswep[,2:29] <- sapply(cswep[,2:29], as.numeric) %>% as.data.frame()

cswep <- pivot_longer(cswep, names_to = "Year", cols=colnames(cswep)[c(-1, -29)])
colnames(cswep)[1] <- "Rank"

##################################################
#               Save new version
##################################################

save(bachelor, file="Data/clean_nsf2021_bachelor.Rdata")
save(doctoral, file="Data/clean_nsf2021_doctoral.Rdata")
save(cswep, file="Data/clean_cswep.Rdata")