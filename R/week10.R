## Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven) #I think this is meant to be in tidyverse, but the functions didn't show up unless I called it directly

## Data Import and Cleaning
#This code reads in the data and removes cases where HRS1 is NA
gss_tbl_original <- read_sav(file = "../data/GSS2016.sav") %>% 
  filter(!is.na(HRS1))

#This code identifies the variables in the previous tibble with 75% or more missingness
missing_75 <- colMeans(is.na(gss_tbl_original)) >= .75

#This code subsets the tibble to include only those columns with less than 75% missingness
gss_tbl <- gss_tbl_original[,!missing_75]
