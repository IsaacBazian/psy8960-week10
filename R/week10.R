## Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven) #I think this is meant to be in tidyverse, but the functions didn't show up unless I called it directly
library(caret)
set.seed(2112) #Setting a seed so I can get consistent results while running


## Data Import and Cleaning
#This code reads in the data and removes cases where HRS1 is NA
gss_tbl_original <- read_sav(file = "../data/GSS2016.sav") %>% 
  filter(!is.na(HRS1))

#This code identifies the variables in the previous tibble with 75% or more missingness
missing_75 <- colMeans(is.na(gss_tbl_original)) >= .75

#This code subsets the tibble to include only those columns with less than 75% missingness
gss_tbl <- gss_tbl_original[,!missing_75]


## Visualization
#This code makes a histogram of the HRS1 variable with descriptive axes titles
ggplot(gss_tbl, aes(x = HRS1)) +
  geom_histogram() +
  labs(x = "Number of Hours Worked Last Week", y = "Number of Respondents")

## Analysis
#This code randomizes the order of the data, then splits the data into training and testing sets with a 75/25 split
gss_shuffled_tbl <- gss_tbl[sample(nrow(gss_tbl)),]
split75 <- round(nrow(gss_shuffled_tbl) * .75)
gss_train_tbl <- gss_shuffled_tbl[1:split75,]
gss_test_tbl <- gss_shuffled_tbl[(split75 + 1):nrow(gss_shuffled_tbl),]













