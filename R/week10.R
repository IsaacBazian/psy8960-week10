## Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven) #I think this is meant to be in tidyverse, but the functions didn't show up unless I called it directly

## Data Import and Cleaning
gss_tbl <- read_sav(file = "../data/GSS2016.sav")
