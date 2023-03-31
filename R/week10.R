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

#This code subsets the tibble to include only those columns with less than 75% missingness,
#makes all variables numeric (which fixes an error I got before, "wrong model for classification"),
#makes it a tibble again and renames HRS1 to workhours
gss_tbl <- gss_tbl_original[,!missing_75] %>% 
  sapply(as.numeric) %>% 
  as_tibble() %>% 
  rename(workhours = HRS1)

## Visualization
#This code makes a histogram of the workhours variable with descriptive axes titles
ggplot(gss_tbl, aes(x = workhours)) +
  geom_histogram() +
  labs(x = "Number of Hours Worked Last Week", y = "Number of Respondents")

## Analysis
#This code randomizes the order of the data, then splits the data into training and testing sets with a 75/25 split, and creates training folds
gss_shuffled_tbl <- gss_tbl[sample(nrow(gss_tbl)),]
split75 <- round(nrow(gss_shuffled_tbl) * .75)
gss_train_tbl <- gss_shuffled_tbl[1:split75,]
gss_test_tbl <- gss_shuffled_tbl[(split75 + 1):nrow(gss_shuffled_tbl),]
training_folds <- createFolds(gss_train_tbl$workhours, 10)


# This code fits a model predicting workhours from all other variables using OLS regression
modelOLS <- train(
  workhours ~ .,
  gss_train_tbl,
  method = "lm",
  na.action = na.pass,
  preProcess = "medianImpute",
  trControl = trainControl(method="cv", indexOut = training_folds, number = 10, search = "grid", verboseIter=T)
)

# This code fits a model predicting workhours from all other variables using elastic net
modelElasticNet <- train(
  workhours ~ .,
  gss_train_tbl,
  method = "glmnet",
  na.action = na.pass,
  preProcess = "medianImpute",
  trControl = trainControl(method="cv", indexOut = training_folds, number = 10, search = "grid", verboseIter=T)
)

# This code fits a model predicting workhours from all other variables using a random forest
modelRandomForest <- train(
  workhours ~ .,
  gss_train_tbl,
  method = "ranger",
  na.action = na.pass,
  preProcess = "medianImpute",
  trControl = trainControl(method="cv", indexOut = training_folds, number = 10, search = "grid", verboseIter=T),
  tuneGrid = expand.grid(mtry = c(2, 10, 50, 100, 200), splitrule = c("variance", "extratrees"), min.node.size = 5) #This seems to run a little faster
)

# This code fits a model predicting workhours from all other variables using extreme gradient boosting
modelXGB <- train(
  workhours ~ .,
  gss_train_tbl,
  method = "xgbLinear",
  na.action = na.pass,
  preProcess = "medianImpute",
  trControl = trainControl(method="cv", indexOut = training_folds, number = 10, search = "grid", verboseIter=T),
  tuneLength = 3 #Still working on making this not take so long
)


## Publication
table1_tbl <- tibble(
  algo = c("OLS Regression", "Elastic Net", "Random Forest", "eXtreme Gradient Boosting"),
  cv_rsq = c("", "", "", ""),
  ho_rsq = c("", "", "", "")
)

modelOLS$results$Rsquared
modelElasticNet$results$Rsquared
cor(predict(modelOLS, gss_test_tbl, na.action = na.pass), gss_test_tbl$workhours)^2
cor(predict(modelElasticNet, gss_test_tbl, na.action = na.pass), gss_test_tbl$workhours)^2







