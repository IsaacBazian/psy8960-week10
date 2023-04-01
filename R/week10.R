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
  tuneLength = 3
  #tuneGrid = expand.grid(mtry = c(2, 10, 50, 100, 200), splitrule = c("variance", "extratrees"), min.node.size = 5) #This seems to run a little faster
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
# This code makes a tibble. I hard code in the names of the models, code in the 
# k-fold CV R^2 for each model, and dynamically generate the holdout CV R^2 for
# each model.
table1_tbl <- tibble(
  algo = c("OLS Regression", "Elastic Net", "Random Forest", "eXtreme Gradient Boosting"),
  cv_rsq = c(".68", ".72", ".91", ".95"),
  ho_rsq = c(
    str_remove(format(round(cor(predict(modelOLS, gss_test_tbl, na.action = na.pass), gss_test_tbl$workhours)^2, 2), nsmall = 2), pattern = "^0"),
    str_remove(format(round(cor(predict(modelElasticNet, gss_test_tbl, na.action = na.pass), gss_test_tbl$workhours)^2, 2), nsmall = 2), pattern = "^0"),
    str_remove(format(round(cor(predict(modelRandomForest, gss_test_tbl, na.action = na.pass), gss_test_tbl$workhours)^2, 2), nsmall = 2), pattern = "^0"),
    str_remove(format(round(cor(predict(modelXGB, gss_test_tbl, na.action = na.pass), gss_test_tbl$workhours)^2, 2), nsmall = 2), pattern = "^0")
  )
) #Need to figure out how to dynamically get cv_rsq, rather than calling each model, finding the Rsquared of the selected model, and hardcoding


# Q1: Results did change considerably between models, such that cv_rsq differed 
# between models: OLS < Elastic Net < Random Forest < XGB. ho_rsq also changed
# between models: OLS < Elastic Net < XGB < Random Forest. This happens because
# each of the algorithms make predictions with different underlying models -
# OLS, being the simplest, produces the least impressive results, while more
# sophisticated algorithms like Random Forest consider more things about the data
# to produce better predictions.

# Q2: Holdout CV was lower than k-fold CV across algorithms. This makes sense -
# the models will, to a certain extent, be optimized to the peculiarities
# of the training data, generally leading to overfitting. When we apply the
# models to new data, they appear to perform less well, as they are no longer
# taking advantage of the chance patterns in the training data.

# Q3: Of these four models, I would generally use Random Forest. In this data,
# it produced the highest ho_rsq, which is what we actually care about for
# making predictions with new data. However, there are some trade-offs: compared
# to OLS and Elastic Net, the Random Forest model took much longer to train. On
# a similar note, it is actually possible that XGB could produce even higher
# ho_rsq, but this would also be at greater computational cost - I originally
# tried running the XGB models with xgbDart, but due to the high number of
# hyperparameters, it was taking an extremely long time, so I elected to use
# xgbLinear instead, due to the lower number of hyperparameters. If I used
# xgbDART or xgbTree, maybe I could get better predictions, but using such
# computationally expensive models was impractical in this case. Therefore,
# a Random Forest model appears to give relatively good prediction without
# taking an unreasonable amount of time to train. If time/computing power was
# less of a concern, I would see if other kinds of XGB gave better predictions
# and, if so, would consider using that model instead.





