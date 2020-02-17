#### Using Extreme Gradient Boosting on the merged dataset to predict the progression of the CKD patients ###

rm(list =ls())
library(caret)
library(randomForest)
library(dplyr)
library(plyr)
library(pROC)
library(tidyr)
library(xgboost)
setwd("/Users/srinath/Downloads/dataScienceTask/Holomusk_Data_Scientist/")

## Loading the dataset
load("merged_data.RData")

model_data = merged_data %>% select(-c(id, drugs))

### Changing Race to numeric
levels(model_data$race)[levels(model_data$race) == "Asian"] = 1
levels(model_data$race)[levels(model_data$race) == "Black"] = 2
levels(model_data$race)[levels(model_data$race) == "Hispanic"] = 3
levels(model_data$race)[levels(model_data$race) == "Unknown"] = 4
levels(model_data$race)[levels(model_data$race) == "White"] = 5

model_data$race = as.numeric(model_data$race)

### Changing gender to numeric
levels(model_data$gender)[levels(model_data$gender) == "Male"] = 1
levels(model_data$gender)[levels(model_data$gender) == "Female"] = 2

model_data$gender = as.numeric(model_data$gender)

### Changing Stage to numeric
levels(model_data$Stage_Progress)[levels(model_data$Stage_Progress) == "True"] = 1
levels(model_data$Stage_Progress)[levels(model_data$Stage_Progress) == "False"] = 0

### Splitting the dataset
set.seed(20)
dat = createDataPartition(model_data$Stage_Progress, p = 0.7, list = FALSE)
train_data = model_data[dat,]
labels_train = data.frame(factor(train_data$Stage_Progress))
train_data$Stage_Progress = NULL

test_data = model_data[-dat,]
labels_test = factor(test_data$Stage_Progress)
test_data$Stage_Progress = NULL

dtrain = xgb.DMatrix(as.matrix(train_data), label = as.matrix(labels_train))
dtest = xgb.DMatrix(as.matrix(test_data), label = as.matrix(data.frame(labels_test)))
watchlist <- list(eval = dtest, train = dtrain)
neg_pos_wt = table(model_data$Stage_Progress)
param <- list(booster = "gbtree", max_depth = 6, eta = 0.01, gamma = 0, min_child_weight = 1.5, colsample_bytree = 0.6,
              objective = "binary:logistic", eval_metric = "auc", subsample = 0.6,
              scale_pos_weight = (neg_pos_wt[1]/neg_pos_wt[2]))
start_time = Sys.time()
xgbcv = xgb.cv(params = param, nrounds = 1000, data = dtrain, nfold = 10, showsd = T, stratified = T,
               print_every_n = 10, early_stopping_rounds = 20)

sol = xgb.train(dtrain, nrounds = xgbcv$best_iteration, watchlist, 
                verbose = 1, params = param)

print(Sys.time()- start_time)

### Predicting using the model and testing the data
pre = predict(sol,as.matrix(test_data))
pre = ifelse(pre>0.5,1,0)

###Performance Metrics
metric = confusionMatrix(as.factor(pre), labels_test, positive = "1")
metric
acc = metric$overall[1]
sens = sensitivity(factor(pre), labels_test)
spec = specificity(factor(pre), labels_test)
r.roc = roc(labels_test, as.numeric(pre))
plot(r.roc, print.thres = "best", print.thres.best.method = "closest.topleft", ret = c("threshold", "accuracy"),
     main = "ROC Plot - Random Forest")
auc = r.roc$auc

### Variable Importance
importance_matrix <- xgb.importance(colnames(train_data), model = sol)
xgb.plot.importance(importance_matrix, rel_to_first = TRUE, xlab = "Relative importance")
