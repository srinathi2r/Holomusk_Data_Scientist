#### Using Random Forest on the merged dataset to predict the progression of the CKD patients ###

rm(list =ls())
library(caret)
library(randomForest)
library(dplyr)
library(pROC)
library(tidyr)
setwd("/Users/srinath/Downloads/dataScienceTask/Holomusk_Data_Scientist/")

## Loading the dataset
load("merged_data.RData")

model_data = merged_data %>% select(-c(id, drugs))

### Splitting the dataset
set.seed(20)
dat = createDataPartition(model_data$Stage_Progress, p = 0.7, list = FALSE)

train_data = model_data[dat,]
labels_train = factor(train_data$Stage_Progress)
train_data$Stage_Progress = NULL

test_data = model_data[-dat,]
labels_test = factor(test_data$Stage_Progress)
test_data$Stage_Progress = NULL

start_time = Sys.time()
### Tuning the RF algorithm 
rf_tune = tuneRF(train_data, labels_train, mtryStart = 50, ntreeTry = 301, 
                 stepFactor = 1.5, trace = T, plot = T, doBest = T)
sol = randomForest(train_data, labels_train, classwt = c(0.67,0.33),
                   importance = T, ntree = rf_tune$ntree)

print(Sys.time()- start_time)

### Predicting using the trained model and testing on the test dataset
pre = predict(sol, test_data)

###Performance Metrics
metric = confusionMatrix(pre, labels_test, positive = "True")
acc = metric$overall[1]
sens = sensitivity(factor(pre), labels_test)
spec = specificity(factor(pre), labels_test)

### Variable Importance
importance_matrix = varImp(sol,useModel = FALSE, nonpara = TRUE, scale = T)
varImpPlot(sol, sort = T, n.var=min(15, nrow(sol$importance)), type = 1, rex.labels = 3)

r.roc = roc(labels_test, as.numeric(pre))
r.roc$auc