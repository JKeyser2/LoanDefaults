library(tidyverse)
library(readxl)
library(caret)
library(ROCR)
library(e1071)

setwd("C:/Users/Owner/Desktop/CMSC462/hw3")


Lending = read_csv("Lending.csv")

Lending2 = Lending

# Balance data set
Lending = rbind(sample_n(filter(Lending2, loan_default == 1),1000),sample_n(filter(Lending2, loan_default == 0),1000))

# Drop columns of independent variables that don't help
Lending <- Lending[, !(names(Lending) %in% c("open_acc",
                                            "num_accts_ever_120_pd", "pct_loan_income"))]

# This is categorical data
columnsChange = c("residence_property", "loan_default")


#sapply allows you to loop through and applies the function to each column
sapply(Lending[columnsChange], unique)
#lapply allows you to loop through, in addition it returns
# a list or dataframe
Lending[columnsChange]  = lapply(Lending[columnsChange], as.factor)

glimpse(Lending)

#75% for training, 25% for testing

## 75% of the sample size
smp_size <- floor(0.75 * nrow(Lending))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(Lending)), size = smp_size)

train <- Lending[train_ind, ]
test <- Lending[-train_ind, ]

NVmodel <- naiveBayes(loan_default~ ., data = train)
preds <- predict(NVmodel, newdata = test)
conf_matrix <- table(preds, test$loan_default)

# 0 is no default, 1 is default

# The rows are the predicted class labels, columns are are actual class labels

# True Positives (TP): 19236
# True Negatives (TN): 32
# False Positives (FP): 2754
# False Negatives (FN): 91

conf_matrix
confusionMatrix(conf_matrix)

## check the raw, this gives you the probability 
#predsRaw <- predict(NVmodel, newdata = test, type = "raw")
#predsRaw

#Examine NVModelin  details

NVmodel

# Columns:
# [,1] is mean, [,2] is standard deviation
# [0] to anaemia, [1] to anaemia

#Rows:
# [0] is no default
# [1] is default

NVmodel$tables

NVmodel$apriori

# Compute AUC for predicting Class with the model
prob <- predict(NVmodel, newdata=test, type="raw")
pred <- prediction(prob[,2], test$loan_default)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
#following order: bottom, left, top, and right. 
par(mar=c(5,8,1,.5))
#Receiver operating characteristic
plot(perf, col="red")
abline(a=0, b=1)
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc

rocData = data.frame(c(perf@alpha.values, perf@x.values, perf@y.values))
