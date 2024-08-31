library(tidyverse)
library(readxl)
library(caret)
library(ggplot2)

setwd("C:/Users/Owner/Desktop/CMSC462/hw3")


Lending = read_csv("Lending.csv")

Lending2 = Lending

# Balance data set
Lending = rbind(sample_n(filter(Lending2, loan_default == 1),1000),sample_n(filter(Lending2, loan_default == 0),1000))

# Create the scatter plot
#scatter_plot <- ggplot(data = Lending, aes(x = open_acc, y = inq_last_6mths, color = factor(loan_default))) +
 # geom_point() +
 # scale_color_manual(values = c("0" = "blue", "1" = "red")) +  # Set colors for loan_default
 # labs(x = "open accouns", y = "Inquiries in Last 6 Months", color = "Loan Default", "residence_propertyRent") +
 # theme_minimal()

# Display the plot
#print(scatter_plot)

# Logistic Regression

# Drop columns of independent variables that don't help
#Lending <- Lending[, !(names(Lending) %in% c("months_since_first_credit",
                                           #  "num_accts_ever_120_pd"))]






glimpse(Lending)


# The I() function creates a logical vector that is TRUE when HeartDisease is "Yes"
# and FALSE otherwise
Lending = Lending %>% mutate(loan_default = I(loan_default == "1") %>% as.numeric())
#Separating Test and Training Data
Lending

columnsChange = c("residence_property")

#sapply allows you to loop through and applies the function to each column
sapply(Lending[columnsChange], unique)
#lapply allows you to loop through, in addition it returns
# a list or dataframe
Lending[columnsChange]  = lapply(Lending[columnsChange], as.factor)
glimpse(Lending)
TrainIndex = sample(1:nrow(Lending), round(0.7*nrow(Lending)))
LendingTrain = Lending[TrainIndex, ] 
LendingTest = Lending[-TrainIndex, ] 

# Here you can try  
LendingLogit = glm(loan_default ~ ., data = LendingTrain,# same as in lm()
                 family = "binomial") # for logistic, this is always set to "binomial"

summary(LendingLogit)

varImp(LendingLogit, scale=FALSE)
#creates a new column called EstimatedProb in HeartTest
EstimatedProb = predict(LendingLogit,
                        newdata = LendingTest, type = "terms")

LendingTest = LendingTest %>% 
  mutate(EstimatedProb = predict(LendingLogit,
                                 newdata = LendingTest, type = "response"))
summary(LendingTest$EstimatedProb)

# Now let's predict Y = 1 if P(Y = 1) > 0.5
LendingTest2 = LendingTest %>% mutate(LendingLogitPredicited = I(EstimatedProb > 0.5) %>% as.numeric())
glimpse(LendingTest2)


LendingTable = table(LendingTest2$LendingLogitPredicited ,LendingTest2$loan_default)
LendingTable

confusionMatrix(LendingTable)

library(ROCR)


# Compute AUC for predicting Class with the model
prob <- predict(LendingLogit, newdata=LendingTest, type="response")
pred <- prediction(prob, LendingTest$loan_default)
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