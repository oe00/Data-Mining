library(dplyr)
library(ROCR)
library(rpart)
library(caret)
library(rpart.plot)


# load data

#setwd("/Users/oe/Desktop/assignment2")

training_set = read.csv(file="training.csv", header = T, sep =",")
training_set$BikeBuyer = as.factor(training_set$BikeBuyer)
training_set$MaritalStatus = as.factor(training_set$MaritalStatus)
training_set$Gender = as.factor(training_set$Gender)
training_set$EnglishEducation = as.factor(training_set$EnglishEducation)
training_set$Region= as.factor(training_set$Region)
training_set$HouseOwnerFlag = as.factor(training_set$HouseOwnerFlag)

test_set = read.csv(file="testing.csv", header = T, sep =",")
test_set$BikeBuyer = as.factor(test_set$BikeBuyer)
test_set$MaritalStatus = as.factor(test_set$MaritalStatus)
test_set$Gender = as.factor(test_set$Gender)
test_set$EnglishEducation = as.factor(test_set$EnglishEducation)
test_set$Region= as.factor(test_set$Region)
test_set$HouseOwnerFlag = as.factor(test_set$HouseOwnerFlag)

str(test_set)

set.seed(1)

# Helper Function - plotROC , given prediction set and test set, it plots the ROC

plotROC<- function(predictedSet,testSet,title) {
  ROCRPred = prediction(as.numeric(predictedSet),as.numeric(testSet))
  ROCRPerf = performance(ROCRPred, measure ="tpr", x.measure ="fpr")
  plot(ROCRPerf, colorize = TRUE, print.cutoffs.at = seq(0.1,by=0.1),
       main = paste("ROC CURVE FOR",title))
  abline(a=0, b=1)
  auc = performance(ROCRPred, measure = "auc")
  auc = auc@y.values[[1]]
  auc = round(auc, 3)
  legend (.6,.4,auc, title = "Area Under Curve (AOC)", cex =1)
  return(paste("Plotted:",title))
}

# 3) Decision Tree

## part 1

decisiontreeModel = rpart(BikeBuyer ~ ., data = training_set,method = "class")
decisiontreePrediction = predict(decisiontreeModel, newdata=test_set, type="class")
decisiontreeResult = table(test_set$BikeBuyer,decisiontreePrediction) 

confusionMatrix(decisiontreeResult) 
plotROC(predictedSet = decisiontreePrediction,testSet = test_set$BikeBuyer,
        title="Decision Tree")

rpart.plot(decisiontreeModel)

## part 2

requestedNormalization = c(3:5) # 3:YearlyIncome 4:TotalChildren 5:NumberChildrenAtHome

training_set_processed = training_set
test_set_processed = test_set

age_cat1 = training_set %>% filter(Age<51) %>% mutate(Age=1)
age_cat2 = training_set %>% filter(Age>50 & Age<66) %>% mutate(Age=2)
age_cat3 = training_set %>% filter(Age>65) %>% mutate(Age=3)
training_set_processed$Age = c(age_cat1[,11] , age_cat2[,11] ,age_cat3[,11])

age_cat1 = test_set %>% filter(Age<51) %>% mutate(Age=1)
age_cat2 = test_set %>% filter(Age>50 & Age<66) %>% mutate(Age=2)
age_cat3 = test_set %>% filter(Age>65) %>% mutate(Age=3)
test_set_processed$Age = c(age_cat1[,11] , age_cat2[,11] ,age_cat3[,11])

training_set_processed[,requestedNormalization] = scale(training_set[,requestedNormalization])
test_set_processed[,requestedNormalization] = scale(test_set[,requestedNormalization])

decisiontreeProcessedModel = rpart(BikeBuyer ~ ., data = training_set_processed,method = "class")
decisiontreeProcessedPrediction = predict(decisiontreeProcessedModel, newdata=test_set_processed, type="class")
decisiontreeProcessedResult = table(test_set$BikeBuyer,decisiontreeProcessedPrediction) 

confusionMatrix(decisiontreeProcessedResult) 
plotROC(predictedSet = decisiontreeProcessedPrediction,testSet = test_set$BikeBuyer,
        title="Decision Tree (N&D'ed)")

rpart.plot(decisiontreeProcessedModel)

# post-Pruned

#cpMinIndex = which.min(decisiontreeModel$cptable[,"xerror"])
#cpMin = decisiontreeModel$cptable[cpMinIndex,"CP"]

#ecisiontreePostPrunedModel= prune(decisiontreeModel,cp=cpMin)
#decisiontreePostPrunedPrediction = predict(decisiontreePostPrunedModel, newdata=test_set, type="class")
#decisiontreePostPrunedResult = table(test_set$BikeBuyer,decisiontreePostPrunedPrediction) 

#confusionMatrix(decisiontreePostPrunedResult) 
#plotROC(predictedSet = decisiontreePostPrunedPrediction,testSet = test_set$BikeBuyer,
#        title="Decision Tree Post-Pruned")


