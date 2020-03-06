library(dplyr)
library(ROCR)
library(rpart)
library(rpart.plot)
library(class)
library(caret)
library(e1071)
library(FSelector)


# Oguzhan Ergun

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


infoGain = information.gain(BikeBuyer~.,training_set)
infoGain

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


#
# Predictions
#

################################# ------------

# 1) Decision Tree

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

################################# ------------

# 2) Naive Bayes

# part 1

naiveBayesModel = naiveBayes(BikeBuyer~.,data=training_set)
naiveBayesPrediction = predict(naiveBayesModel,newdata=test_set)
naiveBayesResult = table(test_set$BikeBuyer,naiveBayesPrediction)

confusionMatrix(naiveBayesResult)
plotROC(predictedSet = naiveBayesPrediction,testSet = test_set$BikeBuyer,
        title="Naive Bayes")

# part 2

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

naiveBayesProcessedModel = naiveBayes(BikeBuyer~.,data=training_set_processed)
naiveBayesProcessedPrediction = predict(naiveBayesProcessedModel,newdata=test_set_processed)
naiveBayesProcessedResult = table(test_set$BikeBuyer,naiveBayesProcessedPrediction)

confusionMatrix(naiveBayesProcessedResult)
plotROC(predictedSet = naiveBayesProcessedPrediction,testSet = test_set$BikeBuyer,
        title="Naive Bayes (N&D'ed)")

################################# ------------

# 3) K-NN

kValue = 3
numericalAttributes = c(3:5,8,9,11)

# performing PCA
training.pca = prcomp(training_set[,numericalAttributes], center = TRUE,scale. = TRUE)
summary(training.pca)

# part 1

training_set_scaled = scale(training_set[,numericalAttributes])
test_set_scaled = scale(test_set[,numericalAttributes])

knnPrediction = knn(train=training_set_scaled,
                    test=test_set_scaled,
                    cl=training_set$BikeBuyer,k=kValue,prob=TRUE)
knnResult = table(test_set$BikeBuyer,knnPrediction)

confusionMatrix(knnResult)
plotROC(predictedSet = knnPrediction,testSet = test_set$BikeBuyer,
        title=paste("KNN , K=",kValue))

# part 2 

requestedNormalization = c(3:5) 

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

training_set_processed = scale(training_set[,c(numericalAttributes,requestedNormalization)])
test_set_processed = scale(test_set[,c(numericalAttributes,requestedNormalization)])

knnPrediction = knn(train=training_set_processed,test=test_set_processed,
                    cl=training_set$BikeBuyer,
                    k=kValue,prob=TRUE)
knnResult = table(test_set$BikeBuyer,knnPrediction)

confusionMatrix(knnResult)
plotROC(predictedSet = knnPrediction,testSet = test_set$BikeBuyer,
        title=paste("KNN (N&D'ed), K=",kValue))
