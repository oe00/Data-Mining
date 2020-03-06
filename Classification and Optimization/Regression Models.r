library(caret)
library(ROCR)
library(e1071)


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


training_set = read.csv(file="training.csv", header = T, sep =",")
training_set$BikeBuyer = as.factor(training_set$BikeBuyer)


test_set = read.csv(file="testing.csv", header = T, sep =",")
test_set$BikeBuyer = as.factor(test_set$BikeBuyer)

#test$BikeBuyer = mapvalues(test$BikeBuyer, from = c("0","1"), to = c("No", "Yes"))
#training$BikeBuyer = mapvalues(training$BikeBuyer, from = c("0","1"), to = c("No", "Yes"))

set.seed(1)


# 4) Linear Regression

linearModel <- lm(BikeBuyer ~ . , data = training_set)

linearPrediction = predict(linearModel,test_set,type="response")
linearPrediction = ifelse(linearPrediction > 1.5,1,0)
linearResult = table(test_set$BikeBuyer,linearPrediction)

confusionMatrix(linearResult)
plotROC(predictedSet = linearPrediction,testSet = test_set$BikeBuyer,
        title="Linear Regression")


# 5) Logistic Regression

logimodel <- glm(BikeBuyer ~ . , data = training_set, family = binomial)
logiPrediction = predict(logimodel,test_set,type="response")
logiPrediction = ifelse(logiPrediction > 0.5,1,0)
logiResult = table(test_set$BikeBuyer,logiPrediction)

confusionMatrix(logiResult)
plotROC(predictedSet = logiPrediction,testSet = test_set$BikeBuyer,
        title="Logistic Regression")