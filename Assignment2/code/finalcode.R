#0. Clean up
rm(list=ls())

length(unique(allData$subject))

# 1. Download the file
file.url <- "https://spark-public.s3.amazonaws.com/dataanalysis/samsungData.rda"
file.local <- "./samsungData.rda"
download.file(file.url,file.local,mode="wb")

# 2. Load the data
load("./samsungData.rda")

# 3. Use this to clean up the duplicate column names
allData <- data.frame(samsungData)

# 4. Split into training and test sets
trainSet <- allData[allData$subject 
#                     %in% c(1,3,5,6,7,8,11,14,15,16,17,19,21,22,23),]
                      %in% c(1,3,5,6),]
testSet <- allData[allData$subject 
                        %in% c(25,26,27,28,29,30),]

# 5. Calculate %age of training and test sets
#68.90642
100.0*nrow(trainSet)/nrow(allData)

#31.09358
100.0*nrow(testSet)/nrow(allData)


# Test using SVM

library(e1071)
colnames(trainSet)
svmTrain <- svm(as.factor(activity) ~ ., data=trainSet,
                kernel="radial", cost=10, gamma = 0.1)
svmTrainTest <- svm(as.factor(activity) ~ ., data=trainSet)

summary(svmTrainTest)

svmPredict <- predict(svmTrain, head(testSet), decision.values = TRUE)
attr(svmPredict, "decision.values")

svmTuner <- tune.svm(as.factor(activity) ~ ., data = trainSet, 
                   gamma=10^(-6:-3), cost = 10^(1:2), cross = 10)
summary(svmTuner)

bestGamma <- svmTuner$best.parameters[[1]]
bestCost <- svmTuner$best.parameters[[2]]

svmTunedTrain <- svm(as.factor(activity) ~ ., data = trainSet,
                   cost = bestCost, gamma = bestGamma, cross = 10)

summary(svmTunedTrain)

?svm

svmTunedPredict <- predict(svmTunedTrain, testSet)

svmTable <- table(observed=testSet$activity, svmPredict=svmTunedPredict)

classAgreement(svmTable)

svmProp <- prop.table(svmTable, 1)

svmAccuracyRate <- sum(svmProp[1,1] + svmProp[2,2] + svmProp[3,3] 
                      + svmProp[4,4] + svmProp[5,5] + svmProp[6,6])/nrow(svmProp)

svmErrorRate <- 1- svmAccuracyRate


svmTable
rfTable

classAgreement(svmTable)
classAgreement(rfTable)

sum(svmTunedPredict!=testSet$activity)/length(testSet$activity)
sum(rfPredict!=testSet$activity)/length(testSet$activity)


# Test using Random Forest
set.seed(1234)
library(randomForest)
rfTrain <- randomForest(as.factor(activity) ~ ., data=trainSet, prox=TRUE, 
                        ntree=400)
print(rfTrain)
rfTrain <- randomForest(as.factor(activity) ~ ., data=trainSet, prox=TRUE, 
                        ntree=500)
print(rfTrain)
rfTrain <- randomForest(as.factor(activity) ~ ., data=trainSet, prox=TRUE, 
                        ntree=600)
print(rfTrain)

rfTrain <- randomForest(as.factor(activity) ~ ., data=trainSet, prox=TRUE, 
                        ntree=650)
print(rfTrain)
# Best choice as low OOB of 0.46%
rfTrain <- randomForest(as.factor(activity) ~ ., data=trainSet, prox=TRUE, 
                        ntree=700)
print(rfTrain)

rfTrain <- randomForest(as.factor(activity) ~ ., data=trainSet, prox=TRUE, 
                        ntree=800)
print(rfTrain)

rfTrain <- randomForest(as.factor(activity) ~ ., data=trainSet, prox=TRUE, 
                        ntree=1000)
print(rfTrain)

rfPredict <- predict(rfTrain, testSet)
print(rfPredict)

rfTable <- table(observed=testSet$activity, predict = rfPredict)
rfTable
classAgreement(rfTable)


library(RColorBrewer)
## Set up a function that makes colors prettier
mypar <- function(a = 1, b = 1, brewer.n = 8, brewer.name = "Dark2", ...) {
  par(mar = c(2.5, 2.5, 1.6, 1.1), mgp = c(1.5, 0.5, 0))
  par(mfrow = c(a, b), ...)
  palette(brewer.pal(brewer.n, brewer.name))
}

## Set size of axes
cx = 1.2

## Save figure to pdf file
pdf(file = "../../figures/finalPredictionfigure.pdf", height = 4, width = 3 * 4)

mypar(mfrow = c(1, 3))

plot(as.factor(testSet$subject), as.factor(testSet$activity) , breaks=100,
     xlab="Subject", ylab="Activity", pch=19,  cex.axis = cx, 
     main="Activity for each Subject in Test Set")

plot(as.factor(testSet$subject), svmTunedPredict, 
     xlab="Subject", ylab="Activity", pch=19, cex.axis = cx, 
     main="SVM Activity Prediction",
     cex.lab = cx)

plot(as.factor(testSet$subject), rfPredict,
     xlab="Subject", ylab="Activity", pch=19,  cex.axis = cx, 
     main="Random Forest Activity Prediction",
     cex.lab = cx)


dev.off()

rfProp <- prop.table(rfTable, 1)

rfAccuracyRate <- sum(rfProp[1,1] + rfProp[2,2] + rfProp[3,3] 
      + rfProp[4,4] + rfProp[5,5] + rfProp[6,6])/nrow(rfProp)

rfErrorRate <- 1- rfAccuracyRate
