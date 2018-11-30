install.packages("e1071")
install.packages("rpart")
install.packages("mlbench")

library(ggplot2)
library(e1071)
library(rpart)
library(mlbench)

#rawdata <- read.csv("C:\\Users\\Admin\\Documents\\glass.data", header = FALSE)
data(Ionosphere, package="mlbench")
##split data into training and test sets
index <- 1:nrow(Ionosphere)
testindex <- sample(index, trunc(length(index)/3))
testset <- Ionosphere[testindex,]
trainset <- Ionosphere[testindex,]

##svm
svm.model <- svm(V3 ~ V4, data = trainset, cost = 100, gamma = 1)
svm.pred <- predict(svm.model, testset[,-10])

##rpart
rpart.model <- rpart(V3 ~ V4, data = trainset)
rpart.pred <- predict(rpart.model,testset[,-10])

##computes svm confusion matrix
table(pred = svm.pred, true = testset[,10])

##computes rpart confusion matrix
table(pred = rpart.pred, true = testset[,10])


