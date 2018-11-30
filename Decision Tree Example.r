library(rpart)
library(randomForest)
library(rattle)

setwd("C:/Users/tknow/Desktop/datasets")
data <- read.csv("titanic.csv")

data$PassengerId <- NULL
data$Name <- NULL
data$Ticket <- NULL
data$Cabin <- NULL
data <- na.omit(data)

data$Pclass <- as.factor(data$Pclass)
data$Parch <- as.factor(data$Parch)

tree <- rpart(Survived ~ . , data=data)

print(tree$cptable)

forest <- randomForest(Survived ~ . , data=data)

print(forest)

data$Survived <- as.factor(data$Survived)

forest <- randomForest(Survived ~ . , data=data)

print(forest)

fancyRpartPlot(tree)
