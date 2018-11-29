install.packages("class")
library(class)

install.packages("ggplot2")
library(ggplot2)

install.packages("hexbin")
library(hexbin)

install.packages("quantreg")
library(quantreg)

install.packages("e1071")
library(e1071)

setwd("C:\\Users\\Admin\\Documents")

iondata <- read.csv("C:\\Users\\Admin\\Documents\\ionosphere.data", header = FALSE)

names(iondata) <- c("ID","type","1 first electro signal","1 second electro signal","2 first electro signal","2 second electro signal","3 first electro signal","3 second electro signal","4 first electro signal","4 second electro signal","5 first electro signal",
                    "5 second electro signal","6 first electro signal","6 second electro signal","7 first electro signal","7 second electro signal","8 first electro signal","8 second electro signal","9 first electro signal","9 second electro signal",
                    "10 first electro signal","10 second electro signal","11 first electro signal","11 second electro signal","12 first electro signal","12 second electro signal","13 first electro signal","13 second electro signal","14 first electro signal",
                    "14 second electro signal","15 first electro signal","15 second electro signal","16 first electro signal","16 second electro signal","Good or Bad")
ION_NOLAST <- iondata[,-35]
ION_NoID <- ION_NOLAST[,-1]
ION_NoClass <- ION_NoID[,-1]

FeatureScaling <- function(x) { ((x - min(x)) / (max(x) - min(x))) }

ION_Normalised <- as.data.frame(lapply(ION_NoClass, FeatureScaling))

ION_Training <- ION_Normalised[1:280,]
ION_Test <- ION_Normalised[281:351,]

svm.model <- svm(Type ~ ., data = ION_Training, cost = 100, gamma = 1)
svm.pred <- predict(svm.model,ION_Test[281:351,])

#IONK_Value <- floor(sqrt(length(ION_Training[,1])))
#IONK_Value <- 55

ION_Predictions <- svm(ION_Training,ION_Test,iondata[1:280,35], svm=IONsvm)

ION_Reference <- ION_NoClass[281:351,1]


table(ION_Predictions,ION_Reference)


big_errors <- c()

kValues <- c()

for(i in c(1:110)) {
  
  next_error <-  table(knn(ION_Training,ION_Test,iondata[1:280,35],k=i),ION_Reference)[1,2] +
    table(knn(ION_Training,ION_Test,iondata[1:280,35],k=i),ION_Reference)[2,1]
  
  big_errors <- c(big_errors, next_error)
  
  kValues <- c(kValues, i)
}

big_errors_df <- data.frame(kValues,big_errors)

names(big_errors_df) <- c("k-Value","Error Value")


ggplot(big_errors_df, aes(x = kValues, y = big_errors)) +
  geom_point()+ 
  geom_smooth(method = "loess",colour = "blue", size = 1) + 
  ggtitle("ION Data Sets") +
  xlab("k-Values") +
  ylab("Error") +
  theme(axis.text.x=element_text(angle=-45, vjust=0.5)) +
  theme(axis.text.y=element_text(angle=-45, hjust=-0.1, vjust=-0.5)) +
  scale_colour_manual(values = c("red","blue"))

