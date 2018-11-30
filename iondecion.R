library(rpart)
install.packages("randomForest")
library(randomForest)
install.packages("rattle")
library(rattle)

setwd("C:\\Users\\Admin\\Documents")

iondata <- read.csv("C:\\Users\\Admin\\Documents\\ionosphere.data", header = FALSE)

names(iondata) <- c("ID","type","1 first electro signal","1 second electro signal","2 first electro signal","2 second electro signal","3 first electro signal","3 second electro signal","4 first electro signal","4 second electro signal","5 first electro signal",
                    "5 second electro signal","6 first electro signal","6 second electro signal","7 first electro signal","7 second electro signal","8 first electro signal","8 second electro signal","9 first electro signal","9 second electro signal",
                    "10 first electro signal","10 second electro signal","11 first electro signal","11 second electro signal","12 first electro signal","12 second electro signal","13 first electro signal","13 second electro signal","14 first electro signal",
                    "14 second electro signal","15 first electro signal","15 second electro signal","16 first electro signal","16 second electro signal","Good or Bad")
ION_NoId <- iondata[,-1]
ION_NoType <- ION_NoId[,-1]

ION_NoType$`Good or Bad` <- as.factor(ION_NoType$`Good or Bad`)
tree <- rpart( `Good or Bad` ~ . , data = ION_NoType)
print(tree$cptable)
fancyRpartPlot(tree)
