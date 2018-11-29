install.packages("class")
library(class)

install.packages("ggplot2")
library(ggplot2)

install.packages("hexbin")
library(hexbin)

install.packages("quantreg")
library(quantreg)

setwd("C:\\Users\\Admin\\Documents")

susydata <- read.csv("C:\\Users\\Admin\\Documents\\leaf.csv", header = FALSE)

names(susydata) <- c("1. Class (Species)", 
"2. Specimen Number", 
"3. Eccentricity", 
"4. Aspect Ratio",
"5. Elongation", 
"6. Solidity", 
"7. Stochastic Convexity", 
"8. Isoperimetric Factor", 
"9. Maximal Indentation Depth", 
"10. Lobedness", 
"11. Average Intensity", 
"12. Average Contrast", 
"13. Smoothness", 
"14. Third moment", 
"15. Uniformity", 
"16. Entropy")

SUSY_NoID <- susydata[,-1]
SUSY_NoClass <- SUSY_NoID[,-1]

FeatureScaling <- function(x) { ((x - min(x)) / (max(x) - min(x))) }

SUSY_Normalised <- as.data.frame(lapply(SUSY_NoClass, FeatureScaling))

SUSY_Training <- SUSY_Normalised[1:280,]
sUSY_Test <- SUSY_Normalised[281:340,]

#SUSYK_Value <- floor(sqrt(length(SUSY_Training[,1])))
SUSYK_Value <- 100

SUSY_Predictions <- knn(SUSY_Training,sUSY_Test,susydata[1:280,1], k=SUSYK_Value)

SUSY_Reference <- SUSY_NoID[281:340,1]

table(SUSY_Predictions,SUSY_Reference)

big_errors <- c()

kValues <- c()

for(i in c(1:100)) {
  
  next_error <-  table(knn(SUSY_Training,sUSY_Test,susydata[1:280,1],k=i),SUSY_Reference)[1,2] +
    table(knn(SUSY_Training,sUSY_Test,susydata[1:280,1],k=i),SUSY_Reference)[2,1]
  
  big_errors <- c(big_errors, next_error)
  
  kValues <- c(kValues, i)
}

big_errors_df <- data.frame(kValues,big_errors)

names(big_errors_df) <- c("k-Value","Error Value")

ggplot(big_errors_df, aes(x = kValues, y = big_errors)) +
  geom_point()+ 
  geom_smooth(method = "loess",colour = "blue", size = 1) + 
  ggtitle("Leaf Data Sets") +
  xlab("k-Values") +
  ylab("Error") +
  theme(axis.text.x=element_text(angle=-45, vjust=0.5)) +
  theme(axis.text.y=element_text(angle=-45, hjust=-0.1, vjust=-0.5)) +
  scale_colour_manual(values = c("red","blue"))

ggplot(big_errors_df, aes(x = big_errors, y = kValues )) + coord_polar(theta = "x",direction = 1)

ggplot(big_errors_df, aes(x = kValues, y = big_errors )) + geom_polygon()

ggplot(big_errors_df, aes(x = kValues, y = big_errors )) + geom_quantile()
