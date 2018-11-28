#This is a template for the Introduction to Data Science Session.
#You are free to use any suitable dataset with a categorical dependent variable and
#and numeric independent variables.

#'install.packages' uses the R Package Manager to find and install packages
#'library' brings needed packages into scope (homologous with 'import' in Java)

#Classifier Library

install.packages("class")
library(class)

#Graphing Library

install.packages("ggplot2")
library(ggplot2)

#setwd = 'Set Working Directory'. This is where R will look for files that you reference.

setwd("C:\\Users\\Admin\\Documents")

#Read in CSV Data into a DataFrame object. A DataFrame is like a table.

RawData <- read.csv("C:\\Users\\Admin\\Documents\\BreastCancerData.data", header = FALSE)

#The 'c' function puts individual Strings into a String vector. This String vector can
#be added to the DataFrame as a header row with the 'names' function.

names(RawData) <- c("ID","diagnosis","radius_mean","texture_mean","perimeter_mean","area_mean","smoothness_mean","compactness_mean","concavity_mean","concave points_mean","symmetry_mean",
                 "fractal_dimensions_mean","radius_se","texture_se","perimeter_se","area_se","smoothness_se","compactness_se","concavity_se","concave points_se","symmetry_se","fractal_dimensions_se",
                 "radius_worst","texture_worst","perimeter_worst","area_worst","smoothness_worst",
                 "compactness_worst","concavity_worst","concave points_worst","symmetry_worse","fractal_dimensions_worse")


#Let's remove the ID Variable - R has an index built-in, so a second one is extraneous
#Besides, we don't want to feed ID in as a parameter to our KNN

Data_NoID <- RawData[,-1]

#We also need a subset of the BCD data set without the Result column
#This allows it to be used for normalisation and validation purposes

Data_NoResults <- Data_NoID[,-1]

#There are many Feature Scaling functions defined in CRAN, but to learn the maths, it is
#good to built one yourself, like so:

FeatureScaling <- function(x) { ((x - min(x)) / (max(x) - min(x))) }
#This corresponds to the equation given in the slides

Data_Normalised <- as.data.frame(lapply(Data_NoResults, FeatureScaling))
#Now our data has been normalised according to our function

#This is a good time to split our data. Let's go for a 75:25 (ish) split.
Data_Training <- Data_Normalised[1:450,] #Leave the value after the comma empty to
                                                    #capture all columns

Data_Test <- Data_Normalised[451:569,]

#Finally, let's compute a k-Value to use with the classifier
#The rule of thumb is the square root of the number of observations
#trained with, so let's use that for now

K_Value <- floor(sqrt(length(Data_Training[,1])))  #Result is floored, as k must be
                                                  #a whole number

#Now we can use the KNN Algorithm that comes with the 'class' package to classify our data
#This is done all at once, in a single function

Data_Predictions <- knn(Data_Training,Data_Test,RawData[1:450,2], k=K_Value)

#Now, let's view the performance of our KNN
#This is expressed in terms of {True Positives   False Positives}
#                              {False Negatives  True Negatives}

#First, subset the reference data into its own data frame

Data_Reference <- Data_NoID[451:569,1]

#Then, we can tabulate the two

table(Data_Predictions,Data_Reference)

#Further analysis of the results can give further insights into the suitability
#of the model, and of its parameters

#Type '?knn' into the console, and observe the various parameters
#You can adjust the parameters to empirically determine which k-Value and which
#geometry works best for this data set

#Once you're done testing results yourself, see below for a comprehensive exploration of all
#K-values up to 100.

###############################

big_errors <- c()

kValues <- c()

#Below is a for loop that creates a DataFrame containing the error values when KNN is applied
#to the dataset, for 1 <= k <= 100.
#When graphed, it allows the Data Scientist to find the lowest point on the trendline and
#therefore empircally determine the best K-Value to model this dataset.

for(i in c(1:100)) {
  
  next_error <-  table(knn(Data_Training,Data_Test,Data_NoID[1:450,1],k=i),Data_Reference)[1,2] +
                table(knn(Data_Training,Data_Test,Data_NoID[1:450,1],k=i),Data_Reference)[2,1]
  
  big_errors <- c(big_errors, next_error)
  
  kValues <- c(kValues, i)
}

big_errors_df <- data.frame(kValues,big_errors)

names(big_errors_df) <- c("k-Value","Error Value")

#A collection of ggplot2 functions used for defining a graph.
#ggplot2 is based off the 'Grammar of Graphics' model, and each component is able to
#be added and altered seperately from the rest.
#You will be provided with a PDF that will help you edit the graphic to your liking.

ggplot(big_errors_df, aes(x = kValues, y = big_errors)) +
  geom_point() +
  geom_smooth(method = "loess",colour = "blue", size = 1) + 
  ggtitle("Error vs k-Value for Breast Cancer Data") +
  xlab("k-Values") +
  ylab("Error") +
  theme(axis.text.x=element_text(angle=-45, vjust=0.5)) +
  theme(axis.text.y=element_text(angle=-45, hjust=-0.1, vjust=-0.5)) +
  scale_colour_manual(values = c("red","blue"))
