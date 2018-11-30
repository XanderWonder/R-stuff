library(rpart)
library(randomForest)
library(rattle)
setwd("C:\\Users\\Admin\\Documents")

metadata <- read.csv("C:\\Users\\Admin\\Documents\\meta.data", header = FALSE)

names(metadata) <- c("1.	DS_Name	categorical	Name of DataSet", 
"2.	T	continuous	Number of examples in test set", 
"3.	N	continuous	Number of examples",
"4.	p	continuous	Number of attributes", 
"5.	k	continuous	Number of classes", 
"6.	Bin	continuous	Number of binary Attributes", 
"7.	Cost	continuous	Cost (1=yes,0=no)", 
"8.	SDratio	continuous	Standard deviation ratio 
9.	correl	continuous	Mean correlation between attributes", 
"10.	cancor1	continuous	First canonical correlation 
11.	cancor2	continuous	Second canonical correlation", 
"12.	fract1	continuous	First eigenvalue", 
"13.	fract2	continuous	Second eigenvalue", 
"14.	skewness	continuous	Mean of |E(X-Mean)|^3/STD^3", 
"15.	kurtosis	continuous	Mean of |E(X-Mean)|^4/STD^4", 
"16.	Hc	continuous	Mean entropy of attributes", 
"17.	Hx	continuous	Entropy of classes", 
"18.	MCx	continuous	Mean mutual entropy of class and attributes", 
"19.	EnAtr	continuous	Equivalent number of attributes", 
"20.	NSRatio	continuous	Noise-signal ratio", 
"21.	Alg_Name	categorical	Name of Algorithm", 
"22.	Norm_error	continuous	Normalized Error (continuous class)",
"23.  Algorithm used",
"24. Score of the algorithm")

Meta_No13 <- metadata[,-11]
Meta_NoType <- Meta_No13[,-12]
