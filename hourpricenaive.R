library(class)
library(ggplot2)
install.packages("e1071")
library(e1071)

setwd("C:\\Users\\Admin\\Documents")

testdata <- read.csv("C:\\Users\\Admin\\Documents\\test.csv")
trainingdata <- read.csv("C:\\Users\\Admin\\Documents\\train.csv")
trainingprices <- read.csv("C:\\Users\\Admin\\Documents\\sample_submission.csv")

modifiedtraindata<-as.data.frame(trainingdata)

cleaneddata<-as.data.frame(modifiedtraindata)
#MSSubClass <-as.data.frame(modifiedtraindata$MSSubClass)
#cleaneddata$MSSubClass<-X

cleaneddata$Id<-NULL
cleaneddata$LotFrontage<-NULL
cleaneddata$LotArea<-NULL
cleaneddata$Street<-NULL
cleaneddata$LotShape<-NULL
cleaneddata$LandContour<-NULL
cleaneddata$Utilities<-NULL
cleaneddata$LotConfig<-NULL
cleaneddata$LandSlope<-NULL
cleaneddata$Condition1<-NULL
cleaneddata$Condition2<-NULL
cleaneddata$HouseStyle<-NULL
cleaneddata$OverallCond<-NULL
cleaneddata$YearBuilt<-NULL
cleaneddata$YearRemodAdd<-NULL
cleaneddata$RoofStyle<-NULL
cleaneddata$RoofMatl<-NULL
cleaneddata$Exterior1st<-NULL
cleaneddata$Exterior2nd<-NULL
cleaneddata$MasVnrType<-NULL
cleaneddata$MasVnrArea<-NULL
cleaneddata$ExterQual<-NULL
cleaneddata$ExterCond<-NULL
cleaneddata$RoofMatl<-NULL
cleaneddata$BsmtCond<-NULL
cleaneddata$BsmtExposure<-NULL
cleaneddata$RoofMatl<-NULL
cleaneddata$BsmtFinSF1<-NULL
cleaneddata$BsmtFinSF2<-NULL
cleaneddata$BsmtFinType2<-NULL
cleaneddata$BsmtUnfSF<-NULL
cleaneddata$TotalBsmtSF<-NULL
cleaneddata$LowQualFinSF<-NULL
cleaneddata$RoofMatl<-NULL
cleaneddata$BsmtFullBath<-NULL
cleaneddata$BsmtHalfBath<-NULL
cleaneddata$HalfBath<-NULL
cleaneddata$BedroomAbvGr<-NULL
cleaneddata$KitchenAbvGr<-NULL
cleaneddata$Functional<-NULL
cleaneddata$FireplaceQu<-NULL
cleaneddata$GarageType<-NULL
cleaneddata$GarageArea<-NULL
cleaneddata$GarageQual<-NULL
cleaneddata$GarageCond<-NULL
cleaneddata$WoodDeckSF<-NULL
cleaneddata$OpenPorchSF<-NULL
cleaneddata$EnclosedPorch<-NULL
cleaneddata$X3SsnPorch<-NULL
cleaneddata$ScreenPorch<-NULL
cleaneddata$PoolArea<-NULL
cleaneddata$PoolQC<-NULL
cleaneddata$Fence<-NULL
cleaneddata$MiscFeature<-NULL
cleaneddata$MiscVal<-NULL
cleaneddata$MoSold<-NULL
cleaneddata$YrSold<-NULL
cleaneddata$SaleType<-NULL
cleaneddata$GarageYrBlt<-NULL

cleaneddata$Alley<-sub("Grvl",2,modifiedtraindata$Alley)
cleaneddata$Alley<-sub("Pave",3,modifiedtraindata$Alley)
cleaneddata$Alley<-as.integer(modifiedtraindata$Alley)
cleaneddata$Alley[is.na(cleaneddata$Alley)]<-0
cleaneddata$Alley<-as.factor(cleaneddata$Alley)

cleaneddata$Electrical<-sub("1",1,cleaneddata$Electrical)
cleaneddata$Electrical<-sub("FuseA",0,cleaneddata$Electrical)
cleaneddata$Electrical<-sub("FuseF",0,cleaneddata$Electrical)
cleaneddata$Electrical<-sub("FuseP",0,cleaneddata$Electrical)
cleaneddata$Electrical<-sub("Mix",0,cleaneddata$Electrical)
cleaneddata$Electrical<-as.factor(cleaneddata$Electrical)

cleaneddata$MSZoning<-as.integer(cleaneddata$MSZoning)
cleaneddata$Alley<-as.integer(cleaneddata$Alley)
cleaneddata$Neighborhood<-as.integer(cleaneddata$Neighborhood)
cleaneddata$BldgType<-as.integer(cleaneddata$BldgType)
cleaneddata$Foundation<-as.integer(cleaneddata$Foundation)
cleaneddata$BsmtQual<-as.integer(cleaneddata$BsmtQual)
cleaneddata$BsmtFinType1<-as.integer(cleaneddata$BsmtFinType1)
cleaneddata$Heating<-as.integer(cleaneddata$Heating)
cleaneddata$HeatingQC<-as.integer(cleaneddata$HeatingQC)
cleaneddata$CentralAir<-as.integer(cleaneddata$CentralAir)
cleaneddata$Electrical<-as.integer(cleaneddata$Electrical)
cleaneddata$KitchenQual<-as.integer(cleaneddata$KitchenQual)
cleaneddata$GarageFinish<-as.integer(cleaneddata$GarageFinish)
cleaneddata$PavedDrive<-as.integer(cleaneddata$PavedDrive)
cleaneddata$SaleCondition<-as.integer(cleaneddata$SaleCondition)

cleaneddata$BsmtQual[is.na(cleaneddata$BsmtQual)]<- 0
cleaneddata$BsmtFinType1[is.na(cleaneddata$BsmtFinType1)]<- 0
cleaneddata$Electrical[is.na(cleaneddata$Electrical)]<- 0
cleaneddata$GarageFinish[is.na(cleaneddata$GarageFinish)]<- 0

##cleaning the test data
cleanedtestdata<-as.data.frame(testdata)

cleanedtestdata$Id<-NULL
cleanedtestdata$LotFrontage<-NULL
cleanedtestdata$LotArea<-NULL
cleanedtestdata$Street<-NULL
cleanedtestdata$LotShape<-NULL
cleanedtestdata$LandContour<-NULL
cleanedtestdata$Utilities<-NULL
cleanedtestdata$LotConfig<-NULL
cleanedtestdata$LandSlope<-NULL
cleanedtestdata$Condition1<-NULL
cleanedtestdata$Condition2<-NULL
cleanedtestdata$HouseStyle<-NULL
cleanedtestdata$OverallCond<-NULL
cleanedtestdata$YearBuilt<-NULL
cleanedtestdata$YearRemodAdd<-NULL
cleanedtestdata$RoofStyle<-NULL
cleanedtestdata$RoofMatl<-NULL
cleanedtestdata$Exterior1st<-NULL
cleanedtestdata$Exterior2nd<-NULL
cleanedtestdata$MasVnrType<-NULL
cleanedtestdata$MasVnrArea<-NULL
cleanedtestdata$ExterQual<-NULL
cleanedtestdata$ExterCond<-NULL
cleanedtestdata$RoofMatl<-NULL
cleanedtestdata$BsmtCond<-NULL
cleanedtestdata$BsmtExposure<-NULL
cleanedtestdata$RoofMatl<-NULL
cleanedtestdata$BsmtFinSF1<-NULL
cleanedtestdata$BsmtFinSF2<-NULL
cleanedtestdata$BsmtFinType2<-NULL
cleanedtestdata$BsmtUnfSF<-NULL
cleanedtestdata$TotalBsmtSF<-NULL
cleanedtestdata$LowQualFinSF<-NULL
cleanedtestdata$RoofMatl<-NULL
cleanedtestdata$BsmtFullBath<-NULL
cleanedtestdata$BsmtHalfBath<-NULL
cleanedtestdata$HalfBath<-NULL
cleanedtestdata$BedroomAbvGr<-NULL
cleanedtestdata$KitchenAbvGr<-NULL
cleanedtestdata$Functional<-NULL
cleanedtestdata$FireplaceQu<-NULL
cleanedtestdata$GarageType<-NULL
cleanedtestdata$GarageArea<-NULL
cleanedtestdata$GarageQual<-NULL
cleanedtestdata$GarageCond<-NULL
cleanedtestdata$WoodDeckSF<-NULL
cleanedtestdata$OpenPorchSF<-NULL
cleanedtestdata$EnclosedPorch<-NULL
cleanedtestdata$X3SsnPorch<-NULL
cleanedtestdata$ScreenPorch<-NULL
cleanedtestdata$PoolArea<-NULL
cleanedtestdata$PoolQC<-NULL
cleanedtestdata$Fence<-NULL
cleanedtestdata$MiscFeature<-NULL
cleanedtestdata$MiscVal<-NULL
cleanedtestdata$MoSold<-NULL
cleanedtestdata$YrSold<-NULL
cleanedtestdata$SaleType<-NULL
cleanedtestdata$GarageYrBlt<-NULL

cleanedtestdata$Alley<-sub("Grvl",2,cleanedtestdata$Alley)
cleanedtestdata$Alley<-sub("Pave",3,cleanedtestdata$Alley)
cleanedtestdata$Alley<-as.integer(cleanedtestdata$Alley)
cleanedtestdata$Alley[is.na(cleanedtestdata$Alley)]<-0
cleanedtestdata$Alley<-as.factor(cleanedtestdata$Alley)

cleanedtestdata$Electrical<-sub("1",1,cleanedtestdata$Electrical)
cleanedtestdata$Electrical<-sub("FuseA",0,cleanedtestdata$Electrical)
cleanedtestdata$Electrical<-sub("FuseF",0,cleanedtestdata$Electrical)
cleanedtestdata$Electrical<-sub("FuseP",0,cleanedtestdata$Electrical)
cleanedtestdata$Electrical<-sub("Mix",0,cleanedtestdata$Electrical)
cleanedtestdata$Electrical<-as.factor(cleanedtestdata$Electrical)

cleanedtestdata$MSZoning<-as.integer(cleanedtestdata$MSZoning)
cleanedtestdata$Alley<-as.integer(cleanedtestdata$Alley)
cleanedtestdata$Neighborhood<-as.integer(cleanedtestdata$Neighborhood)
cleanedtestdata$BldgType<-as.integer(cleanedtestdata$BldgType)
cleanedtestdata$Foundation<-as.integer(cleanedtestdata$Foundation)
cleanedtestdata$BsmtQual<-as.integer(cleanedtestdata$BsmtQual)
cleanedtestdata$BsmtFinType1<-as.integer(cleanedtestdata$BsmtFinType1)
cleanedtestdata$Heating<-as.integer(cleanedtestdata$Heating)
cleanedtestdata$HeatingQC<-as.integer(cleanedtestdata$HeatingQC)
cleanedtestdata$CentralAir<-as.integer(cleanedtestdata$CentralAir)
cleanedtestdata$Electrical<-as.integer(cleanedtestdata$Electrical)
cleanedtestdata$KitchenQual<-as.integer(cleanedtestdata$KitchenQual)
cleanedtestdata$GarageFinish<-as.integer(cleanedtestdata$GarageFinish)
cleanedtestdata$PavedDrive<-as.integer(cleanedtestdata$PavedDrive)
cleanedtestdata$SaleCondition<-as.integer(cleanedtestdata$SaleCondition)

cleanedtestdata$BsmtQual[is.na(cleanedtestdata$BsmtQual)]<-0
cleanedtestdata$BsmtFinType1[is.na(cleanedtestdata$BsmtFinType1)]<-0
cleanedtestdata$Electrical[is.na(cleanedtestdata$Electrical)]<-0
cleanedtestdata$GarageFinish[is.na(cleanedtestdata$GarageFinish)]<-0

pricetest <- as.data.frame(trainingprices)

#cleaneddata$MSSubClass <-as.factor(cleaneddata$MSSubClass)
#cleanedtestdata$MSSubClass <- as.factor(cleanedtestdata$MSSubClass)

str(cleaneddata)
str(cleanedtestdata)
##WHERE THE NAIVE BAYES START
#repeating_sequence = rep.int(seq_len(nrow(cleaneddata)), cleaneddata$MSSubClass)
#House_dataset = cleaneddata[repeating_sequence,]
#House_dataset$MSSubClass = NULL

naivehouseprice <- naiveBayes(SalePrice ~ ., data = cleanedtestdata)

housepred <- predict(naivehouseprice,cleaneddata)
write.csv(housepred, "naivepred.csv")

##MultiLine regression 
lines <- lm(pricetest$SalePrice ~ cleaneddata$MSSubClass+cleaneddata$MSZoning)
summary(lines)
plot(lines)
#qplot(cleaneddata$SalePrice, cleaneddata$SaleCondition, data = cleaneddata) +geom_smooth(method = 'lm',formula = y~x)



