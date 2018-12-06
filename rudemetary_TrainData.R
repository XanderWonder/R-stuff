install.packages("rpart")
install.packages("randomForest")
install.packages("rattle")
library(rpart)
library(randomForest)
library(rattle)
library(class)
library(ggplot2)
library(e1071)

setwd("C:/Users/Admin/Documents")
TestData <- read.csv("train.csv")
cleanedtraindata<-as.data.frame(TestData)

RTestData <- read.csv("test.csv")
cleantestdata <-as.data.frame(RTestData)

cleanedtraindata$Id<-NULL
cleanedtraindata$LotFrontage<-NULL
cleanedtraindata$LotArea<-NULL
cleanedtraindata$Street<-NULL
cleanedtraindata$LotShape<-NULL
cleanedtraindata$LandContour<-NULL
cleanedtraindata$Utilities<-NULL
cleanedtraindata$LotConfig<-NULL
cleanedtraindata$LandSlope<-NULL
cleanedtraindata$Condition1<-NULL
cleanedtraindata$Condition2<-NULL
cleanedtraindata$HouseStyle<-NULL
cleanedtraindata$OverallCond<-NULL
cleanedtraindata$YearBuilt<-NULL
cleanedtraindata$YearRemodAdd<-NULL
cleanedtraindata$RoofStyle<-NULL
cleanedtraindata$RoofMatl<-NULL
cleanedtraindata$Exterior1st<-NULL
cleanedtraindata$Exterior2nd<-NULL
cleanedtraindata$MasVnrType<-NULL
cleanedtraindata$MasVnrArea<-NULL
cleanedtraindata$ExterQual<-NULL
cleanedtraindata$ExterCond<-NULL
cleanedtraindata$RoofMatl<-NULL
cleanedtraindata$BsmtCond<-NULL
cleanedtraindata$BsmtExposure<-NULL
cleanedtraindata$RoofMatl<-NULL
cleanedtraindata$BsmtFinSF1<-NULL
cleanedtraindata$BsmtFinSF2<-NULL
cleanedtraindata$BsmtFinType2<-NULL
cleanedtraindata$BsmtUnfSF<-NULL
cleanedtraindata$TotalBsmtSF<-NULL
cleanedtraindata$LowQualFinSF<-NULL
cleanedtraindata$RoofMatl<-NULL
cleanedtraindata$BsmtFullBath<-NULL
cleanedtraindata$BsmtHalfBath<-NULL
cleanedtraindata$HalfBath<-NULL
cleanedtraindata$BedroomAbvGr<-NULL
cleanedtraindata$KitchenAbvGr<-NULL
cleanedtraindata$Functional<-NULL
cleanedtraindata$FireplaceQu<-NULL
cleanedtraindata$GarageType<-NULL
cleanedtraindata$GarageArea<-NULL
cleanedtraindata$GarageQual<-NULL
cleanedtraindata$GarageCond<-NULL
cleanedtraindata$GarageYrBlt<-NULL
cleanedtraindata$WoodDeckSF<-NULL
cleanedtraindata$OpenPorchSF<-NULL
cleanedtraindata$EnclosedPorch<-NULL
cleanedtraindata$X3SsnPorch<-NULL
cleanedtraindata$ScreenPorch<-NULL
cleanedtraindata$PoolArea<-NULL
cleanedtraindata$PoolQC<-NULL
cleanedtraindata$Fence<-NULL
cleanedtraindata$MiscFeature<-NULL
cleanedtraindata$MiscVal<-NULL
cleanedtraindata$MoSold<-NULL
cleanedtraindata$YrSold<-NULL
cleanedtraindata$SaleType<-NULL
cleanedtraindata$Neighborhood<-NULL
cleanedtraindata$KitchenQual<-NULL
cleanedtraindata$HeatingQC<-NULL
cleanedtraindata$Electrical<-NULL

#clean the MSZoning 
cleanedtraindata$MSZoning <- sub("A",1,cleanedtraindata$MSZoning)
cleanedtraindata$MSZoning <- sub("C(all)",2,cleanedtraindata$MSZoning)
cleanedtraindata$MSZoning <- sub("FV",3,cleanedtraindata$MSZoning)
cleanedtraindata$MSZoning <- sub("I",4,cleanedtraindata$MSZoning)
cleanedtraindata$MSZoning <- sub("RH",5,cleanedtraindata$MSZoning)
cleanedtraindata$MSZoning <- sub("RL",6,cleanedtraindata$MSZoning)
cleanedtraindata$MSZoning <- sub("RM",7,cleanedtraindata$MSZoning)
cleanedtraindata$MSZoning <- as.integer(cleanedtraindata$MSZoning)

#cleaning ally
cleanedtraindata$Alley <- sub("Grvl",1,cleanedtraindata$Alley)
cleanedtraindata$Alley <- sub("Pave",2,cleanedtraindata$Alley)
cleanedtraindata$Alley <-as.integer(cleanedtraindata$Alley)
cleanedtraindata$Alley[is.na(cleanedtraindata$Alley)] <- 0
cleanedtraindata$Alley <-as.integer(cleanedtraindata$Alley)

#cleaning electrical
cleanedtraindata$Electrical<-sub("1",1,cleanedtraindata$Electrical)
cleanedtraindata$Electrical<-sub("FuseA",0,cleanedtraindata$Electrical)
cleanedtraindata$Electrical<-sub("FuseF",0,cleanedtraindata$Electrical)
cleanedtraindata$Electrical<-sub("FuseP",0,cleanedtraindata$Electrical)
cleanedtraindata$Electrical<-sub("Mix",0,cleanedtraindata$Electrical)
cleanedtraindata$Electrical<-as.integer(cleanedtraindata$Electrical)
cleanedtraindata$Electrical[is.na(cleanedtraindata$Electrical)] <- 0
#send back to factor
cleanedtraindata$Electrical<-as.factor(cleanedtraindata$Electrical)

#replace other na values by making the variable an integer, sending NA values to 0, then sending variable back to a factor
cleanedtraindata$BsmtQual<-as.integer(cleanedtraindata$BsmtQual)
cleanedtraindata$BsmtQual[is.na(cleanedtraindata$BsmtQual)]<-0
cleanedtraindata$BsmtQual<-as.integer(cleanedtraindata$BsmtQual)

cleanedtraindata$BsmtFinType1<-as.integer(cleanedtraindata$BsmtFinType1)
cleanedtraindata$BsmtFinType1[is.na(cleanedtraindata$BsmtFinType1)]<-0 
cleanedtraindata$BsmtFinType1<-as.integer(cleanedtraindata$BsmtFinType1)

cleanedtraindata$GarageFinish<-as.integer(cleanedtraindata$GarageFinish)
cleanedtraindata$GarageFinish[is.na(cleanedtraindata$GarageFinish)]<-0 
cleanedtraindata$GarageFinish<-as.integer(cleanedtraindata$GarageFinish)

#change fireplaces to either do or dont have one
cleanedtraindata$Fireplaces<-sub(2,1,cleanedtraindata$Fireplaces)
cleanedtraindata$Fireplaces<-sub(3,1,cleanedtraindata$Fireplaces)
cleanedtraindata$Fireplaces<-as.integer(cleanedtraindata$Fireplaces)

#change heating to gas or non gas
cleanedtraindata$Heating<-sub("GasA",3, cleanedtraindata$Heating)
cleanedtraindata$Heating<-sub("GasW",3, cleanedtraindata$Heating)
cleanedtraindata$Heating<-sub("Wall",2, cleanedtraindata$Heating)
cleanedtraindata$Heating<-sub("Floor",2, cleanedtraindata$Heating)
cleanedtraindata$Heating<-sub("OthW",1, cleanedtraindata$Heating)
cleanedtraindata$Heating<-sub("Grav",1, cleanedtraindata$Heating)
cleanedtraindata$Heating<-as.integer(cleanedtraindata$Heating)

#clean functional
cleanedtraindata$Functional<-sub("Typ",1,cleanedtraindata$Functional)
cleanedtraindata$Functional<-sub("Min1",0,cleanedtraindata$Functional)
cleanedtraindata$Functional<-sub("Min2",0,cleanedtraindata$Functional)
cleanedtraindata$Functional<-sub("Mod",0,cleanedtraindata$Functional)
cleanedtraindata$Functional<-sub("Maj1",0,cleanedtraindata$Functional)
cleanedtraindata$Functional<-sub("Maj2",0,cleanedtraindata$Functional)
cleanedtraindata$Functional<-sub("Sev",0,cleanedtraindata$Functional)
cleanedtraindata$Functional<-sub("Sel",0,cleanedtraindata$Functional)
cleanedtraindata$Functional<-as.integer(cleanedtraindata$Functional)
#cleanedtraindata$Functional<-as.factor(cleanedtraindata$Functional)

#change paveddrive to paved and not paved
cleanedtraindata$PavedDrive<-sub("Y",2, cleanedtraindata$PavedDrive)
cleanedtraindata$PavedDrive<-sub("P",1, cleanedtraindata$PavedDrive)
cleanedtraindata$PavedDrive<-sub("N",0, cleanedtraindata$PavedDrive)
cleanedtraindata$PavedDrive<-as.integer(cleanedtraindata$PavedDrive)

#CLEAN THE FENCE
cleanedtraindata$Fence<-sub("GdPrv", 4, cleanedtraindata$Fence)
cleanedtraindata$Fence<-sub("MnPrv", 3, cleanedtraindata$Fence)
cleanedtraindata$Fence<-sub("GdWo", 2, cleanedtraindata$Fence)
cleanedtraindata$Fence<-sub("MnWw", 1, cleanedtraindata$Fence)
cleanedtraindata$Fence<-sub("NA", 0, cleanedtraindata$Fence)
cleanedtraindata$Fence[is.na(cleanedtraindata$Fence)]<-0 

#CLEAN THE LANCONTUR
cleanedtraindata$LandContour<-sub("Lvl",3,cleanedtraindata$LandContour)
cleanedtraindata$LandContour<-sub("Bnk",2,cleanedtraindata$LandContour)
cleanedtraindata$LandContour<-sub("HLS",1,cleanedtraindata$LandContour)
cleanedtraindata$LandContour<-sub("Low",0,cleanedtraindata$LandContour)
cleanedtraindata$LandContour<-as.integer(cleanedtraindata$LandContour)

#CLEAN THE LAND SLOPE
cleanedtraindata$LandSlope<-sub("Gtl",2,cleanedtraindata$LandSlope)
cleanedtraindata$LandSlope<-sub("Mod",1,cleanedtraindata$LandSlope)
cleanedtraindata$LandSlope<-sub("Sev",0,cleanedtraindata$LandSlope)
cleanedtraindata$LandSlope<-as.integer(cleanedtraindata$LandSlope)

#CLEAN THE BldgType
cleanedtraindata$BldgType<-sub("1Fam",2,cleanedtraindata$BldgType)
cleanedtraindata$BldgType<-sub("2fmCon",3,cleanedtraindata$BldgType)
cleanedtraindata$BldgType<-sub("Duplex",4,cleanedtraindata$BldgType)
cleanedtraindata$BldgType<-sub("TwnhsE",5,cleanedtraindata$BldgType)
cleanedtraindata$BldgType<-sub("Twnhs",6,cleanedtraindata$BldgType)
cleanedtraindata$BldgType<-as.integer(cleanedtraindata$BldgType)

#CLEAN THE FOUNDATION
cleanedtraindata$Foundation<-sub("BrkTil",5,cleanedtraindata$Foundation)
cleanedtraindata$Foundation<-sub("CBlock",4,cleanedtraindata$Foundation)
cleanedtraindata$Foundation<-sub("PConc",3,cleanedtraindata$Foundation)
cleanedtraindata$Foundation<-sub("Slab",2,cleanedtraindata$Foundation)
cleanedtraindata$Foundation<-sub("Stone",1,cleanedtraindata$Foundation)
cleanedtraindata$Foundation<-sub("Wood",0,cleanedtraindata$Foundation)
cleanedtraindata$Foundation<-as.integer(cleanedtraindata$Foundation)

#CLEAN THE SALE CONDI
cleanedtraindata$SaleCondition<-sub("Normal",1,cleanedtraindata$SaleCondition)
cleanedtraindata$SaleCondition<-sub("Abnorml",2,cleanedtraindata$SaleCondition)
cleanedtraindata$SaleCondition<-sub("AdjLand",3,cleanedtraindata$SaleCondition)
cleanedtraindata$SaleCondition<-sub("Alloca",4,cleanedtraindata$SaleCondition)
cleanedtraindata$SaleCondition<-sub("Family",5,cleanedtraindata$SaleCondition)
cleanedtraindata$SaleCondition<-sub("Partial",6,cleanedtraindata$SaleCondition)
cleanedtraindata$SaleCondition<-as.integer(cleanedtraindata$SaleCondition)

cleanedtraindata$CentralAir<-sub("Y",1,cleanedtraindata$CentralAir)
cleanedtraindata$CentralAir<-sub("N",0,cleanedtraindata$CentralAir)
cleanedtraindata$CentralAir<-as.integer(cleanedtraindata$CentralAir)

##Clean Test
cleantestdata$Id<-NULL
cleantestdata$LotFrontage<-NULL
cleantestdata$LotArea<-NULL
cleantestdata$Street<-NULL
cleantestdata$LotShape<-NULL
cleantestdata$LandContour<-NULL
cleantestdata$Utilities<-NULL
cleantestdata$LotConfig<-NULL
cleantestdata$LandSlope<-NULL
cleantestdata$Condition1<-NULL
cleantestdata$Condition2<-NULL
cleantestdata$HouseStyle<-NULL
cleantestdata$OverallCond<-NULL
cleantestdata$YearBuilt<-NULL
cleantestdata$YearRemodAdd<-NULL
cleantestdata$RoofStyle<-NULL
cleantestdata$RoofMatl<-NULL
cleantestdata$Exterior1st<-NULL
cleantestdata$Exterior2nd<-NULL
cleantestdata$MasVnrType<-NULL
cleantestdata$MasVnrArea<-NULL
cleantestdata$ExterQual<-NULL
cleantestdata$ExterCond<-NULL
cleantestdata$RoofMatl<-NULL
cleantestdata$BsmtCond<-NULL
cleantestdata$BsmtExposure<-NULL
cleantestdata$RoofMatl<-NULL
cleantestdata$BsmtFinSF1<-NULL
cleantestdata$BsmtFinSF2<-NULL
cleantestdata$BsmtFinType2<-NULL
cleantestdata$BsmtUnfSF<-NULL
cleantestdata$TotalBsmtSF<-NULL
cleantestdata$LowQualFinSF<-NULL
cleantestdata$RoofMatl<-NULL
cleantestdata$BsmtFullBath<-NULL
cleantestdata$BsmtHalfBath<-NULL
cleantestdata$HalfBath<-NULL
cleantestdata$BedroomAbvGr<-NULL
cleantestdata$KitchenAbvGr<-NULL
cleantestdata$Functional<-NULL
cleantestdata$FireplaceQu<-NULL
cleantestdata$GarageType<-NULL
cleantestdata$GarageArea<-NULL
cleantestdata$GarageQual<-NULL
cleantestdata$GarageCond<-NULL
cleantestdata$GarageYrBlt<-NULL
cleantestdata$WoodDeckSF<-NULL
cleantestdata$OpenPorchSF<-NULL
cleantestdata$EnclosedPorch<-NULL
cleantestdata$X3SsnPorch<-NULL
cleantestdata$ScreenPorch<-NULL
cleantestdata$PoolArea<-NULL
cleantestdata$PoolQC<-NULL
cleantestdata$Fence<-NULL
cleantestdata$MiscFeature<-NULL
cleantestdata$MiscVal<-NULL
cleantestdata$MoSold<-NULL
cleantestdata$YrSold<-NULL
cleantestdata$SaleType<-NULL
cleantestdata$Neighborhood<-NULL
cleantestdata$KitchenQual<-NULL
cleantestdata$HeatingQC<-NULL
cleantestdata$Electrical<-NULL

#clean the MSZoning 
cleantestdata$MSZoning <- sub("A",1,cleantestdata$MSZoning)
cleantestdata$MSZoning <- sub("C(all)",2,cleantestdata$MSZoning)
cleantestdata$MSZoning <- sub("FV",3,cleantestdata$MSZoning)
cleantestdata$MSZoning <- sub("I",4,cleantestdata$MSZoning)
cleantestdata$MSZoning <- sub("RH",5,cleantestdata$MSZoning)
cleantestdata$MSZoning <- sub("RL",6,cleantestdata$MSZoning)
cleantestdata$MSZoning <- sub("RM",7,cleantestdata$MSZoning)
cleantestdata$MSZoning <- as.integer(cleantestdata$MSZoning)

#cleaning ally
cleantestdata$Alley <- sub("Grvl",1,cleantestdata$Alley)
cleantestdata$Alley <- sub("Pave",2,cleantestdata$Alley)
cleantestdata$Alley <-as.integer(cleantestdata$Alley)
cleantestdata$Alley[is.na(cleantestdata$Alley)] <- 0
cleantestdata$Alley <-as.integer(cleantestdata$Alley)

#cleaning electrical
cleantestdata$Electrical<-sub("1",1,cleantestdata$Electrical)
cleantestdata$Electrical<-sub("FuseA",0,cleantestdata$Electrical)
cleantestdata$Electrical<-sub("FuseF",0,cleantestdata$Electrical)
cleantestdata$Electrical<-sub("FuseP",0,cleantestdata$Electrical)
cleantestdata$Electrical<-sub("Mix",0,cleantestdata$Electrical)
cleantestdata$Electrical<-as.integer(cleantestdata$Electrical)
cleantestdata$Electrical[is.na(cleantestdata$Electrical)] <- 0
#send back to factor
cleantestdata$Electrical<-as.factor(cleantestdata$Electrical)

#replace other na values by making the variable an integer, sending NA values to 0, then sending variable back to a factor
cleantestdata$BsmtQual<-as.integer(cleantestdata$BsmtQual)
cleantestdata$BsmtQual[is.na(cleantestdata$BsmtQual)]<-0
cleantestdata$BsmtQual<-as.integer(cleantestdata$BsmtQual)

cleantestdata$BsmtFinType1<-as.integer(cleantestdata$BsmtFinType1)
cleantestdata$BsmtFinType1[is.na(cleantestdata$BsmtFinType1)]<-0 
cleantestdata$BsmtFinType1<-as.integer(cleantestdata$BsmtFinType1)

cleantestdata$GarageFinish<-as.integer(cleantestdata$GarageFinish)
cleantestdata$GarageFinish[is.na(cleantestdata$GarageFinish)]<-0 
cleantestdata$GarageFinish<-as.integer(cleantestdata$GarageFinish)

#change fireplaces to either do or dont have one
cleantestdata$Fireplaces<-sub(2,1,cleantestdata$Fireplaces)
cleantestdata$Fireplaces<-sub(3,1,cleantestdata$Fireplaces)
cleantestdata$Fireplaces<-as.integer(cleantestdata$Fireplaces)

#change heating to gas or non gas
cleantestdata$Heating<-sub("GasA",3, cleantestdata$Heating)
cleantestdata$Heating<-sub("GasW",3, cleantestdata$Heating)
cleantestdata$Heating<-sub("Wall",2, cleantestdata$Heating)
cleantestdata$Heating<-sub("Floor",2, cleantestdata$Heating)
cleantestdata$Heating<-sub("OthW",1, cleantestdata$Heating)
cleantestdata$Heating<-sub("Grav",1, cleantestdata$Heating)
cleantestdata$Heating<-as.integer(cleantestdata$Heating)

#clean functional
cleantestdata$Functional<-sub("Typ",1,cleantestdata$Functional)
cleantestdata$Functional<-sub("Min1",0,cleantestdata$Functional)
cleantestdata$Functional<-sub("Min2",0,cleantestdata$Functional)
cleantestdata$Functional<-sub("Mod",0,cleantestdata$Functional)
cleantestdata$Functional<-sub("Maj1",0,cleantestdata$Functional)
cleantestdata$Functional<-sub("Maj2",0,cleantestdata$Functional)
cleantestdata$Functional<-sub("Sev",0,cleantestdata$Functional)
cleantestdata$Functional<-sub("Sel",0,cleantestdata$Functional)
cleantestdata$Functional<-as.integer(cleantestdata$Functional)
#cleantestdata$Functional<-as.factor(cleantestdata$Functional)

#change paveddrive to paved and not paved
cleantestdata$PavedDrive<-sub("Y",2, cleantestdata$PavedDrive)
cleantestdata$PavedDrive<-sub("P",1, cleantestdata$PavedDrive)
cleantestdata$PavedDrive<-sub("N",0, cleantestdata$PavedDrive)
cleantestdata$PavedDrive <- as.integer(cleantestdata$PavedDrive)

#CLEAN THE FENCE
cleantestdata$Fence<-sub("GdPrv", 4, cleantestdata$Fence)
cleantestdata$Fence<-sub("MnPrv", 3, cleantestdata$Fence)
cleantestdata$Fence<-sub("GdWo", 2, cleantestdata$Fence)
cleantestdata$Fence<-sub("MnWw", 1, cleantestdata$Fence)
cleantestdata$Fence<-sub("NA", 0, cleantestdata$Fence)
cleantestdata$Fence[is.na(cleantestdata$Fence)]<-0 
cleantestdata$Fence<-as.integer(cleantestdata$Fence)

#CLEAN THE LANCONTUR
cleantestdata$LandContour<-sub("Lvl",3,cleantestdata$LandContour)
cleantestdata$LandContour<-sub("Bnk",2,cleantestdata$LandContour)
cleantestdata$LandContour<-sub("HLS",1,cleantestdata$LandContour)
cleantestdata$LandContour<-sub("Low",0,cleantestdata$LandContour)
cleantestdata$LandContour<-as.integer(cleantestdata$LandContour)

#CLEAN THE LAND SLOPE
cleantestdata$LandSlope<-sub("Gtl",2,cleantestdata$LandSlope)
cleantestdata$LandSlope<-sub("Mod",1,cleantestdata$LandSlope)
cleantestdata$LandSlope<-sub("Sev",0,cleantestdata$LandSlope)
cleantestdata$LandSlope<-as.integer(cleantestdata$LandSlope)

#CLEAN THE BldgType
cleantestdata$BldgType<-sub("1Fam",2,cleantestdata$BldgType)
cleantestdata$BldgType<-sub("2fmCon",3,cleantestdata$BldgType)
cleantestdata$BldgType<-sub("Duplex",4,cleantestdata$BldgType)
cleantestdata$BldgType<-sub("TwnhsE",5,cleantestdata$BldgType)
cleantestdata$BldgType<-sub("Twnhs",6,cleantestdata$BldgType)
cleantestdata$BldgType<-as.integer(cleantestdata$BldgType)

#CLEAN THE FOUNDATION
cleantestdata$Foundation<-sub("BrkTil",5,cleantestdata$Foundation)
cleantestdata$Foundation<-sub("CBlock",4,cleantestdata$Foundation)
cleantestdata$Foundation<-sub("PConc",3,cleantestdata$Foundation)
cleantestdata$Foundation<-sub("Slab",2,cleantestdata$Foundation)
cleantestdata$Foundation<-sub("Stone",1,cleantestdata$Foundation)
cleantestdata$Foundation<-sub("Wood",0,cleantestdata$Foundation)
cleantestdata$Foundation<-as.integer(cleantestdata$Foundation)

#CLEAN THE SALE CONDI
cleantestdata$SaleCondition<-sub("Normal",1,cleantestdata$SaleCondition)
cleantestdata$SaleCondition<-sub("Abnorml",2,cleantestdata$SaleCondition)
cleantestdata$SaleCondition<-sub("AdjLand",3,cleantestdata$SaleCondition)
cleantestdata$SaleCondition<-sub("Alloca",4,cleantestdata$SaleCondition)
cleantestdata$SaleCondition<-sub("Family",5,cleantestdata$SaleCondition)
cleantestdata$SaleCondition<-sub("Partial",6,cleantestdata$SaleCondition)
cleantestdata$SaleCondition<-as.integer(cleantestdata$SaleCondition)

cleantestdata$CentralAir<-sub("Y",1,cleantestdata$CentralAir)
cleantestdata$CentralAir<-sub("N",0,cleantestdata$CentralAir)
cleantestdata$CentralAir<-as.integer(cleantestdata$CentralAir)

str(cleanedtraindata)
str(cleantestdata)

cleanedtraindata$SalePrice <- as.factor(cleanedtraindata$SalePrice)
#FeatureScaling
FeatureScaling <- function(x) { ((x - min(x)) / (max(x) - min(x))) }

#making each one into a number to be featurescaling(train)
cleanedtraindata$PavedDrive <- as.integer(cleanedtraindata$PavedDrive)
cleanedtraindata$Electrical <- as.integer(cleanedtraindata$Electrical)
cleanedtraindata$MSZoning[is.na(cleanedtraindata$MSZoning)]<-0 
cleanedtraindata$MSZoning <- as.integer(cleanedtraindata$MSZoning)
salepricez <- cleanedtraindata$SalePrice
traindata <- cleanedtraindata[,]

Train_Normalised <- as.data.frame(lapply(traindata, FeatureScaling))
traindata <- Train_Normalised[,]
cleanedtraindata <- traindata[,]
cleanedtraindata$SalePrice <- salepricez
#making each one into a number to be featurescaling(test)
cleantestdata$MSZoning<-as.integer(cleantestdata$MSZoning)
cleantestdata$MSZoning[is.na(cleantestdata$MSZoning)]<-4
cleantestdata$MSZoning<-as.factor(cleantestdata$MSZoning)

cleantestdata$KitchenQual<-as.integer(cleantestdata$KitchenQual)
cleantestdata$KitchenQual[is.na(cleantestdata$KitchenQual)]<-3
cleantestdata$KitchenQual<-as.factor(cleantestdata$KitchenQual)

cleantestdata$GarageCars<-as.integer(cleantestdata$GarageCars)
cleantestdata$GarageCars[is.na(cleantestdata$GarageCars)]<-0
cleantestdata$GarageCars<-as.factor(cleantestdata$GarageCars)

cleantestdata$GarageArea[is.na(cleantestdata$GarageArea)]<-0
cleantestdata$GarageArea <- as.integer(cleantestdata$GarageArea)

cleantestdata$HeatingQC <- as.integer(cleantestdata$HeatingQC)
cleantestdata$MSZoning <- as.integer(cleantestdata$MSZoning)
cleantestdata$CentralAir <- as.integer(cleantestdata$CentralAir)
cleantestdata$KitchenQual <- as.integer(cleantestdata$KitchenQual)
cleantestdata$Electrical <- as.integer(cleantestdata$Electrical)
cleantestdata$GarageCars<- as.integer(cleantestdata$GarageCars)
cleantestdata$Fence<-as.integer(cleantestdata$Fence)
cleantestdata$PavedDrive <- as.integer(cleantestdata$PavedDrive)
testdata <- cleantestdata[,]

Test_Normalised <- as.data.frame(lapply(testdata, FeatureScaling))
testdata <- Test_Normalised[,]
cleantestdata <- testdata[,]
cleantestdata$MSZoning[is.na(cleantestdata$MSZoning)]<-0 

##WHERE THE NAIVE BAYES START
#repeating_sequence = rep.int(seq_len(nrow(cleaneddata)), cleaneddata$MSSubClass)
#House_dataset = cleaneddata[repeating_sequence,]
#House_dataset$MSSubClass = NULL

naivehouseprice <- naiveBayes(SalePrice ~ ., data = cleanedtraindata)

housepred <- predict(naivehouseprice,cleantestdata)
summary(housepred)
write.csv(housepred, "naivepred1.csv")

##MultiLine regression 
lines <- lm(cleanedtraindata$SalePrice ~ cleanedtraindata$MSSubClass+cleanedtraindata$MSZoning)
summary(lines)
plot(lines)
#qplot(cleaneddata$SalePrice, cleaneddata$SaleCondition, data = cleaneddata) +geom_smooth(method = 'lm',formula = y~x)


