myfunc <- function(num1,num2,bool){
   if(TRUE){
    sumnum = num1 + num2
    print(sumnum)
  }else{
    sumnum = num1 * num2
    print(sumnum)
  }
  if (num1 == 0 | num2 == 0){
    return(num1 + num2)
  }
}
for( i in 1:10){
  myfunc(i,10,TRUE)
}

eval <- function(func, data){
  func(data)
}
eval(function(x){cars[1:1,1:1]}, 0)

blackjack <- function(card1,card2){
  if(card1 <= 21 & card2 <= 21){
    if(card1 > card2){
      print(card1)
    }else{
      print(card2)
    }
  }else{
    return(0)
  } 
}
blackjack(22,12)

unisum <- function(num1,num2,num3){
  sum = num1 + num2 + num3
  if(num1 == num2 && num2 == num3){
    sum = 0
  }else{
    if(num1 == num2){
      sum = sum - (num1 + num2)
    }
    if(num1 == num3){
      sum = sum - (num1 + num3)
    }
    if(num2 == num3){
      sum = sum - (num2 + num3)
    }
  }
  return(sum)
}
unisum(2,2,2)

toohot <- function(temperature,isSummer){
  if(temperature >= 60 & temperature <=100 & isSummer == TRUE){
      return(TRUE)
  }else{
    return(FALSE)
  }
}
toohot(50,TRUE)

leapyear <- function(year){
  if(year%%4 == 0 | year&&400 == 0 | year%%100 > 1){
    print("TIS A LEAP YEAR")
  }else{
    print("NOT LEAP YEAR")
  }
}
leapyear(1960)

tvector <- c("2,4,6,8,10,12")
write.csv(tvector, "tcsv.csv")

table1 <- read.csv("tcsv.csv")
tvector <- c("1,3,5,7")
write.csv(tvector, "odd.csv")
table1 <-read.csv("odd.csv")

CO2$Plant
class(CO2$Plant)
mean_uptake <- mean(CO2$uptake)
boxplot(CO2$uptake ~ CO2$Type)
quebec_CO2 <-CO2[CO2$Type == "Quebec",]
missipi_CO2 <-CO2[CO2$Type == "Mississippi",]

meancheck <- function(x,y){
  if(x<y){
    print("QUEBEC IS HIGH")
  }else{
    print("MISSISSIPPI IS HIGH")
  }
}
meanquebec <- mean(quebec_CO2$uptake)
meanmissi <- mean(missipi_CO2$uptake)
meancheck(meanquebec,meanmissi)


data("OrchardSprays")
summary(OrchardSprays)

max_decrease <- OrchardSprays[OrchardSprays$decrease == max(OrchardSprays$decrease), ]
boxplot(OrchardSprays$decrease ~ OrchardSprays$treatment)

data("ChickWeight")
summary(ChickWeight)
par(mfrow=c(2,2))
boxplot(ChickWeight$weight ~ ChickWeight$Diet, main = "chicken weight with chicken diet", col = "blue")
hist(ChickWeight$Chick , main="Chicken Weight")
ChickWeight$Diet <- as.numeric()
