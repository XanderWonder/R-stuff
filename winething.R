winetable <- read.csv("Wine_Data_Unclean.csv")

regex <- "\\w+"
word <- "HOOO THE HOOOO"
x <- regexpr(word,regex)
regmatches(word,x)

regoutput <- c(regmatches(winetable$variety_and_region,regexpr("/ .+",winetable$variety_and_region)))
fix <- sub("/ ","",regoutput)
winetable$Region <- fix

regio2 <- c(regmatches(winetable$Region,regexpr("/.+",winetable$Region)))
fix2 <- sub("/ ","",regio2)
winetable$subregion <- fix2

fix3 <- sub("/.+","",winetable$Region)
winetable$Region <- fix3

fix4 <- sub("/.+","",winetable$variety_and_region)
winetable$variety_and_region <- fix4

colnames(winetable)[10] = "Variety"
colnames(winetable)[12] = "SubRegion"


cleantime <- function(regoutput){
  regoutput <- c(regmatches(winetable$variety_and_region,regexpr("/ .+",winetable$variety_and_region)))
  fix <- sub("/ ","",regoutput)
  winetable$Region <- fix
  
  regio2 <- c(regmatches(winetable$Region,regexpr("/.+",winetable$Region)))
  fix2 <- sub("/ ","",regio2)
  winetable$SubRegion <- fix2
  
  fix3 <- sub("/.+","",winetable$Region)
  winetable$Region <- fix3
  
  fix4 <- sub("/.+","",winetable$variety_and_region)
  winetable$variety_and_region <- fix4
  colnames(winetable)[10] = "Variety"
  
  num <- readline(prompt = "Enter number between 1 to 12: ")
  name <-readline(prompt = "Enter the name: ")
  number <- as.integer(num)
  colnames(winetable)[number] = name
  
  return(winetable)
  
}
newwine <- cleantime(regoutput)
