library(shiny)
library(tm)
library(wordcloud)
library(memoise)
library(dplyr)

setwd("C:/Users/Admin/Documents/WordCloudWork")

review <- read.csv("amazon_alexa2.csv", stringsAsFactors = FALSE)
reviewsum <- read.csv("summary2.csv")
reviewQuote <- review
getTermMatrix <- memoise(function(sorter){
  myCorpus = Corpus(VectorSource(sorter))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "I", "the", "and", "but", "alexa", "echo", "amazon"))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  
  return(myCorpus)
})

review$verified_reviews <- getTermMatrix(review$verified_reviews)

variantsorter <- function(vreview){
  specificvariantsubset <- subset(review,variation == vreview)
  variantreviews <- specificvariantsubset$verified_reviews
  return(variantreviews)
}

ratingsorter <- function(freview){
  specificratingsubset <- subset(review,rating == freview)
  ratingreviews <- specificratingsubset$verified_reviews
  return(ratingreviews)
}

shinyApp(ui = ui, server = server)

