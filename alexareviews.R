install.packages("tm")
install.packages("wordcloud")
install.packages("memoise")
install.packages("shiny")
library(tm)
library(wordcloud)
library(memoise)
library(shiny)

setwd("C:/Users/Admin/Documents")
reviews <- read.csv("alexa .csv")

variation <- as.data.frame(reviews$variation)

review_v <- as.data.frame(reviews$verified_reviews)

myCorpus = Corpus(VectorSource(review_v))
myCorpus = tm_map(myCorpus, content_transformer(tolower))
myCorpus = tm_map(myCorpus, removePunctuation)
myCorpus = tm_map(myCorpus, removeNumbers)
myCorpus = tm_map(myCorpus, removeWords,
                  c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but"))

ui <- fluidPage( titlePanel("Alexa Reviews"), 
                 sidebarPanel(
                   sidebarPanel(
                     selectInput("Variations","Variations", choices = variation), 
                     width = 1000, actionButton("update", "change"),
                     sliderInput("frq","rating for alexa", min = 1, max = 5, value = 5)
                  )
                )
              )

server <- function(input, output){
  output$plot <- renderPlot({
    wordcloud(review_v, reviews$rating, scale=c(4,0.5),
                  min.freq = 1, max.words= 5,
                  colors=brewer.pal(8, "Dark2"))
  })
}

shinyApp(ui = ui, server = server)
