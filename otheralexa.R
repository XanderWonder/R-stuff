install.packages("shiny")
install.packages("tm")
install.packages("wordcloud")
install.packages("memoise")
library(shiny)
library(tm)
library(wordcloud)
library(memoise)
library(dplyr)

review <- read.csv("C:/Users/Admin/Documents/amazon_alexa.csv")

reviw_test <- review$verified_reviews

myCorpus = Corpus(VectorSource(reviw_test))
myCorpus = tm_map(myCorpus, content_transformer(tolower))
myCorpus = tm_map(myCorpus, removePunctuation)
myCorpus = tm_map(myCorpus, removeNumbers)
myCorpus = tm_map(myCorpus, removeWords,
                  c(stopwords("SMART"), "I", "the", "and", "but","Echo","Alexa","echo","alexa"))
review$verified_reviews <- myCorpus

getTermMatrix <- memoise(function(butto){
  myCorpus = Corpus(VectorSource(butto))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "I", "the", "and", "but","Echo","Alexa","echo","alexa"))
  mew <- myCorpus
  return(mew)
})
butto <- function(vreview){
  tope <- subset(review,variation == vreview)
  tope_t <- tope$verified_reviews
  return(tope_t)
}

ui <- fluidPage(
  titlePanel("Amazon Alexa Sidebar"),
  sidebarLayout(
  sidebarPanel(
  selectInput(inputId ="varia", label ="Alexa Variations: ", choices = review$variation),
  sliderInput(inputId = "freq",
              label = "choose number of times it appers",
              value = 100,min = 25, max = 100)),
  mainPanel(
    tabsetPanel(
      tabPanel("Word Cloud for all reviews",plotOutput("plot")),
      tabPanel("Word Cloud with Variants and reviews",plotOutput("plot2"))
      
      )
    )
  )
)
server <- function(input,output,session) {
  terms <- reactive({
    input$update
    isolate({
      withProgress({
        setProgress(message = "Processing corpus")
      getTermMatrix(input$varia)
      })
    })
  })
  output$plot <- renderPlot({
    title <- "amazon review with all"
    wordcloud(review$verified_reviews,random.order = FALSE, min.freq = input$freq , 
              colors = brewer.pal(8,"Accent"),scale = c(5,0.5))
    output$plot2 <- renderPlot({
      title <- "amazon review with variants"
      wordcloud(getTermMatrix(butto(vreview = input$varia)),random.order = FALSE, min.freq = input$freq , 
                colors = brewer.pal(8,"Dark2"))
    })
  })
}
shinyApp(ui = ui, server = server)
