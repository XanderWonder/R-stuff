server <- function(input,output,session) {
  terms <- reactive({
    input$update
    isolate({
      withProgress({
        setProgress(message = "Processing corpus")
      })
    })
  })
  output$plotrating <- renderPlot({
    title <- "amazon review"
    
    wordcloud(getTermMatrix(ratingsorter(freview = input$rating)),min.freq = input$freq , max.words=input$max,  
              colors = brewer.pal(8,"Dark2"))
    
    output$myText <- renderPrint({
      
      starrating<-as.integer(input$rating)
      specificreviews<-reviewQuote[reviewQuote$rating == starrating,]
      sample(specificreviews$verified_reviews,3)
      
    })
    
  })
  output$plotvariant <- renderPlot({
    title <- "amazon review"
    
    wordcloud(getTermMatrix(variantsorter(vreview = input$varia)), min.freq = input$freq , max.words=input$max, 
              colors = brewer.pal(8,"Dark2"))
    
    output$myImage<-renderImage({
     
      filename <- normalizePath(file.path('C:/Users/Admin/Documents/WordCloudWork/echoimages',
                                          paste(input$varia, '.jpg', sep='')))
      
      # Return a list containing the filename and alt text
      list(src = filename,
           alt = paste("Image number", input$varia))
      
    }, deleteFile = FALSE)
    
  })
  output$plotHist <- renderPlot({
    title <- "amazon most used words"
    hist(review$rating,col="green",xlab="Star Rating", border = "blue")
  }) 
  output$plotall <- renderPlot({
    title <- "All Reviews"
    wordcloud(review$verified_reviews, min.freq = input$freq,max.words=input$max,colors=brewer.pal(8, "Dark2"))
  })
  output$summary <- renderTable({
    head(reviewsum)
  })
  output$quote <- renderPrint({
    sample(reviewQuote$verified_reviews,4)
  })

}
 