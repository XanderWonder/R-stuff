ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      titlePanel("Alexa Reviews Analytics"),
      selectInput("rating", "Choose rating:",
                  choices = review$rating),
                  value=3,
      actionButton("update", "Change"),
      selectInput("varia", "Choose a variant:",
                  choices = review$variation),
                  value="Black",
      actionButton("update", "Change"),
      
      sliderInput(inputId = "freq",
                  label = "Choose the minimum frequency of the word",
                  value = 100,min = 20, max =50),
      sliderInput("max",
                  "Maximum Number of Words:",
                  min = 1,  max = 100,  value = 50)),
    mainPanel(
      tabsetPanel(
        tabPanel("Overall", plotOutput("plotall")),
        tabPanel("Ratings", plotOutput("plotrating"), verbatimTextOutput("myText")),
        tabPanel("Variants", plotOutput("plotvariant"), imageOutput("myImage"), align="center"),
        tabPanel("quotes + information", tableOutput("summary"),verbatimTextOutput("quote")),
        tabPanel("Rating Frequency",plotOutput("plotHist"))
      )
    )
  )
)
