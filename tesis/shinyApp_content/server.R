suppressPackageStartupMessages(library(shiny))




# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  ntext <- eventReactive(input$tag.button, {
    input$input.content
  })
  
  output$suggested.tags <- renderText({
    ntext()
  })
    
    
})