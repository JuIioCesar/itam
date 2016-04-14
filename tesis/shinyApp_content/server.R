suppressPackageStartupMessages(library(shiny))

source("../tags_manipulation.R")
source("../suggestedTags.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  ntext <- eventReactive(input$tag.button, {
    input$input.content
  })
  
  output$suggested.tags <- renderDataTable({
    getTags(ntext())
  })
  
  pruning <- eventReactive(input$refine.button, {
    pruneHierarchy()
  })
  
  output$refined.tags <- renderDataTable({
    pruning()
  })
  

  
  
})