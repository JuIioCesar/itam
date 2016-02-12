library(shiny)

# textareaInput <- function(inputId, label, value="", placeholder="", rows=2){
#   tagList(
#     div(strong(label), style="margin-top: 5px;"),
#     tags$style(type="text/css", "textarea {width:100%; margin-top: 5px;}"),
#     tags$textarea(id = inputId, placeholder = placeholder, rows = rows, value=value))
# }

shinyUI(
  verticalLayout(
    
    titlePanel("Etiquetador de contenido editorial BM25"),
  
    h4("Ingrese el contenido que será etiquetado"),
    
    tags$textarea(id="input.content", rows=15, cols=100, value=""),
      
    actionButton(inputId="tag.button", label="Etiquetar"),
    
    actionButton(inputId="refine.button", label="Refinar"),
    
    hr(),
    
    h4("Etiquetas sugeridas"),

    dataTableOutput("suggested.tags"),
    
    hr(),
    
    h4("Refinado"),
    
    dataTableOutput("refined.tags")
    
  )
)