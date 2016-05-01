library(shiny)
library(shinydashboard)

# textareaInput <- function(inputId, label, value="", placeholder="", rows=2){
#   tagList(
#     div(strong(label), style="margin-top: 5px;"),
#     tags$style(type="text/css", "textarea {width:100%; margin-top: 5px;}"),
#     tags$textarea(id = inputId, placeholder = placeholder, rows = rows, value=value))
# }

###
shinyUI(
  fluidPage(
    titlePanel("Okapi BM25"),

    sidebarLayout(
      sidebarPanel(
        actionButton(inputId="tag.button", label="Etiquetar"),
        actionButton(inputId="refine.button", label="Podar etiquetas")#,
        #actionButton(inputId="clean.button", label="Limpiar")
      ),

      mainPanel(
        tabsetPanel(
          tabPanel("Contenido",
                   tags$textarea(id="input.content", rows=15, cols=100, value="")
          ),
          tabPanel("Etiquetas recomendadas", dataTableOutput("suggested.tags")),
          tabPanel("Etiquetas podadas", dataTableOutput("pruned.tags"))
        )
      )
    )

  )
)

#####
# dashboardPage(
#   dashboardHeader(title="BM25 Etiquetador"),
#   dashboardSidebar(
#     sidebarMenu(
#       menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
#       menuItem("Widgets", tabName = "widgets", icon = icon("th"))
#     ),
#     actionButton(inputId="tag.button", label="Etiquetar"),
#     actionButton(inputId="refine.button", label="Podar etiquetas")#,
#     #actionButton(inputId="clean.button", label="Limpiar")
#     
#   ),
#   dashboardBody(
#     tabItems(
#       tabItem(
#         tabName="dashboard",
#         fluidRow(
#           column(12,
#             box("Contenido", width=12,
#                      tags$textarea(id="input.content", rows=15, cols=100, value=""))
#             )
#           )
#         ),
#       tabItem(
#         tabName="widgets",
#         fluidRow(
#           column(12,
#             box("Etiquetas recomendadas", width=12,
#                 dataTableOutput("suggested.tags")
#             )
#           )
#         )
#       )
#       )
#      )
#    )
