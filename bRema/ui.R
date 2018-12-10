library(shiny)
library(plotly)
library(lubridate)

library(devtools)
install_github('cunybpl/bRema')
library(bRema)

ui <- fluidPage(
  titlePanel('bRema Dashboard'),
  sidebarPanel(
    fileInput("file1", "Choose Utility File", multiple = FALSE, accept = c("text/csv","text/comma-separated-values,text/plain", ".csv")),
    uiOutput("first"),
    uiOutput("second"),
    fluidRow(
        column(4, actionButton(inputId = "prevBin", label = "Previous")),
        column(4, actionButton(inputId = "nextBin", label = "Next"))
        #column(2, checkboxInput(inputId = 'aggregate', label = "Aggregate", value = FALSE, width = NULL))
    ),
    fluidRow(
      column(width= 4, checkboxInput('step', 'Multiple Step', value = FALSE, width = NULL)),
      column(width= 6, checkboxInput('sqft', 'Normalized by sqft', value = FALSE, width = NULL))
    ),
    selectInput('model', 'Choose model', c('2P','3PC', '3PH', '4P', '5P'), selected = NULL, multiple = FALSE,selectize = TRUE, width = NULL, size = NULL)
  ),
  mainPanel(
    tabsetPanel(
      tabPanel('Batch Run', 
        br(),
        br(),
        fluidRow(
        column(2, actionButton(inputId = "run", label = "Batch Run")),
        column(4, checkboxInput(inputId = "lean_flag", label = "Perform lean analysis ranking", value = TRUE, width = NULL))
        ),
        h5("Depending on your dataset it may take time to run. When it is completed, a notification will show up at the right corner of the screen."),
        br(),
        fluidRow(
          column(5, textInput(inputId = 'best_name', label = 'Best Model File Name', value = "best_model", width = NULL, placeholder = NULL), textInput(inputId = 'post_name', label = 'Post Model File Name', value = "post_model", width = NULL, placeholder = NULL)),
          column(3, br(), downloadButton("best_download", "Download Best Model"), br(), br(), br(), downloadButton("post_download", "Download Post Model"))
          )
        ),
      tabPanel('Individual Model', 
      h3("Time Series", align = 'center'),
      plotlyOutput('plot3'),
      fluidRow(
        column(6, h3('Parameters'),
          fluidRow(
            column(6, tableOutput('params'))
          )
        ), column(6, h3('Stats'),
          fluidRow(
            column(6, tableOutput('stats'))
          )
        )
      ),
      h3("Parameter Model Graph", align = "center"),
      plotlyOutput('model_figure')
    )
  )
  )
)