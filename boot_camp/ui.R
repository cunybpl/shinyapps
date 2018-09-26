library(shiny)
library(ggplot2)
library(devtools)
#install_github('tinnaing347/bRema/bRema')
library(bRema)
library(plotly)

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
    )
  ),
  mainPanel(
  tabsetPanel(
    tabPanel('Elec',
      h3('2P Graph'),
      plotlyOutput('model_figure_2p'),
      uiOutput('range_2p'),
      h3('Parameters'),
      tableOutput('params_2p'),
      h3('Stats'),
      tableOutput('stats_2p'),
      tableOutput('test_2p'),
      h3('3PC Graph'),
      plotlyOutput('model_figure_3pc'),
      uiOutput('range_3pc'),
      h3('Parameters'),
      tableOutput('params_3pc'),
      h3('Stats'),
      tableOutput('stats_3pc'),
      tableOutput('test_3pc'),
      h3('3PH Graph'),
      plotlyOutput('model_figure_3ph'),
      uiOutput('range_3ph'),
      h3('Parameters'),
      tableOutput('params_3ph'),
      h3('Stats'),
      tableOutput('stats_3ph'),
      tableOutput('test_3ph'),
      h3('4P Graph'),
      plotlyOutput('model_figure_4p'),
      uiOutput('range_4p'),
      h3('Parameters'),
      tableOutput('params_4p'),
      h3('Stats'),
      tableOutput('stats_4p'),
      tableOutput('test_4p'),
      h3('5P Graph'),
      plotlyOutput('model_figure_5p'),
      fluidRow(
        column(6, uiOutput('range_5p1')),
        column(6, uiOutput('range_5p2'))
      ),
      h3('Parameters'),
      tableOutput('params_5p'),
      h3('Stats'),
      tableOutput('stats_5p'),
      tableOutput('test_5p')
      ),
    tabPanel('Fuel',
      h3('2P Graph'),
      plotlyOutput('model_figure_2p2'),
      uiOutput('range_2p2'),
      h3('Parameters'),
      tableOutput('params_2p2'),
      h3('Stats'),
      tableOutput('stats_2p2'),
      tableOutput('test_2p2'),
      h3('3PC Graph'),
      plotlyOutput('model_figure_3pc2'),
      uiOutput('range_3pc2'),
      h3('Parameters'),
      tableOutput('params_3pc2'),
      h3('Stats'),
      tableOutput('stats_3pc2'),
      tableOutput('test_3pc2'),
      h3('3PH Graph'),
      plotlyOutput('model_figure_3ph2'),
      uiOutput('range_3ph2'),
      h3('Parameters'),
      tableOutput('params_3ph2'),
      h3('Stats'),
      tableOutput('stats_3ph2'),
      tableOutput('test_3ph2'),
      h3('4P Graph'),
      plotlyOutput('model_figure_4p2'),
      uiOutput('range_4p2'),
      h3('Parameters'),
      tableOutput('params_4p2'),
      h3('Stats'),
      tableOutput('stats_4p2'),
      tableOutput('test_4p2'),
      h3('5P Graph'),
      plotlyOutput('model_figure_5p2'),
      fluidRow(
        column(6, uiOutput('range_5p12')),
        column(6, uiOutput('range_5p22'))
      ),
      h3('Parameters'),
      tableOutput('params_5p2'),
      h3('Stats'),
      tableOutput('stats_5p2'),
      tableOutput('test_5p2')
      )
    )
  )
)
