library(shiny)
library(plotly)

ui <- fluidPage(
  headerPanel('Shiny Interval Data Tool'),
  sidebarPanel(
    fileInput("interval_file", "Choose Interval Data CSV File", accept = c("text/csv","text/comma-separated-values,text/plain", ".csv"))
  ),
  mainPanel(
    h5("yeah"),
    textOutput("blah"),
    tableOutput("bema_out")
  )
)