library(shiny)
library(plotly)

ui <- fluidPage(
  headerPanel('Shiny Interval Data Tool'),
  sidebarPanel(
    fileInput("file1", "Choose Interval Data CSV File", accept = c("text/csv","text/comma-separated-values,text/plain", ".csv")),
    uiOutput("month_id"),
    uiOutput("meter_id"),
    actionButton(inputId = "click", label = "Graph")
  ),
  mainPanel(
    tabsetPanel(
        tabPanel("App Info",
            br(),
            h5('To get started, Inteval Data CSV must be uploaded.'),
            h5('Since Interval Data CSV File Needs to be in a certain format,  please clean the data in Excel prior to using this visualization tool.'),
            h5('The format is as follows:'),
            tags$ul(
                tags$li("Remove metdata above and below the interval data usage."),
                tags$li("All dates must be in m/d/yyyy h:mm format."),
                tags$li("All usage must be in 'Number' format."),
                tags$li("Add column for Aggregation that adds all meter data as another column at the end."),
                tags$li("Ensure only one year goes in at a time (Starting January 1 to December 31)") 
            ),
            h5('Time Series: If there is only one energy type or information of bdbid is missing, it will show either a blank graph or a horizontal line at zero.'),
            h5('Parameter Model: If the required file is not uploaded or there are missing information, a blank graph will be shown'),
            h5('Other tables, graphs and visulizations: If required file are not uploaded or missing, nothing will be shown.'),
            h5("If there is something wrong the interval data file or the wrong file is uploaded, error message 'An error has occurred. Check your logs or contact the app author for clarification.' will be displayed.")
        ),
        tabPanel("Weekly Graph",
            h3("Week 1", align = "center"),
            plotlyOutput('plot1'),
            br(), br(),br(),br(),br(),br(),
            h3("Week 2", align = "center"),
            plotlyOutput('plot2'),
            br(), br(),br(),br(),br(),br(),
            h3("Week 3", align = "center"),
            plotlyOutput('plot3'),
            br(), br(),br(),br(),br(),br(),
            h3("Week 4", align = "center"),
            plotlyOutput('plot4'),
            br()
        ),
        tabPanel("Monthly",
            h3("Month", align = "center"),
            plotlyOutput('plot5'),
            br()
        ),
        tabPanel("Heat Map",
        h3("Heat Map", align = "center"),
        plotlyOutput('plot6'),
        plotlyOutput('plot7')
        )
    )
  )
)