library(shiny)
library(plotly)

ui <- fluidPage(
  headerPanel('Shiny Interval Data Tool'),
  sidebarPanel(
    fileInput("interval_file", "Choose Interval Data CSV File", accept = c("text/csv","text/comma-separated-values,text/plain", ".csv")),
    fileInput("oat_file", "Choose Temperature CSV File", accept = c("text/csv","text/comma-separated-values,text/plain", ".csv")),
    uiOutput("meter_id"),
    uiOutput("year_id")
  ),
  mainPanel(
    tabsetPanel(
        tabPanel("App Info",
            br(),
            h5('yaddy yadda')
        ),
        tabPanel("Weekly Graph",
            h3("Weekly Graphs", align = "center"),
            br(),
            uiOutput('elec_multi_plot'),
            br()
        ),
        tabPanel('Timeseries',
          br(),
          plotlyOutput('timeseries_plot')
        ),
        tabPanel('Avg Weekly load profile',
          br(),
          plotlyOutput('avg_weekload_plot')
          ),
        tabPanel('Msg To Tanya',
          br(),
          img(src='plotly.gif', align = "center"),
          br(),
          br(),
          img(src='matplotlib.gif', align = "center")
          )
    )
  )
)