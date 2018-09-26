library(shiny)
library(plotly)

ui <- fluidPage(
  headerPanel('Time Series'),
  sidebarPanel(
    fileInput("file2", "Choose your input CSV File", accept = c("text/csv","text/comma-separated-values,text/plain", ".csv")),
    textInput(inputId = 'plot_title', 'Plot Title', value = "", width = NULL, placeholder = NULL),
    textInput(inputId = 'y_title', 'Y-axis Title', value = "", width = NULL, placeholder = NULL),
    selectInput(inputId = 'interval_type', 'Interval Type', c('Second','Minute'), selected = 'Minute', multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
    numericInput(inputId = 'interval' , 'Interval', value = 0, min = NA, max = NA, step = NA, width = NULL),
    dateRangeInput("daterange1", "Date range:",
                 start = "2001-01-01",
                 end   = "2010-12-31"),
    textInput(inputId = 'start_time', 'Start Time', value = "00:00:00", width = NULL, placeholder = NULL),
    textInput(inputId = 'end_time', 'End Time', value = "00:00:00", width = NULL, placeholder = NULL)
  ),
  mainPanel(
    tabsetPanel(
        tabPanel("App Info",
            br(),
            h5('To get started, please upload your csv file.'),
            h5('Please make sure that there are no empty or NA columns in the input file.'),
            h5('Date format must be: mm/dd/yy HH:MM:SS.'),
            h5("Please name all date time (x-axis) columns as 'datetime<other_name>'. For example, 'datetime1', 'datetime2'..."),
            h5("The columns of the input file should be in the format: datetime<other_name>, <y-axis 1_1>, ..., <y-axis n_1>, datetime<other_name>, <y_axis 1_2>, ..., <y_axis n_2>, datetime<other_name>,..."),
            h5('Depending on the file size, it may take some time to load the graph.'),
            h5("For 'Custom Date Range' tab, adjust date range in the side panel."),
            br(),
            br()
        ),
        tabPanel("Timeseries",
            plotlyOutput('plot1')
        ),
        tabPanel("Custom Date Range",
            plotlyOutput('plot2')
        )
    )
  )
)