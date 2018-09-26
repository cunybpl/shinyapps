library(shiny)
library(DT)


ui <- fluidPage(
  headerPanel('Utility Files Generator'),
  sidebarPanel(
    fileInput("file2", "Choose raw cosumption CSV File", accept = c("text/csv","text/comma-separated-values,text/plain", ".csv")),
    fileInput("file3", "Choose OAT CSV File", accept = c("text/csv","text/comma-separated-values,text/plain", ".csv")),
    numericInput(inputId = 'fiscal_year' , 'Fiscal Year', value = 2017, min = NA, max = NA, step = NA, width = NULL),
    numericInput(inputId = 'points' , 'Number of points', value = 24, min = NA, max = NA, step = NA, width = NULL),
    dateRangeInput("daterange1", "Date range:",
                 start = "2001-01-01",
                 end   = "2010-12-31")
  ),
  mainPanel(
    tabsetPanel(
        tabPanel("App Info",
            br(),
            h5("To get started, please upload your raw consumption csv file. If OAT file is not uploaded default OAT file will be used."),
            h5('Please make sure that there are no empty or NA columns in the input file.'),
            h5('Date format must be: mm/dd/yy HH:MM:SS.'),
            h5("Please name all date time (x-axis) columns as 'datetime<other_name>'. For example, 'datetime1', 'datetime2'..."),
            h5("The columns of the input file should be in the format: datetime<other_name>, <y-axis 1_1>, ..., <y-axis n_1>, datetime<other_name>, <y_axis 1_2>, ..., <y_axis n_2>, datetime<other_name>,..."),
            h5('Depending on the file size, it may take some time to load the graph.'),
            h5("For 'Custom Date Range' tab, adjust date range in the side panel."),
            br(),
            br()
        ),
        tabPanel("Main",
        	br(),
        	fluidRow(column(4, actionButton(inputId = "run", label = "Generate Utility File"))),
        	DT::dataTableOutput("util_table")
        )
    )
  )
)