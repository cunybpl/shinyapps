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
            h3("File Requirement"),
            tableOutput('data_req_df'),
            br(),
            h4('Input Files Format'),
            h5('Interval Data CSV'),
            tags$ul(
                tags$li("Column Names: 'Interval End', <meter_1>, <meter_2>,...,<meter_n>"),
                tags$li("'Interval End' column will be renamed to 'date'"),
                tags$li("Format of 'date' column: yyyy-mm-dd HH:MM. For example, 2017-04-31 12:00:00. Note: Second is optional but they need to be zero."),
                tags$li("Sometimes, there may be extra column of rownames. In that case, 'date' will be automatically choosen as 'Meter Type'. Thus, one will have to change meter type.")
            ),
            h5('Temperature File CSV'),
            tags$ul(
                tags$li("PLEASE BE CAREFUL THAT THERE IS NO EXTRA COLUMN OTHER THAN COLUMN MENTIONED IN COLUMN NAMES", style = "color:red"),
                tags$li("Column Names: 'date', 'daylight', 'OAT'"),
                tags$li("Format of 'date' column: yyyy-mm-dd HH. For example, 2017-04-31 12:00:00. Note: Minute and second is optional but they need to be zero.")
            ),
            br()
        ),
        tabPanel('Data Info',
          br(),
          span(textOutput('count_header'), size = '24pt'),
          tableOutput('approx_count_df'),
          h5('Note: Missing points in data sets are interpolated (approximated) by linear interpolation.'),
          span(textOutput('na_point_note'), size = '24pt'),
          tableOutput('data_count_df')
          ),
        tabPanel("Daily Load Profile Graph",
            h3("Daily Load Profile Per Week", align = "center"),
            br(),
            uiOutput('elec_multi_plot'),
            br()
        ),
        tabPanel('Time Series',
          br(),
          plotlyOutput('timeseries_plot')
        ),
        tabPanel('Average Weekly Load Profile',
          br(),
          h5('Average Weekly Load Graph'),
          plotlyOutput('avg_weekload_plot'),
          h5('The shaded region (and its boundary line) in the above graph represents upper and lower confidence intervals.'),
          br(),
          h5('Weekly Load Profile'),
          plotlyOutput('all_weekload_plot_2d'),
          br(),
          plotlyOutput('color_heat_plot', height = '100px', width = '500px'),
          plotlyOutput('all_weekload_plot_3d'),
          br(),
          plotlyOutput('color_heat_plot2', height = '100px', width = '500px'),
          br()
          )
    )
  )
)