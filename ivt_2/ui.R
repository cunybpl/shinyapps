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
            h5("All graphs in this shiny app are created using plotly. This message is not sponsored by plotly. I just do not like matplotlib."),
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
          h5('Day Count'),
          tableOutput('day_count'),
          h5('Hour Count'),
          tableOutput('hour_count'),
          textOutput('approx_alert'),
          tableOutput('approx_count')
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
          h5('Average Weekly load Graph'),
          plotlyOutput('avg_weekload_plot'),
          br(),
          h5('Weekly Load Profile'),
          plotlyOutput('all_weekload_plot_2d'),
          br(),
          plotlyOutput('color_heat_plot', height = '100px', width = '500px'),
          plotlyOutput('all_weekload_plot_3d'),
          br()
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