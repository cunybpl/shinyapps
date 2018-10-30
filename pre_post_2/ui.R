library(shiny)
library(plotly)

ui <- fluidPage(
  headerPanel('Pre/Post Retrofit'),
  sidebarPanel(
    fileInput("file2", "Choose Utility CSV File", accept = c("text/csv","text/comma-separated-values,text/plain", ".csv")),
    fileInput("file1", "Choose Best Model CSV File", accept = c("text/csv","text/comma-separated-values,text/plain", ".csv")),
    fileInput("file6", "Choose Building Information CSV File", accept = c("text/csv","text/comma-separated-values,text/plain", ".csv")),
    fileInput("file5", "Choose Post Modeller CSV File", accept = c("text/csv","text/comma-separated-values,text/plain", ".csv")),
    fileInput("file3", "Choose Saving Uncertainty CSV File", accept = c("text/csv","text/comma-separated-values,text/plain", ".csv")),
    fileInput("file4", "Choose Total Savings CSV File", accept = c("text/csv","text/comma-separated-values,text/plain", ".csv")),
    uiOutput("first"),
    fluidRow(
        column(4, actionButton(inputId = "prevBin", label = "Previous")),
        column(4, actionButton(inputId = "nextBin", label = "Next"))
        #column(2, checkboxInput(inputId = 'aggregate', label = "Aggregate", value = FALSE, width = NULL))
    )
  ),
  mainPanel(
    tabsetPanel(
        tabPanel("App Info",
            br(),
            h4("PLEASE DO NOT CLICK ANYTHING ON THIS PAGE WHEN SHINY IS LOADING DROPDOWN MENUS."),
            h5("Please wait until 'Choose Facility' dropdown shows up."),
            tableOutput('help'),
            h5("To get started, upload BEMA CSV output file to dropbox 'shiny/retrofit_output/{project_ouput}/{project}/{run_date}' folder. Then refresh this page again and just wait for shiny to load drop down menus. If files have already been uploaded to dropbox, just wait for shiny to load."),
            h5("Please make sure every file name is in this format: {specific filename}_{project}_{run_date}.csv. For example, 'utility_ace_2018-04-04.csv'"),
            h5('Time Series: If there is only one energy type or information of bdbid is missing, it will show either a blank graph or a horizontal line at zero.'),
            h5('Parameter Model: If the required file is not uploaded or there are missing information, a blank graph will be shown'),
            h5('Other tables, graphs and visulizations: If required files are not uploaded or missing, nothing will be shown.')
        ),
        tabPanel("Graph and Tables for Elec",
            h3("Building Information"),
            br(),
            fluidRow(
                column(6, tableOutput('binfo_df1')),
                column(width = 6, tableOutput('binfo_df2'))
            ),
            h3("Time Series", align = "center"),
            plotlyOutput('plot3'),
            br(),
            h3("Parameter Model Graph for Pre and Post", align = "center"),
            plotlyOutput('plot2'),
            tableOutput('params_df'),
            tableOutput('stat_df'),
            br(),
            tableOutput('post'),
            br(),
            h3("Adjusted Baseline", align = "center"),
            plotlyOutput('plot1'),
            br(),
            h3("Gross Saving"),
            tableOutput('savings'),
            br()
        ),
        tabPanel("Graph and Tables for Fuel",
            h3("Building Information"),
            br(),
            fluidRow(
                column(6, tableOutput('binfo_df12')),
                column(width = 6, tableOutput('binfo_df22'))
            ),
            h3("Time Series", align = "center"),
            plotlyOutput('plot32'),
            br(),
            h3("Parameter Model Graph for Pre and Post", align = "center"),
            plotlyOutput('plot22'),
            tableOutput('params_df2'),
            tableOutput('stat_df2'),
            br(),
            tableOutput('post2'),
            br(),
            h3("Adjusted Baseline", align = "center"),
            plotlyOutput('plot12'),
            br(),
            h3("Gross Saving"),
            tableOutput('savings2'),
            br()
        ),
        tabPanel("Multi Elec",
            br(),
            uiOutput('plots')
        ),
        tabPanel("Multi Fuel",
            br(),
            uiOutput('plots2')
        )
    )
  )
)