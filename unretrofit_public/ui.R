library(shiny)
library(plotly)

ui <- fluidPage(
  headerPanel('Unretrofit Graph'),
  sidebarPanel(
    fileInput("file2", "Choose Utility CSV File", accept = c("text/csv","text/comma-separated-values,text/plain", ".csv")),
    fileInput("file1", "Choose Best Model CSV/All Model CSV File", accept = c("text/csv","text/comma-separated-values,text/plain", ".csv")),
    fileInput("file7", "Choose Building Info CSV File", accept = c("text/csv","text/comma-separated-values,text/plain", ".csv")),
    fileInput("file5", "Choose Post Modeller CSV File", accept = c("text/csv","text/comma-separated-values,text/plain", ".csv")),
    fileInput("file9", "Choose Lean Rank CSV File", accept = c("text/csv","text/comma-separated-values,text/plain", ".csv")),
    fileInput("file6", "Choose CO2 EUI Ranking CSV File", accept = c("text/csv","text/comma-separated-values,text/plain", ".csv")),
    fileInput("file8", "Choose CO2e Breakdown CSV File", accept = c("text/csv","text/comma-separated-values,text/plain", ".csv")),
    fileInput("file3", "Choose Energy Breakdown CSV File", accept = c("text/csv","text/comma-separated-values,text/plain", ".csv")),
    uiOutput("category"),
    uiOutput("first"),
    fluidRow(
        column(4, actionButton(inputId = "prevBin", label = "Previous")),
        column(4, actionButton(inputId = "nextBin", label = "Next"))
    )
  ),
  mainPanel(
    tabsetPanel(
        tabPanel("App Info",
            br(),
            tableOutput('help'),
            h5('To get started, Utility CSV must be uploaded.'),
            h5('For Elec Vs Fuel (single graph), the height and the width of the graph can be adjusted for analytical purposes.'),
            h5("- Building Information"),
            tags$ul(
                tags$li("If the required file is not uploaded, nothing will be shown."),
                tags$li("If the wrong file is uploaded, error message 'An error has occurred. Check your logs or contact the app author for clarification.' will be displayed.") 
            ),
            h5("- Time Series"),
            tags$ul(
                tags$li("If there is only one energy type for a selected BDBID, a blank graph with title 'No Usage Point for <energy_type>' will be displayed.")
            ),
            h5("- Parameter Model"),
            tags$ul(
                tags$li("If the required file is not uploaded, nothing will be shown."),
                tags$li("If certain energy type is not in Utility CSV,  blank graph with title 'No data Point for <energy_type>' will be displayed."),
                tags$li("If there are only points and no lines, it is because the model is failing."),
                tags$li("If a dashed line is displayed instead of a solid line, it is because the model failed. This will happen if the 'all model' csv file, a combination of best and failed models, is uploaded instead of the best model csv file."),
                tags$li("If the wrong file is uploaded, error message 'An error has occurred. Check your logs or contact the app author for clarification.' will be displayed.") 
            ),
            h5("- Elec Vs Fuel"),
            tags$ul(
                tags$li("If the required file is not uploaded, a blank graph will be shown."),
                tags$li("If certain energy type is not in Utility CSV, blank graph with title 'Only one energy type for <BDBID>' will be displayed."),
                tags$li("If the wrong file is uploaded, error message 'An error has occurred. Check your logs or contact the app author for clarification.' will be displayed.") 
            ),
            h5('- Multi Elec and Multi Fuel'),
            tags$ul(
                tags$li("If a facility lacks a certain energy type, then a blank graph will be shown."),
                tags$li("If an certain energy type does not make it to best model(i.e the model fails), then only usage points will be shown."),
                tags$li("If a dashed line is displayed instead of a solid line, it is because the model failed. This will happen if the 'all model' csv file, a combination of best and failed models, is uploaded instead of the best model csv file."),
                tags$li("It may take time to load the graphs, if there are a lot of facilities since it is drawing all the graphs at once.")
            ),
            h5("- Other tables and visualizations"),
            tags$ul(
                tags$li("If the required file is not uploaded, nothing will be shown."),
                tags$li("If the wrong file is uploaded, error message 'An error has occurred. Check your logs or contact the app author for clarification.' will be displayed.")
            ),
            h5('- Graphical Display Errors'),
            tags$ul(
                tags$li("If graphs in Multi Elec and Multi Fuel tabs are not displaying properly, you will have to refresh the page and restart."),
                tags$li("If graphs in other tabs are not displaying properly, first try skipping to the next facility or the previous one, and if that does not work, you will have to refresh the page and restart.")
            ),
            br(),
            br()
        ),
        tabPanel("Graphs & Tables for Elec",
            h3("Building Information"),
            br(),
            fluidRow(
                column(6, tableOutput('elec_binfo_df1')),
                column(width = 6, tableOutput('elec_binfo_df2'))
            ),
            h3("Time Series", align = "center"),
            plotlyOutput('elec_timeseries'),
            h3("Parameter Model Graph", align = "center"),
            plotlyOutput('elec_param_plot'),
            span(textOutput('elec_sqft')),
            br(),
            tableOutput('elec_params_df'),
            br(),
            tableOutput('elec_stat_df'),
            br(),
            tableOutput('elec_post_df'),
            br(),
            h3("Lean Analysis"),
            plotlyOutput('elec_lean_plot', width = '700px', height = '250px'),
            br(),
            br(),
            br(),
            tableOutput('elec_numeric_lean_df'),
            br(),
            h3('CO2e Summary'),
            plotlyOutput('elec_co2eui_lean_plot', width = '700px', height = '250px'),
            #tableOutput('co2_df'),
            br(),
            br(),
            tableOutput('elec_co2e_break_df'),
            #fluidRow(
            #    column(4, br(), tableOutput('breakdown_df')),
            #    column(6, tableOutput('elec_break_table'))
            #),
            br(),
            h3('Energy Breakdown Plot'),
            tableOutput('elec_break_table'),
            fluidRow(
                column(6, plotlyOutput('elec_site_break')),
                column(6,  plotlyOutput('elec_source_break'))
            )
        ),
        tabPanel("Graphs & Tables for Fuel",
            h3("Building Information"),
            br(),
            fluidRow(
                column(6, tableOutput('fuel_binfo_df1')),
                column(width = 6, tableOutput('fuel_binfo_df2'))
            ),
            h3("Time Series", align = "center"),
            plotlyOutput('fuel_timeseries'),
            h3("Parameter Model Graph", align = "center"),
            plotlyOutput('fuel_param_plot'),
            span(textOutput('fuel_sqft')),
            br(),
            tableOutput('fuel_params_df'),
            br(),
            tableOutput('fuel_stat_df'),
            br(),
            tableOutput('fuel_post_df'),
            br(),
            h3("Lean Analysis"),
            plotlyOutput('fuel_lean_plot', width = '700px', height = '250px'),
            br(),
            br(),
            br(),
            tableOutput('fuel_numeric_lean_df'),
            br(),
            h3('CO2e Summary'),
            plotlyOutput('fuel_co2eui_lean_plot', width = '700px', height = '250px'),
            #tableOutput('co2_df2'),
            br(),
            br(),
            tableOutput('fuel_co2e_break_df'),
            #fluidRow(
            #    column(4, br(), tableOutput('breakdown_df2')),
            #    column(6, tableOutput('fuel_break_table'))
            #),
            br(),
            h3('Energy Breakdown Plot'),
            tableOutput('fuel_break_table'),
            fluidRow(
                column(6, plotlyOutput('fuel_site_break')),
                column(6,  plotlyOutput('fuel_source_break'))
            )
        ),
        tabPanel('Elec Vs Fuel',
            br(),
            fluidRow(
                column(6, sliderInput(inputId = "height_ef", 'Height of graph', min = 400 , max = 1000, value = 455, step = 5)),
                column(6,sliderInput(inputId = "width_ef", 'Width of graph', min = 400 , max = 1000, value = 850, step = 5))
            ),
            hr(),
            h3("Elec Vs. Fuel", align = "center"),
            plotlyOutput('elec_fuel_plot')
        ),
        tabPanel('Multi Elec',
            #sliderInput(height_e, 'Height of graph', min = 400 , max = 600, value = 450, step = 5),
            br(),
            uiOutput('elec_multi_plot')),
        tabPanel('Multi Fuel',
            #sliderInput(height_f, 'Height of graph', min = 400 , max = 600, value = 450, step = 5),
            br(),
            uiOutput('fuel_multi_plot'))
    )
  )
)