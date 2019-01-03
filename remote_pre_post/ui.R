library(shiny)
library(plotly)

ui <- fluidPage(
  headerPanel('Pre/Post Retrofit'),
  sidebarPanel(
    uiOutput("sessions_ui"),
    uiOutput("cat_ui"),
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
            plotlyOutput('plot_time_elec'),
            br(),
            h3("Parameter Model Graph for Pre and Post", align = "center"),
            plotlyOutput('plot_model_elec'),
            tableOutput('params_df_elec'),
            tableOutput('stat_df_elec'),
            br(),
            tableOutput('post_elec'),
            br(),
            h3("Adjusted Baseline", align = "center"),
            plotlyOutput('plot_adjust_elec'),
            br(),
            h3("Adjusted Saving"),
            tableOutput('adjust_saving_elec'),
            br(),
            h3("Normalized Baseline", align = "center"),
            plotlyOutput('plot_norm_elec'),
            h3("Normalized Saving"),
            tableOutput('norm_saving_elec'),
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
            plotlyOutput('plot_time_fuel'),
            br(),
            h3("Parameter Model Graph for Pre and Post", align = "center"),
            plotlyOutput('plot_model_fuel'),
            tableOutput('params_df_fuel'),
            tableOutput('stat_df_fuel'),
            br(),
            tableOutput('post_fuel'),
            br(),
            h3("Adjusted Baseline", align = "center"),
            plotlyOutput('plot_adjust_fuel'),
            br(),
            h3("Adjusted Saving"),
            tableOutput('adjust_saving_fuel'),
            br(),
            h3("Normalized Baseline", align = "center"),
            plotlyOutput('plot_norm_fuel'),
            h3("Normalized Saving"),
            tableOutput('norm_saving_fuel'),
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