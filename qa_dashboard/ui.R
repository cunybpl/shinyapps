library(shinydashboard)

ui <- dashboardPage(
          dashboardHeader(title = 'QA Dashboard'),
          dashboardSidebar(
            menuItem("Log In Page", tabName = "login_page", icon = icon("sign-in")),
            menuItem("Dashboard",
              menuSubItem("Baseline", tabName = "baseline", icon = icon("dashboard")),
              menuSubItem("Retrofit", tabName = "retrofit", icon = icon("dashboard"))
            ),
            uiOutput('bdbid_wiggy'),
            uiOutput('period_wiggy'),
            fluidRow(
                column(4, uiOutput('fiscal_wiggy')),
                column(6, uiOutput('sqft_wiggy'))
            ),
            uiOutput('tar_date_wiggy'),
            uiOutput('retro_start_wiggy'),
            uiOutput('retro_end_wiggy'),
            uiOutput('energy_wiggy'),
            uiOutput('model_ordering_wiggy')
          ), #dashboard sidebar
          dashboardBody(
            tabItems(
              tabItem(tabName = 'login_page',
                uiOutput('log_in_ui')
              ),
              tabItem(tabName = 'baseline',
                h2('Baseline Dashboard'),
                fluidRow(
                  box(title = "Bema Model Controls", collapsible = TRUE, solidHeader = TRUE, background = 'blue',
                    fluidRow(
                      column(6,
                        uiOutput('elec_model_ord_base_wiggy'),
                        uiOutput('elec_r2_base_wiggy'),
                        uiOutput('elec_cvrmse_base_wiggy')
                      ),#elec
                      column(6,
                        uiOutput('fuel_model_ord_base_wiggy'),
                        uiOutput('fuel_r2_base_wiggy'),
                        uiOutput('fuel_cvrmse_base_wiggy')
                      )#fuel
                    ),
                    fluidRow(
                      column(4, uiOutput('main_test_base_wiggy')),
                      column(4, uiOutput('all_models_base_wiggy')),
                      column(4, uiOutput('relax_base_wiggy'))
                    )
                  ), #baseline model box
                  box(title = 'Building Information',
                    fluidRow(
                        column(6, tableOutput('base_binfo_df1')),
                        column(width = 6, tableOutput('base_binfo_df2'))
                    )
                  )#building box
                ),#boxes 
                h3("Time Series", align = "center"),
                plotlyOutput('base_timeseries'),
                h3("Parameter Model Graph", align = "center"),
                plotlyOutput('base_param_plot'),
                br(),
                tableOutput('base_param_df'),
                br(),
                tableOutput('base_stat_df'),
                br(),
                tableOutput('base_post_df')
              ), #baseline
              tabItem(tabName = 'retrofit',
                h3('Retrofit Dashboard')
              )# retrofit
            )
      ) #dashboard body
)
