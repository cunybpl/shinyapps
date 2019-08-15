library(shinydashboard)
library(markdown)

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
                      column(3, uiOutput('main_test_base_wiggy')),
                      column(3, uiOutput('all_models_base_wiggy')),
                      column(3, uiOutput('relax_base_wiggy')),
                      column(3, uiOutput('nac_base_wiggy'))
                    )
                  ), #baseline model box
                  box(title = 'Building Information', collapsible = TRUE,
                    fluidRow(
                        column(6, tableOutput('base_binfo_df1')),
                        column(width = 6, tableOutput('base_binfo_df2'))
                    )
                  )#building box
                ),#boxes
                h3("Time Series", align = "center"),
                plotlyOutput('base_timeseries'),
                fluidRow(
                  tabBox(title = 'Parameter Model Graph', width = 24,
                    tabPanel('Baseline',
                      plotlyOutput('base_param_plot')
                    ), #base plot
                    tabPanel('Portfolio',
                      plotlyOutput('port_base_param_plot')
                    )
                  )#end of plot box
                ),#parameter plots for base and port_base
                br(),
                fluidRow(
                  column(6, uiOutput('model_base_wiggy')),
                  column(6, uiOutput('model_port_wiggy'))
                ),
                br(),
                h4('Baseline Model Results'),
                tableOutput('base_param_df'),
                br(),
                tableOutput('base_stat_df'),
                h4('Portfolio Model Results'),
                tableOutput('port_base_param_df'),
                br(),
                tableOutput('port_base_stat_df'),
                br(),
                fluidRow(
                  column(6,
                    h4('Baseline Post Model'),
                    tableOutput('base_post_df')
                  ),
                  column(6,
                    h4('Portfolio Post Model'),
                    tableOutput('port_base_post_df')
                  )
                )# end of post
              ), #baseline
              tabItem(tabName = 'retrofit',
                h2('Retrofit Dashboard'),
                fluidRow(
                  box(title = "Bema Model Controls", collapsible = TRUE, solidHeader = TRUE, background = 'blue',
                    fluidRow(
                      column(6,
                        uiOutput('elec_model_ord_retro_wiggy'),
                        uiOutput('elec_r2_retro_wiggy'),
                        uiOutput('elec_cvrmse_retro_wiggy')
                      ),#elec
                      column(6,
                        uiOutput('fuel_model_ord_retro_wiggy'),
                        uiOutput('fuel_r2_retro_wiggy'),
                        uiOutput('fuel_cvrmse_retro_wiggy')
                      )#fuel
                    ),
                    fluidRow(
                      column(3, uiOutput('main_test_retro_wiggy')),
                      column(3, uiOutput('all_models_retro_wiggy')),
                      column(3, uiOutput('relax_retro_wiggy')),
                      column(3, uiOutput('nac_retro_wiggy'))
                    )
                  ), #baseline model box
                  box(title = 'Building Information', collapsible = TRUE,
                    fluidRow(
                        column(6, tableOutput('retro_binfo_df1')),
                        column(width = 6, tableOutput('retro_binfo_df2'))
                    )
                  )#building box
                ), #boxes
                h3("Time Series", align = "center"),
                plotlyOutput('retro_timeseries'),
                h3("Parameter Model Graph", align = "center"),
                plotlyOutput('retro_param_plot'),
                br(),
                fluidRow(
                  column(6, uiOutput('model_retro_wiggy_pre')),
                  column(6, uiOutput('model_retro_wiggy_post'))
                ),
                br(),
                tableOutput('retro_param_df'),
                br(),
                tableOutput('retro_stat_df'),
                br(),
                tableOutput('retro_post_df')
              )# retrofit
            )
      ) #dashboard body
)
