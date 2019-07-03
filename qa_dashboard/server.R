library(shiny)
library(bplclientR)
library(plotly)
library(DT)
shinyServer(function(input, output, session) {
#### UI code --------------------------------------------------------------
  output$ui <- renderUI({
    if (user_input$authenticated == FALSE) {
      ##### UI code for login page
      fluidPage(
        fluidRow(
          column(width = 2, offset = 5,
            br(), br(), br(), br(),
            uiOutput("uiLogin"),
            uiOutput("pass")
          )
        )
      )
    } else {
      #### Your app's UI code goes here!
      fluidPage(
          titlePanel('QA Dashboard'),
          fluidRow(width = 12,
          sidebarLayout(
          column(width = 4,
          sidebarPanel(style = "position:fixed;width:inherit;",width = 4,
            uiOutput('bdbid_wiggy'),
            uiOutput('period_wiggy'),
            fluidRow(
                column(4, uiOutput('fiscal_wiggy')),
                column(6, uiOutput('sqft_wiggy'))
            ),
            uiOutput('tar_date_wiggy'),
            uiOutput('retro_start_wiggy'),
            uiOutput('retro_end_wiggy'),
            uiOutput('energy_wiggy')
          )),
          column(width = 8,
          mainPanel(width = 12,
            tabsetPanel(
              tabPanel("App Info",
                h5('To get started, Utility CSV must be uploaded.')
              ),
        tabPanel("Graphs & Tables for Baseline",
            h3("Building Information"),
            br(),
            fluidRow(
                column(6, tableOutput('base_binfo_df1')),
                column(width = 6, tableOutput('base_binfo_df2'))
            ),
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
        ),
        tabPanel("Graphs & Tables for Retrofit",
            h3("Building Information"),
            br(),
            fluidRow(
                column(6, tableOutput('retro_binfo_df1')),
                column(width = 6, tableOutput('retro_binfo_df2'))
            ),
            h3("Time Series", align = "center"),
            plotlyOutput('retro_timeseries'),
            h3("Parameter Model Graph", align = "center"),
            plotlyOutput('retro_param_plot'),
            br(),
            tableOutput('retro_param_df'),
            br(),
            tableOutput('retro_stat_df'),
            br(),
            tableOutput('retro_post_df')
        )
            )#main tab panel
          )))#main panle
        )
      )#fluid page
    }
  })


  ##########################################
  ########## LOGIN PAGE STARTS HERE ########
  ##########################################

  output$uiLogin <- renderUI({
    wellPanel(
      textInput("user_name", "User Name:"),

      passwordInput("password", "Password:"),

      actionButton("login_button", "Log in")
    )
  })


  user_input <- reactiveValues(authenticated = FALSE, status = "")
  observeEvent(input$login_button, {
      cache_init(base_url = 'staging', api_url = 'https://api.testing.cunybplservices.fun/', ssl_verify = FALSE)
      result = tryCatch({
                    fetch_auth_token(input$user_name, input$password)},
                    error = function(e){NULL}
              )
      if(is.null(result))
      {
        user_input$authenticated <- FALSE
        user_input$status <- "bad_password"
      }else
      {
        user_input$authenticated <- TRUE
      }
    })

    # red error message if bad credentials
  output$pass <- renderUI({
    if (user_input$status == "bad_password") {
      h5(strong("Incorrect password!", style = "color:red"), align = "center")
    }else{
      ""
    }
  })


  ##########################################
  ########## LOGIN PAGE ENDS HERE ##########
  ##########################################
  ############## GET QUERIES ###############
  ##########################################

  output$bdbid_wiggy <- renderUI({
    tagList(
      textInput('bdbid', 'Facillity/BDBID', value ='', width = NULL, placeholder = '666')
    )
    })

  output$period_wiggy <- renderUI({
    numericInput('period', 'Period', value = 12, min = NA, max = NA, step = 12, width = NULL)
    })

  output$fiscal_wiggy <- renderUI({
    tagList(
      checkboxInput('fiscal_year', 'Fiscal Year', value = FALSE, width = NULL)
    )
    })

  output$sqft_wiggy <- renderUI({
    checkboxInput('sqft_fl', 'Normalized by sqft', value = FALSE, width = NULL)
    })

  output$tar_date_wiggy <- renderUI({
    tagList(
      textInput('tar_date', 'Traget Date', value =NULL, width = NULL, placeholder = 'yyyy-mm-dd')
    )
    })

  output$retro_start_wiggy <- renderUI({
    tagList(
      textInput('start_date', 'Retrofit Start Date', value =NULL, width = NULL, placeholder = 'yyyy-mm-dd')
    )
    })

  output$retro_end_wiggy <- renderUI({
    tagList(
      textInput('end_date', 'Retrofit End Date', value =NULL, width = NULL, placeholder = 'yyyy-mm-dd')
    )
    })

  output$energy_wiggy <- renderUI({
    tagList(
      selectInput('energy_type', 'Energy Type', choices = c('Elec', 'Fuel'), selected = 'Elec', multiple = FALSE,
  selectize = TRUE, width = NULL, size = NULL)

    )
  })

  ##########################################
  ############# Prepare Queries ############
  ##########################################

  baseline_query <- reactive({list(bdbid = input$bdbid, period = input$period, no_sqft=!input$sqft_fl,
                          fiscal_year=input$fiscal_year, target_date = input$tar_date,
                          relax = TRUE, invert_order_elec = TRUE, invert_order_fuel = TRUE)})

  retro_query <- reactive({list(bdbid = input$bdbid, period = input$period, no_sqft=!input$sqft_fl,
                          fiscal_year=input$fiscal_year, retrofit_start_date = input$start_date, retrofit_end_date = input$end_date)})

  ##########################################
  ####### GATHER AROUND, DATA IS HERE ######
  ##########################################

  baseline_batch <- reactive({
    if(input$tar_date == ""){
      return(list())
    }
    df = get_batch_result('/bema/modeler/baseline/basic/', baseline_query())
    return(df$results)
  })

  retro_batch <- reactive({
    if(input$start_date == "" | input$end_date == ""){
      return(list())
    }
    df = get_batch_result('/bema/modeler/retrofit/basic/', retro_query())
    return(df$results)
  })

  ##########################################
  ######## VISULIZATION STARTS HERE ########
  ##########################################


  binfo_df <- reactive({
    if(length(baseline_batch())){
      return(baseline_batch()$building)
    }else if(length(retro_batch())){
      return(retro_batch()$building)
    }else{
      return(NULL)
    }
  })

  ################# Unretrofit ################

  utility_base <- reactive({
    baseline_batch()$utility
  })

  best_base <- reactive({
    df = baseline_batch()$best
    if(length(df)){
      best_cols = c("fiscal_year", "period", "tmin", "tmax", "n", "session_id", "nac")
      df = missing_cols_handler(best_cols, df)
    }
    return(df)
  })

  post_base <- reactive({
    df = baseline_batch()$post
    if(length(df))
    {
      post_cols = c("sitename", 'period', 'percent_cooling', 'percent_heating', 'percent_baseload', "session_id")
      df = missing_cols_handler(post_cols, df)
    }
    return(df)
  })

  ################## Retrofit #################

  utility_retro <- reactive({
    retro_batch()$utility
  })

  best_retro <- reactive({
    retro_batch()$best
  })

  post_retro <- reactive({
    df = retro_batch()$post
    if(nrow(df)){
      df = percent_heat_cool_func(df)
    }
    df
  })

  ################## energy #################

  util_energy <- reactive({
    if (flag_func(utility_retro(), input$bdbid, input$energy_type)){
      return(subset(utility_retro(), bdbid == input$bdbid & energy_type == input$energy_type))
    }
    NULL
  })

  energy_null_flag <- reactive({is.null(util_energy())})

  #############################################
  ################# Calculator ################
  #############################################

  b_name <- reactive({get_building_name(binfo_df(), input$bdbid)})

  binfo_output_list <- reactive({
    if (is.null(binfo_df()))
    {
      list(binfo_df1 = data.frame(), binfo_df1 = data.frame())
    }else
    {
      binfo_table(binfo_df(), input$bdbid)
    }
  })

  area_info <- reactive({
    flag_area = check_sqft_na(binfo_output_list()$binfo_df2)
    if (flag_area)
    {
        return(list(flag_area = flag_area, area = binfo_output_list()$binfo_df2['Gross Square Feet',]))
    }else
    {
        return(list(flag_area = flag_area))
    }
  })

  #############################################
  ############## BASELINE OUTPUT ##############
  #############################################

  output$base_binfo_df1 <- renderTable({
    binfo_output_list()$binfo_df1
  }, align = 'l', colnames = FALSE, width = "auto")

  output$base_binfo_df2 <- renderTable({
    binfo_output_list()$binfo_df2
  }, align = 'l', colnames = FALSE, rownames = TRUE, width = "auto")

  output$base_timeseries <- renderPlotly({

    if (flag_func(utility_base(), input$bdbid, input$energy_type))
    {
      util = subset(utility_base(), bdbid == input$bdbid & energy_type == input$energy_type)
      plot_timeseries(util, input$energy_type)
    }else
    {
      plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = paste('No usage points for', input$energy_type))
    }
  })

  output$base_param_plot <- renderPlotly({
    utility = subset(utility_base(), utility_base()$bdbid == input$bdbid & utility_base()$energy_type == input$energy_type)
    if(length(utility)){
      best_model = subset(best_base(), best_base()$bdbid == input$bdbid & best_base()$energy_type == input$energy_type)
      main_param_plot(utility, best_model, b_name())
    }else
    {
      plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = paste('No usage points for', input$energy_type))
    }
  })

  output$base_param_df <- renderTable({
      params_table(best_base(), input$bdbid, input$energy_type, 1)
  }, align = 'l', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$base_stat_df <- renderTable({
      stat_table(best_base(), input$bdbid, input$energy_type)
  }, align = 'l', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$base_post_df <- renderTable({
    if (!flag_func(post_base(), input$bdbid, input$energy_type))
    {
      return(NULL)
    }
    post_output_df_server(post_base(), input$bdbid, input$energy_type, area_info(), input$period)
  }, align = 'l', rownames = TRUE, colnames = TRUE, width = "auto", digits = 7)

  #############################################
  ################ RETRO OUTPUT ###############
  #############################################

  output$retro_binfo_df1 <- renderTable({
    binfo_output_list()$binfo_df1
  }, align = 'l', colnames = FALSE, width = "auto")

  output$retro_binfo_df2 <- renderTable({
    binfo_output_list()$binfo_df2
  }, align = 'l', colnames = FALSE, rownames = TRUE, width = "auto")

  output$retro_timeseries <- renderPlotly({
    if (energy_null_flag())
    {
      plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = 'No data points for Elec')
    }else
    {
      plot_timeseries(util_energy(), 'Elec')
    }
  })

  output$retro_param_plot <- renderPlotly({
    if (energy_null_flag())
    {
      return(plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = 'No data points for Elec'))
    }else{
      best_model = subset(best_retro(), best_retro()$bdbid == input$bdbid & best_retro()$energy_type == input$energy_type)
      main_param_plot(util_energy(), best_model, b_name())
    }
    })

  output$retro_param_df <- renderTable({
    pre_df = params_table(best_retro(), input$bdbid, input$energy_type, 1)
    post_df = params_table(best_retro(), input$bdbid, input$energy_type, 3)
    return(rbind(pre_df, post_df))
  }, align = 'l', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$retro_stat_df <- renderTable({
      stat_table(best_retro(), input$bdbid, input$energy_type)
  }, align = 'l', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$retro_post_df <- renderTable({
    post_output_df = post_output_retrofit(post_retro(), input$bdbid, input$energy_type)
    post_output_df = post_col_retrofit(post_output_df, input$period, input$energy_type)
    }, align = 'c', rownames = TRUE, colnames = TRUE, width = "auto", digits = 7)

})
