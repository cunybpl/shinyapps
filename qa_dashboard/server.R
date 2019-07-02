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
            uiOutput('retro_end_wiggy')
          )),
          column(width = 8,
          mainPanel(width = 12,
            tabsetPanel(
              tabPanel("App Info",
                h5('To get started, Utility CSV must be uploaded.')
              ),
        tabPanel("Graphs & Tables for Elec",
            h3("Building Information"),
            br(),
            fluidRow(
                column(6, tableOutput('elec_binfo_df1')),
                column(width = 6, tableOutput('elec_binfo_df2'))
            )
        ),
        tabPanel("Graphs & Tables for Fuel",
            h3("Building Information")
        ),
        tabPanel('Elec Vs Fuel',
            br(),
            h3("Elec Vs. Fuel", align = "center")
        )
            )#main tab panel
          )))#main panle
        )
      )#fluid page
    }
  })

#### YOUR APP'S SERVER CODE GOES HERE ----------------------------------------
  # slider input widget

  # password entry UI componenets:
  #   username and password text fields, login button

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

  ##########################################
  ############# Prepare Queries ############
  ##########################################

  baseline_query <- reactive({list(bdbid = input$bdbid, period = input$period, no_sqft=input$sqft_fl,
                          fiscal_year=input$fiscal_year, target_date = input$tar_date,
                          relax = TRUE, invert_order_elec = TRUE, invert_order_fuel = TRUE)})

  retro_query <- reactive({list(bdbid = input$bdbid, period = input$period, no_sqft=input$sqft_fl,
                          fiscal_year=input$fiscal_year, retrofit_start_date = input$start_date, retrofit_end_date = input$end_date)})

  ##########################################
  ####### GATHER AROUND, DATA IS HERE ######
  ##########################################

  task_id_base <- reactive({
    get_task_id('/bema/modeler/baseline/basic/', query_params = baseline_query())
    })

  url_get <- reactive({
    base_url <- cache_get_base_url()
    token <- cache_get_token()
    url <- paste0(base_url, 'bema/results/', task_id_base())
    return(url)
  })

  baseline_batch <- reactive({
    print(task_id_base())
    token <- cache_get_token()
    res <- httr::GET(url_get(), httr::add_headers(Authorization=token),encode='json', timeout=20, config = httr::config(ssl_verifypeer = FALSE))
    print(res)
    res = .parse_contents(res)
    print(res$status)
    res$results
  })

  retro_batch <- reactive({
    if(is.null(input$start_date) | is.null(input$end_date)){
      return(list())
    }
    return(list())
    })

  ##########################################
  ######## VISULIZATION STARTS HERE ########
  ##########################################


  binfo_df <- reactive({
    print('your mom')
    df = baseline_batch()$building
    print(df)
    return(df)
  })

  utility_base <- reactive({
    baseline_batch()$utility
  })


  #############################################
  ################# Calculator ################
  #############################################

  binfo_output_list <- reactive({
    if (is.null(binfo_df()))
    {
      list(binfo_df1 = data.frame(), binfo_df1 = data.frame())
    }else
    {
      binfo_table(binfo_df(), bdbid_n())
    }
  })

  ################# Energy Dependent ###############

  ########## Building comparison, lean ##############


  ################# Energy Independent ###############



  #############################################
  ################# ELEC OUTPUT ###############
  #############################################

  output$elec_binfo_df1 <- renderTable({
    binfo_output_list()$binfo_df1
  }, align = 'l', colnames = FALSE, width = "auto")

  output$elec_binfo_df2 <- renderTable({
    binfo_output_list()$binfo_df2
  }, align = 'l', colnames = FALSE, rownames = TRUE, width = "auto")


  #############################################
  ################# FUEL OUTPUT ###############
  #############################################



  ############### Elec Vs Fuel ################


  ##########################################
  ############### Buildings ################
  ##########################################



  ##########################################
  ############### Models ###################
  ##########################################


})
