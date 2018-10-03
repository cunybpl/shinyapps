library(shiny)
library(bplclientR)
library(plotly)
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
          titlePanel('TEST'),
          fluidRow(width = 12,
          sidebarLayout(
          column(width = 4,
          sidebarPanel(style = "position:fixed;width:inherit;",width = 4,
            uiOutput('category_wiggy'),
            uiOutput('bdbid_wiggy'),
            numericInput("fiscal_year", "Fiscal Year: ", 2017, min = 2014, max = 2018),
            fluidRow(
                column(4, actionButton(inputId = "prevBin", label = "Previous")),
                column(4, actionButton(inputId = "nextBin", label = "Next"))
            )
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
                column(6, h5('Site Energy Breakdown', align = 'left'), plotlyOutput('elec_site_break')),
                column(6, h5('Source Energy Breakdown', align = 'left'), plotlyOutput('elec_source_break'))
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
                column(6, h5('Site Energy Breakdown', align = 'left'), plotlyOutput('fuel_site_break')),
                column(6, h5('Source Energy Breakdown', align = 'left'), plotlyOutput('fuel_source_break'))
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
        tabPanel('Building Comparison',
          br(),
          plotlyOutput('b_comp_plot'))
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
      cache_init()
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
  ######## VISULIZATION STARTS HERE ########
  ##########################################


  lean_category_df <- reactive({
    df = fetch_request('portfolios/', query_params=list(page = 1))$result
    df$lean_category = trimws(df$lean_category)
    df$char_n = nchar(trimws(df$lean_category))
    df$primary_fun = 1
    df$primary_fun[df$char_n <= 5] = 0
    df
    })

  output$category_wiggy <- renderUI({
    tagList(
      selectInput('category', 'Choose Category', sort(lean_category_df()$lean_category), selected = NULL, multiple = FALSE,selectize = TRUE, width = NULL, size = NULL)
    )
    })


  binfo_df <- reactive({

    load("building_info.RData", df <- new.env()) 
    bin_df = df$binfo_df
    #bin_df = paginator_fetch_request('dcasdb/buildings/', query_params=list(page_size = 1000))$result

    fun_flag = lean_category_df()$primary_fun[lean_category_df()$lean_category == input$category]
    if (fun_flag)
    {
      #result_list = paginator_fetch_request('dcasdb/buildings/', query_params=list(epapm_primary_function = input$category))
      df = subset(bin_df, bin_df$epapm_primary_function == input$category)
    }else
    {
      #result_list = paginator_fetch_request('dcasdb/buildings/', query_params=list(oper_agency_acronym = input$category))
      df = subset(bin_df, bin_df$oper_agency_acronym == input$category)
    }
    df
  })

  b_df <- reactive({
      comb = paste(binfo_df()$bdbid, binfo_df()$building_name, sep = ' - ')
      b_df = data.frame(bdbid = binfo_df()$bdbid, name = comb)
  })

  output$bdbid_wiggy <- renderUI({
    tagList(
      selectInput('bdbid', 'Choose Facillity', b_df()$name, selected = NULL, multiple = FALSE,selectize = TRUE, width = NULL, size = NULL)
    )
    })

  bdbid_n <- reactive({
    b_df()$bdbid[b_df()$name == input$bdbid]
  })

  b_name  <- reactive({get_building_name(binfo_df(), bdbid_n())})

  bdbid_vec <- reactive({paste0(b_df()$bdbid, collapse = ',')})

  observeEvent(input$prevBin, {
        current <- which(b_df()$bdbid == bdbid_n())
        if(current > 1){
            updateSelectInput(session, "bdbid",
                              choices = b_df()$name,
                              selected = b_df()$name[current - 1])
        }
        #click_n() = click_n() + 1
  })
    
  observeEvent(input$nextBin, {
        current <- which(b_df()$bdbid  == bdbid_n())
        if(current < length(b_df()$name)){
            updateSelectInput(session, "bdbid",
                              choices = b_df()$name,
                              selected = b_df()$name[current + 1])
        }
        #click_n() = click_n() + 1
  })

  #############################################
  ################# INPUT FILE ################
  #############################################

  temp_df <- reactive({
    fiscal_n = paste(input$fiscal_year, input$fiscal_year - 1, sep=',')
    df = fetch_request('portfolios/utility/', query_params=list(bdbid = bdbid_n(), fiscal_year = fiscal_n))$result
    if (length(df))
    {
      colnames(df)[colnames(df) == 'oat' | colnames(df) == 'OAT'] = 'OAT'
      return(df)
    }else
    {
      return(NULL)
    }
  })

  best_model <- reactive({
    df = paginator_fetch_request('portfolios/changepoint-model/', query_params=list(bdbid = bdbid_vec(), fiscal_year = input$fiscal_year, best = 1, page_size = 1000))$result
    
    if (length(df))
    {
      if(!('prepost' %in% colnames(df))){df$prepost = 1}
      best_cols = c("fiscal_year", "period", "tmin", "tmax", "n", "session_id", "nac")
      df = missing_cols_handler(best_cols, df)
      return(df)
    }else
    {
      return(NULL)
    }
  })

  binfo_output_list <- reactive({
    if (is.null(binfo_df()))
    {
      list(binfo_df1 = data.frame(), binfo_df1 = data.frame())
    }else
    { 
      binfo_table(binfo_df(), bdbid_n())
    }
  })

  post_df <- reactive({
    if(is.null(best_model()))
    {
      return(NULL)
    }
    df = paginator_fetch_request('portfolios/bestmodel-loads-sensitivity/', query_params=list(bdbid = bdbid_vec(), fiscal_year = input$fiscal_year, page_size = 1000))$result
    post_cols = c("sitename", 'period', 'percent_cooling', 'percent_heating', 'percent_baseload', "session_id")
    df = missing_cols_handler(post_cols, df)
    return(df)
    })

  lean_df <- reactive({
    df = paginator_fetch_request('portfolios/bestmodel-loads-sensitivity-lean-rank/', query_params=list(fiscal_year = input$fiscal_year, lean_category = input$category, page_size = 1000))$result
    if(length(df) == 0)
    {
      return(NULL)
    }
    return(df)
    })

  co2eui_df <- reactive({
    df = paginator_fetch_request('portfolios/co2eui-lean-rank/', query_params=list(fiscal_year = input$fiscal_year, lean_category = input$category, page_size = 1000))$result
    if(length(df) == 0)
    {
      return(NULL)
    }
    return(df)
  })

  breakdown_df <- reactive({
    df = paginator_fetch_request('portfolios/co2eui-breakdown/', query_params=list(bdbid = bdbid_vec(), fiscal_year = input$fiscal_year, page_size = 1000))$result
    if(length(df) == 0)
    {
      return(NULL)
    }
    return(df)
  })

  energy_df <- reactive({
    df = paginator_fetch_request('portfolios/energy-breakdown/', query_params=list(bdbid = bdbid_vec(), fiscal_year = input$fiscal_year, page_size = 1000))$result
    if(length(df) == 0)
    {
      return(NULL)
    }
    return(df)
    })

  #############################################
  ################# Calculator ################
  #############################################

  ################# Energy Dependent ###############
  per_num_list_elec <- reactive({
    per_num_func(lean_df(), bdbid_n(), 'Elec')
    })

  per_num_list_fuel <- reactive({
    per_num_func(lean_df(), bdbid_n(), 'Fuel')
    })

  ################# Energy Independent ###############

  energy_break_plot <- reactive({
    if (bdbid_n() %in% unique(energy_df()$bdbid))
    {
      df = subset(energy_df(), energy_df()$bdbid == bdbid_n())
      p = energy_break_pie_chart(df)
      p$break_table = make_break_table(df)
      return(p)
    }else
    {
      p = plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = 'No usage points')
      return(list(site_p = p, source_p = p, break_table = data.frame()))
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

  b_name  <- reactive({get_building_name(binfo_df(), bdbid_n())})

  co2_lean_plot <- reactive({
    if(bdbid_n() %in% unique(co2eui_df()$bdbid))
    { 
      co2_rank = co2_rank_get(co2eui_df(), bdbid_n())
      return(percent_figure(co2_rank, co2_flag = TRUE))
    }else
    {
      return(plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = 'No data points'))
    }
  })

  co2_break_df <- reactive({
    co2_value_get(breakdown_df(), bdbid_n())
  })

  #############################################
  ################# ELEC OUTPUT ###############
  #############################################

  output$elec_binfo_df1 <- renderTable({
    binfo_output_list()$binfo_df1
  }, align = 'l', colnames = FALSE, width = "auto")

  output$elec_binfo_df2 <- renderTable({
    binfo_output_list()$binfo_df2
  }, align = 'l', colnames = FALSE, rownames = TRUE, width = "auto")


  #timeseries
  output$elec_timeseries <- renderPlotly({

    if (flag_func(temp_df(), bdbid_n(), 'Elec'))
    { 
      util = subset(temp_df(), bdbid == bdbid_n() & energy_type == 'Elec')
      plot_timeseries_2(util, 'Elec')
    }else
    {
      plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = 'No usage points for Elec')
    }
  })

  #prameter plot
  output$elec_param_plot <- renderPlotly({main_plot(temp_df(), best_model(), bdbid_n(), 'Elec', b_name())})


  output$elec_sqft <- renderText({
    if (flag_func(temp_df(), bdbid_n(), 'Elec'))
      { 
        sqft_col = subset(temp_df()$using_sqft, temp_df()$bdbid == bdbid_n() & temp_df()$energy_type == 'Elec')
        sqft_message(sqft_col)
      }else
      {
        "No usage points for Elec."
      }
    })

  output$elec_params_df <- renderTable({
      params_table(best_model(), bdbid_n(), 'Elec')
  }, align = 'l', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$elec_stat_df <- renderTable({
      stat_table(best_model(), bdbid_n(), 'Elec')
  }, align = 'l', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$elec_post_df <- renderTable({
    if (!flag_func(post_df(), bdbid_n(), 'Elec'))
    {
      return(NULL)
    }
    n = ifelse(flag_func(best_model(), bdbid_n(), 'Elec'), subset(best_model()$n, best_model()$bdbid == bdbid_n() & best_model()$energy_type == 'Elec'), 0)
    post_output_df_server(post_df(), bdbid_n(), 'Elec', area_info(), n)
  }, align = 'l', rownames = TRUE, colnames = TRUE, width = "auto", digits = 7)

  #lean rank plot
  output$elec_lean_plot <- renderPlotly({per_num_list_elec()$percent_fig})

  #lean rank table
  output$elec_numeric_lean_df <- renderTable({
      per_num_list_elec()$numeric_df
  }, align = 'l', rownames = TRUE, colnames = TRUE, width = "auto", digits = NULL)

  #co2_eui_lean plot
  output$elec_co2eui_lean_plot <- renderPlotly({
    co2_lean_plot()
  })

  #co2e breakdown table
  output$elec_co2e_break_df <- renderTable({
    co2_break_df()
    }, align = 'l', rownames = TRUE, colnames = TRUE, width = "auto", digits = NULL)

  output$elec_site_break <- renderPlotly({
    energy_break_plot()$site_p
    })

  output$elec_source_break <- renderPlotly({
    energy_break_plot()$source_p
    })

  output$elec_break_table <- renderTable({
    energy_break_plot()$break_table
    }, align = 'l', rownames = FALSE, colnames = TRUE, width = "auto", digits = NULL)

  #############################################
  ################# FUEL OUTPUT ###############
  #############################################

  output$fuel_binfo_df1 <- renderTable({
    binfo_output_list()$binfo_df1
  }, align = 'l', colnames = FALSE, width = "auto")

  output$fuel_binfo_df2 <- renderTable({
    binfo_output_list()$binfo_df2
  }, align = 'l', colnames = FALSE, rownames = TRUE, width = "auto")

  #timeseries
  output$fuel_timeseries <- renderPlotly({

    if (flag_func(temp_df(), bdbid_n(), 'Fuel'))
    { 
      util = subset(temp_df(), bdbid == bdbid_n() & energy_type == 'Fuel')
      plot_timeseries_2(util, 'Fuel')
    }else
    {
      plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = 'No usage points for Fuel')
    }
  })

  #prameter plot
  output$fuel_param_plot <- renderPlotly({main_plot(temp_df(), best_model(), bdbid_n(), 'Fuel', b_name())})


  output$fuel_sqft <- renderText({
    if (flag_func(temp_df(), bdbid_n(), 'Fuel'))
      { 
        sqft_col = subset(temp_df()$using_sqft, temp_df()$bdbid == bdbid_n() & temp_df()$energy_type == 'Fuel')
        sqft_message(sqft_col)
      }else
      {
        "No usage points for Fuel."
      }
    })

  output$fuel_params_df <- renderTable({
      params_table(best_model(), bdbid_n(), 'Fuel')
  }, align = 'l', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$fuel_stat_df <- renderTable({
      stat_table(best_model(), bdbid_n(), 'Fuel')
  }, align = 'l', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$fuel_post_df <- renderTable({
    #figure()$'Elec'$post_output_df
    if (!flag_func(post_df(), bdbid_n(), 'Fuel'))
    {
      return(NULL)
    }
    n = ifelse(flag_func(best_model(), bdbid_n(), 'Fuel'), subset(best_model()$n, best_model()$bdbid == bdbid_n() & best_model()$energy_type == 'Fuel'), 0)
    post_output_df_server(post_df(), bdbid_n(), 'Fuel', area_info(), n)
  }, align = 'c', rownames = TRUE, colnames = TRUE, width = "auto", digits = 7)

  #lean rank plot
  output$fuel_lean_plot <- renderPlotly({per_num_list_fuel()$percent_fig})

  #lean rank table
  output$fuel_numeric_lean_df <- renderTable({
      per_num_list_elec()$numeric_df
  }, align = 'l', rownames = TRUE, colnames = TRUE, width = "auto", digits = NULL)

  #co2_eui_lean plot
  output$fuel_co2eui_lean_plot <- renderPlotly({
    co2_lean_plot()
  })

  #co2e breakdown table
  output$fuel_co2e_break_df <- renderTable({
    co2_break_df()
    }, align = 'l', rownames = TRUE, colnames = TRUE, width = "auto", digits = NULL)

  output$fuel_site_break <- renderPlotly({
    energy_break_plot()$site_p
    })
  output$fuel_source_break <- renderPlotly({
    energy_break_plot()$source_p
    })

  output$fuel_break_table <- renderTable({
    energy_break_plot()$break_table
    }, align = 'l', rownames = FALSE, colnames = TRUE, width = "auto", digits = NULL)

  ############### Elec Vs Fuel ################

  output$elec_fuel_plot <- renderPlotly({
    if (length(best_model()$energy_type[best_model()$bdbid == bdbid_n()]) == 2)
    {
      return(elec_fuel_graph_func(temp_df(), best_model(), bdbid_n(), height = input$height_ef, width = input$width_ef))
    }else
    {
      return(plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = paste('Only one energy type for', bdbid_n())))
    }
  })

  output$b_comp_plot <- renderPlotly({
    building_comparison_graph(breakdown_df(), b_df())
    })


})
