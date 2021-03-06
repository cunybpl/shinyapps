library(shiny)
library(bplclientR)
library(plotly)
library(DT)
library(markdown)
shinyServer(function(input, output, session) {
#### UI code --------------------------------------------------------------

  output$log_in_ui <- renderUI({
    if (user_input$authenticated == FALSE){

    fluidPage(
      fluidRow(
        column(width = 2, offset = 5,
          br(), br(), br(), br(),
          uiOutput("uiLogin"),
          uiOutput("pass")
        )
      )
    )
  }else{
    fluidPage(
      textOutput('log_in_page_text'),
      br(),
      h4('Help'),
      includeMarkdown('help.md')
    )
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

  output$log_in_page_text <- renderText({
    "Already Logged In. It is like Hotel California; once you are logged in, you can never get out."
  })


  user_input <- reactiveValues(authenticated = FALSE, status = "")

  observeEvent(input$login_button, {
      cache_init(base_url = 'staging', api_url = 'https://api.testing.cunybplservices.fun/', ssl_verify = FALSE)
      result = tryCatch({
                    fetch_auth_token(input$user_name, input$password)},
                    error = function(e){
                      print(e)
                      return(NULL)
                    }
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

  output$sqft_wiggy <- renderUI({
    checkboxInput('sqft_fl', 'Normalized by sqft', value = TRUE, width = NULL)
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

  output$model_base_wiggy <- renderUI({
    tagList(
      selectInput('model_base', 'Choose (new BEMA) Model to visualize', choices = base_model_df()$label, selected = choose_best_func(base_model_df()), multiple = FALSE,
  selectize = TRUE, width = NULL, size = NULL)
    )
  })

  output$model_retro_wiggy_pre <- renderUI({
    tagList(
      selectInput('model_retro_pre', 'Choose Pre Model to visualize', choices = retro_model_df_pre()$label, selected = choose_best_func(retro_model_df_pre()), multiple = FALSE,
  selectize = TRUE, width = NULL, size = NULL)
    )
  })

  output$model_retro_wiggy_post <- renderUI({
    tagList(
      selectInput('model_retro_post', 'Choose Post Model to visualize', choices = retro_model_df_post()$label, selected = choose_best_func(retro_model_df_post()), multiple = FALSE,
  selectize = TRUE, width = NULL, size = NULL)
    )
  })

  output$model_port_wiggy <- renderUI({
    tagList(
      selectInput('model_port', 'Choose (old BEMA) Model to visualize', choices = port_model_df()$label, selected = choose_best_func(port_model_df()), multiple = FALSE,
  selectize = TRUE, width = NULL, size = NULL)
    )
  })

  ##########################################
  ######## bema model config wiggy #########
  ##########################################

  ############### Baseline #################

  output$elec_model_ord_base_wiggy <- renderUI({
    tagList(
      selectizeInput('elec_model_ord_base', 'Elec Model Ordering', choices = c('3PC', '3PH', '4P', '5P', '2P'),
      selected = c('3PH', '3PC', '4P', '5P', '2P'), multiple = TRUE, width = NULL, size = NULL)
    )
  })

  output$fuel_model_ord_base_wiggy <- renderUI({
    tagList(
      selectizeInput('fuel_model_ord_base', 'Fuel Model Ordering', choices = c('3PH', '3PC', '4P', '5P', '2P'),
      selected = c('3PH', '3PC', '4P', '5P', '2P'), multiple = TRUE, width = NULL, size = NULL)
    )
  })

  output$elec_r2_base_wiggy <-renderUI({
    numericInput('elec_r2_base', 'Elec Rsquared Threshold', value = 0.75, min = 0, max = 1, step = 0.01, width = NULL)
  })

  output$fuel_r2_base_wiggy <-renderUI({
    numericInput('fuel_r2_base', 'Fuel Rsquared Threshold', value = 0.75, min = 0, max = 1, step = 0.01, width = NULL)
  })

  output$elec_cvrmse_base_wiggy <-renderUI({
    numericInput('elec_cvrmse_base', 'Elec CVRMSE Threshold', value = 0.25, min = 0, max = 1, step = 0.01, width = NULL)
  })

  output$fuel_cvrmse_base_wiggy <-renderUI({
    numericInput('fuel_cvrmse_base', 'Fuel CVRMSE Threshold', value = 0.50, min = 0, max = 1, step = 0.01, width = NULL)
  })

  output$main_test_base_wiggy <- renderUI({
    checkboxInput('main_test_base', 'Bypass Tests', value = FALSE, width = NULL)
  })

  output$all_models_base_wiggy <- renderUI({
    checkboxInput('all_models_base', 'All Models', value = FALSE, width = NULL)
  })

  output$relax_base_wiggy <- renderUI({
    checkboxInput('relax_base', 'Relax', value = FALSE, width = NULL)
  })

  output$nac_base_wiggy <- renderUI({
    checkboxInput('nac_base', 'Include NAC', value = FALSE, width = NULL)
  })

  ################ Retrofit ################

  output$elec_model_ord_retro_wiggy <- renderUI({
    tagList(
      selectizeInput('elec_model_ord_retro', 'Elec Model Ordering', choices = c('3PC', '3PH', '4P', '5P', '2P'),
      selected = c('3PC', '3PH', '4P', '5P', '2P'), multiple = TRUE, width = NULL, size = NULL)
    )
  })

  output$fuel_model_ord_retro_wiggy <- renderUI({
    tagList(
      selectizeInput('fuel_model_ord_retro', 'Fuel Model Ordering', choices = c('3PH', '3PC', '4P', '5P', '2P'),
      selected = c('3PH', '3PC', '4P', '5P', '2P'), multiple = TRUE, width = NULL, size = NULL)
    )
  })

  output$elec_r2_retro_wiggy <-renderUI({
    numericInput('elec_r2_retro', 'Elec Rsquared Threshold', value = 0.75, min = 0, max = 1, step = 0.01, width = NULL)
  })

  output$fuel_r2_retro_wiggy <-renderUI({
    numericInput('fuel_r2_retro', 'Fuel Rsquared Threshold', value = 0.75, min = 0, max = 1, step = 0.01, width = NULL)
  })

  output$elec_cvrmse_retro_wiggy <-renderUI({
    numericInput('elec_cvrmse_retro', 'Elec CVRMSE Threshold', value = 0.25, min = 0, max = 1, step = 0.01, width = NULL)
  })

  output$fuel_cvrmse_retro_wiggy <-renderUI({
    numericInput('fuel_cvrmse_retro', 'Fuel CVRMSE Threshold', value = 0.50, min = 0, max = 1, step = 0.01, width = NULL)
  })

  output$main_test_retro_wiggy <- renderUI({
    checkboxInput('main_test_retro', 'Bypass Tests', value = FALSE, width = NULL)
  })

  output$all_models_retro_wiggy <- renderUI({
    checkboxInput('all_models_retro', 'All Models', value = FALSE, width = NULL)
  })

  output$relax_retro_wiggy <- renderUI({
    checkboxInput('relax_retro', 'Relax', value = TRUE, width = NULL)
  })

  output$nac_retro_wiggy <- renderUI({
    checkboxInput('nac_retro', 'Include nac', value = FALSE, width = NULL)
  })

  ##########################################
  ############# Prepare Queries ############
  ##########################################

  baseline_query <- reactive({list(bdbid = input$bdbid, period = input$period, no_sqft=!input$sqft_fl,
                          target_date = input$tar_date,
                          relax = input$relax_base, elec_model_ordering = char_to_vec(input$elec_model_ord_base),
                          fuel_model_ordering = char_to_vec(input$fuel_model_ord_base),
                          elec_r2_threshold = input$elec_r2_base, elec_cvrmse_threshold = input$elec_cvrmse_base,
                          fuel_r2_threshold = input$fuel_r2_base, fuel_cvrmse_threshold = input$fuel_cvrmse_base,
                          all_models = input$all_models_base, ignore_main_test = input$main_test_base,
                          include_nac = input$nac_base)
                          })

  retro_query <- reactive({list(bdbid = input$bdbid, period = input$period, no_sqft=!input$sqft_fl,
                          retrofit_start_date = input$start_date, retrofit_end_date = input$end_date,
                          relax = input$relax_retro, elec_model_ordering = char_to_vec(input$elec_model_ord_retro),
                          fuel_model_ordering = char_to_vec(input$fuel_model_ord_retro),
                          elec_r2_threshold = input$elec_r2_retro, elec_cvrmse_threshold = input$elec_cvrmse_retro,
                          fuel_r2_threshold = input$fuel_r2_retro, fuel_cvrmse_threshold = input$fuel_cvrmse_retro,
                          all_models = input$all_models_retro, ignore_main_test = input$main_test_retro,
                          include_nac = input$nac_retro)
                          })

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
      df = baseline_batch()$utility
      if (length(df)){
        colnames(df)[colnames(df) == 'id'] <- 'bdbid'
      }
      return(df)
  })

  best_base <- reactive({
    df = baseline_batch()$changepoint
    if(length(df)){
      colnames(df)[colnames(df) == 'id'] <- 'bdbid'
      best_cols = c("fiscal_year", "period", "tmin", "tmax", "n", "session_id", "nac")
      df = missing_cols_handler(best_cols, df)
      df = df [, !(colnames(df) %in% 'y_predict')]
    }else{
      print('bdfdsfsdf')
      df = NULL
    }
    return(df)
  })

  post_base <- reactive({
    df = baseline_batch()$post
    if(length(df))
    {
      colnames(df)[colnames(df) == 'id'] <- 'bdbid'
      post_cols = c("sitename", 'period', "session_id")
      df = missing_cols_handler(post_cols, df)
    }else{
      df = NULL
    }
    return(df)
  })

  ################## Retrofit #################

  utility_retro <- reactive({
    retro_batch()$utility
  })

  best_retro_org <- reactive({
    df = retro_batch()$changepoint
    if(length(df)){
      best_cols = c("fiscal_year", "period", "tmin", "tmax", "n", "session_id", "nac")
      df = missing_cols_handler(best_cols, df)
      df = df [, !(colnames(df) %in% 'y_predict')]
    }else{
      df = NULL
    }
    return(df)
  })

  best_retro <- reactive({
    if (length(best_retro_org())){
      pre_df = subset(best_retro_org(), best_retro_org()$prepost == 1 & best_retro_org()$model_type == retro_model_type_pre())
      post_df = subset(best_retro_org(), best_retro_org()$prepost == 3 & best_retro_org()$model_type == retro_model_type_post())
      return(rbind(pre_df, post_df))
    }
    return(NULL)
  })

  post_retro_org <- reactive({
    df = retro_batch()$post
    if(length(df))
    {
      post_cols = c("sitename", 'period', "session_id")
      df = missing_cols_handler(post_cols, df)
    }else{
      df = NULL
    }
    return(df)
  })

  post_retro <- reactive({
    if(length(post_retro_org())){
      pre_df = subset(post_retro_org(), post_retro_org()$prepost == 1 & post_retro_org()$model_type == retro_model_type_pre())
      post_df = subset(post_retro_org(), post_retro_org()$prepost == 3 & post_retro_org()$model_type == retro_model_type_post())
      return(rbind(pre_df, post_df))
    }
    return(NULL)
  })




  ################## energy #################

  util_energy <- reactive({
    if (flag_func(utility_retro(), input$bdbid, input$energy_type)){
      return(subset(utility_retro(), bdbid == input$bdbid & energy_type == input$energy_type))
    }
    NULL
  })

  energy_null_flag <- reactive({is.null(util_energy())})

  ################### model ###################

  base_model_df <- reactive({
    create_model_df(best_base(), input$energy_type)
  })

  retro_model_df_pre <- reactive({
    create_model_df(best_retro_org(), input$energy_type)
  })

  retro_model_df_post <- reactive({
    create_model_df(best_retro_org(), input$energy_type, 3)
  })

  base_model_type <- reactive({
    as.character(subset(base_model_df()$model_type, base_model_df()$label == input$model_base))
  })

  retro_model_type_pre <- reactive({
    as.character(subset(retro_model_df_pre()$model_type, retro_model_df_pre()$label == input$model_retro_pre))
  })

  retro_model_type_post <- reactive({
    as.character(subset(retro_model_df_post()$model_type, retro_model_df_post()$label == input$model_retro_post))
  })

  ################### port ###################

  port_model_df <- reactive({
    create_model_df(best_port_base(), input$energy_type)
  })

  port_model_type <- reactive({
    as.character(subset(port_model_df()$model_type, port_model_df()$label == input$model_port))
  })

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
      plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = paste('No usage points for', input$energy_type),  plot_bgcolor='rgb(240, 242, 247)', paper_bgcolor='rgb(240, 242, 247)')
    }
  })

  output$base_param_plot <- renderPlotly({
    utility = subset(utility_base(), utility_base()$bdbid == input$bdbid & utility_base()$energy_type == input$energy_type)
    if(length(utility) & nrow(utility)){
      best_model = subset(best_base(), best_base()$bdbid == input$bdbid & best_base()$energy_type == input$energy_type & best_base()$model_type == base_model_type())
      main_param_plot(utility, best_model, b_name())
    }else
    {
      plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = paste('No usage points for', input$energy_type),  plot_bgcolor='rgb(240, 242, 247)', paper_bgcolor='rgb(240, 242, 247)')
    }
  })

  output$base_param_df <- renderTable({
      params_table(best_base(), input$bdbid, input$energy_type, 1, base_model_type())
  }, align = 'l', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$base_stat_df <- renderTable({
      stat_table(best_base(), input$bdbid, input$energy_type, base_model_type())
  }, align = 'l', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$base_post_df <- renderTable({
    if (!flag_func(post_base(), input$bdbid, input$energy_type))
    {
      return(NULL)
    }
    post_output_df_server(post_base(), input$bdbid, input$energy_type, area_info(), input$period, base_model_type())
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
      plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = paste('No data points for', input$energy_type),  plot_bgcolor='rgb(240, 242, 247)', paper_bgcolor='rgb(240, 242, 247)')
    }else
    {
      plot_timeseries(util_energy(), 'Elec')
    }
  })

  output$retro_param_plot <- renderPlotly({
    if (energy_null_flag())
    {
      return(plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = paste('No data points for', input$energy_type), plot_bgcolor='rgb(240, 242, 247)', paper_bgcolor='rgb(240, 242, 247)'))
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
    if (!flag_func(post_retro(), input$bdbid, input$energy_type))
    {
      return(NULL)
    }
    post_output_df = post_output_retrofit(post_retro(), input$bdbid, input$energy_type)
    post_output_df = post_col_retrofit(post_output_df, input$period, input$energy_type)
    }, align = 'c', rownames = TRUE, colnames = TRUE, width = "auto", digits = 7)


  ########################################
  ############## Portfolio ###############
  ########################################

  ############ input for query ###########

  fiscal_year <- reactive({
    fiscal_year_shcemea(input$tar_date, input$period)
  })

  ############### get data ###############

  utility_port_base <- reactive({
    df = fetch_request('portfolios/utility/', query_params=list(bdbid = input$bdbid, fiscal_year = fiscal_year()))$result
    if (length(df))
    {
      colnames(df)[colnames(df) == 'OAT'] = 'oat'
      util_cols = c("using_sqft", "using_fuel_oil")
      df = missing_cols_handler(util_cols, df)
      return(df)
    }else
    {
      return(NULL)
    }
  })

  best_port_base <- reactive({
    df = fetch_request('portfolios/changepoint-model/', query_params=list(bdbid = input$bdbid, fiscal_year = fiscal_year()))$result

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

  post_port_base <- reactive({
    if(is.null(best_port_base()))
    {
      return(NULL)
    }

    df = fetch_request('portfolios/bestmodel-loads-sensitivity/', query_params=list(bdbid = input$bdbid, fiscal_year = fiscal_year()))$result
    post_cols = c("sitename", 'period', 'percent_cooling', 'percent_heating', 'percent_baseload', "session_id")
    df = missing_cols_handler(post_cols, df)
    return(df)
  })

  ############### output #################

  output$port_base_param_plot <- renderPlotly({
    utility = subset(utility_port_base(), utility_port_base()$bdbid == input$bdbid & utility_port_base()$energy_type == input$energy_type)
    if(length(utility)){
      best_model = subset(best_port_base(), best_port_base()$bdbid == input$bdbid & best_port_base()$energy_type == input$energy_type & best_port_base()$model_type == port_model_type())
      main_param_plot(utility, best_model, b_name())
    }else
    {
      plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = paste('No usage points for', input$energy_type),  plot_bgcolor='rgb(240, 242, 247)', paper_bgcolor='rgb(240, 242, 247)')
    }
  })

  output$port_base_param_df <- renderTable({
      params_table(best_port_base(), input$bdbid, input$energy_type, 1, port_model_type())
  }, align = 'l', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$port_base_stat_df <- renderTable({
      stat_table(best_port_base(), input$bdbid, input$energy_type, port_model_type())
  }, align = 'l', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$port_base_post_df <- renderTable({
    if (!flag_func(post_port_base(), input$bdbid, input$energy_type))
    {
      return(NULL)
    }
    post_output_df_server(post_port_base(), input$bdbid, input$energy_type, area_info(), input$period)
  }, align = 'l', rownames = TRUE, colnames = TRUE, width = "auto", digits = 7)


})
