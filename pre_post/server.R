library(shiny)
library(plotly)


server <- function(input, output, session) {

  best_model_org <- reactive({
    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)

    read.csv(inFile$datapath)
    })

  best_model <- reactive(({
    filter_project_session(best_model_org(), input$session, input$project)
    }))

  org_util_df <- reactive({
    inFile2 <- input$file2

    if (is.null(inFile2))
      return(NULL)

    df = read.csv(inFile2$datapath)
    colnames(df)[colnames(df) == 'oat'] <- 'OAT'
    util_cols = c("normalized_usage", "noaa_temps", "adjusted_usage", "project_id", "timestamp")
    df = missing_cols_handler(util_cols, df)
    df = fixed_time(df)
    })

  session_vec <- reactive({
    unique(org_util_df()$timestamp)
    })

  output$session_out <- renderUI({
    tagList(
      selectInput('session', 'Choose session', session_vec(), multiple = FALSE,selectize = TRUE, width = NULL, size = NULL)
    )
  })

  project_vec <- reactive({
    if (input$session == 'NA')
    {
      return(NA)
    }else{
      
      return(subset(org_util_df()$project_id, org_util_df()$timestamp == input$session))
    }
    })

  output$project_out <- renderUI({
    tagList(
      selectInput('project', 'Choose project', project_vec(), multiple = FALSE,selectize = TRUE, width = NULL, size = NULL)
    )
  })

  temp_df <- reactive({
    filter_project_session(org_util_df(), input$session, input$project)
    })

  adjust_saving_org <- reactive({
    inFile3 <- input$file3

    if (is.null(inFile3))
      return(NULL)

    df = read.csv(inFile3$datapath)
    df = missing_cols_handler(c('adjusted_pecent_savings'), df)
  })

  adjust_saving <- reactive({
    filter_project_session(adjust_saving_org(), input$session, input$project)
    })

  normalized_saving_org <- reactive({
    inFile4 <- input$file4

    if(is.null(inFile4))
      return(NULL)

    df = read.csv(inFile4$datapath)
    df = missing_cols_handler(c('normalized_pecent_savings'), df)
  })

  normalized_saving <- reactive({
    filter_project_session(normalized_saving_org(), input$session, input$project)
    })

  post_df_org <- reactive({
    inFile5 <- input$file5

    if(is.null(inFile5))
      return(NULL)

    df = read.csv(inFile5$datapath)
    df = percent_heat_cool_func(df)
    #df = missing_cols_handler(c('percent_heating', 'percent_cooling', 'percent_baseload'), df)
  })

  post_df <- reactive({
    filter_project_session(post_df_org(), input$session, input$project)
    })

  binfo_df <- reactive({
    inFile6 <- input$file6

    if(is.null(inFile6))
      return(NULL)

    read.csv(inFile6$datapath)
  })

  b_df <- reactive({
    if(!is.null(binfo_df()))
    { 
      temp_index = unique(match(temp_df()$bdbid, binfo_df()$bdbid))
      b_name = binfo_df()$building_name[temp_index]
      b_id = binfo_df()$bdbid[temp_index]
      comb = paste(b_id, b_name, sep = ' - ')
      b_df = data.frame(bdbid = b_id, name = comb)
    }else
    { 
      bdbid = unique(temp_df()$bdbid)
      b_df = data.frame(bdbid = bdbid, name = bdbid)
    }
    b_df
  })

  output$first <- renderUI({
    tagList(
      selectInput('bdbid', 'Choose Facility', b_df()$name, selected = b_df()$name[1], multiple = FALSE,selectize = TRUE, width = NULL, size = NULL)
    )
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

  bdbid_n <- reactive({
    b_df()$bdbid[b_df()$name == input$bdbid]
  })

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

  bdbid_n <- reactive({
    b_df()$bdbid[b_df()$name == input$bdbid]
  })

  
  ################################################
  ################## PREPARE FILE ################
  ################################################

  util_elec <- reactive({
    if (flag_func(temp_df(), bdbid_n(), 'Elec')){
      return(subset(temp_df(), bdbid == bdbid_n() & energy_type == 'Elec'))
    }
    NULL
    })

  util_fuel <- reactive({
    if (flag_func(temp_df(), bdbid_n(), 'Fuel')){
      return(subset(temp_df(), bdbid == bdbid_n() & energy_type == 'Fuel'))
    }
    NULL
  })

  elec_null_flag <- reactive({is.null(util_elec())})

  fuel_null_flag <- reactive({is.null(util_fuel())})

  b_name <- reactive({get_building_name(binfo_df(), bdbid_n())})

  ################################################
  ################## ELEC PLOT ###################
  ################################################

  output$plot_time_elec <- renderPlotly({
    if (elec_null_flag())
    { 
      plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = 'No data points for Elec')
    }else
    {
      plot_timeseries(util_elec(), 'Elec')
    }
  })

  output$plot_model_elec <- renderPlotly({
    if (elec_null_flag())
    {
      return(plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = 'No data points for Elec'))
    }
    main_plot_model(util_elec(), best_model(), bdbid_n(), 'Elec', b_name())
    })

  output$plot_adjust_elec <- renderPlotly({
    if(elec_null_flag())
    {
      return(plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = 'No data points for Elec'))
    }
    main_plot_baseload(util_elec(), best_model(), bdbid_n(), 'Elec', b_name())
    })

  output$plot_norm_elec <- renderPlotly({
    flag = length(util_elec()$normalized_usage[!is.na(util_elec()$normalized_usage)])
    if(flag)
    {
      return(plot_normalized_graph(util_elec(), 'Elec', b_name()))
    }
      plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = 'No data points for Elec')
    })

  ################################################
  ################ ELEC TABLES ###################
  ################################################

  output$post_elec <- renderTable({
    n = ifelse(flag_func(best_model(), bdbid_n(), 'Elec'), subset(best_model()$n, best_model()$bdbid == bdbid_n() & best_model()$energy_type == 'Elec'), 0)
    post_output_df = post_output(post_df(), bdbid_n(), 'Elec')
    post_output_df = post_col(post_output_df, n, 'Elec')
    }, align = 'c', rownames = TRUE, colnames = TRUE, width = "auto", digits = 7)

  output$stat_df_elec <- renderTable({
    stat_table(best_model(), bdbid_n(), 'Elec')
  }, align = 'c', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$params_df_elec <- renderTable({
    params_table(best_model(), bdbid_n(), 'Elec')
  }, align = 'c', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$norm_saving_elec <- renderTable({
    construct_saving_table(normalized_saving(), 'Elec', bdbid_n(), 'normalized')
  }, align = 'c', colnames = TRUE, width = "auto", digits = 7)

  output$adjust_saving_elec <- renderTable({
    construct_saving_table(adjust_saving(), 'Elec', bdbid_n(), 'adjusted')
  }, align = 'c', colnames = TRUE, width = "auto", digits = 7)

  ################################################
  ################## FUEL PLOT ###################
  ################################################

  output$plot_time_fuel <- renderPlotly({
    if (fuel_null_flag())
    { 
      plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = 'No data points for Fuel')
    }else
    {
      plot_timeseries(util_fuel(), 'Fuel')
    }
  })

  output$plot_model_fuel <- renderPlotly({
    if (fuel_null_flag())
    {
      return(plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = 'No data points for Fuel'))
    }
    main_plot_model(util_fuel(), best_model(), bdbid_n(), 'Fuel', b_name())
    })

  output$plot_adjust_fuel <- renderPlotly({
    if(fuel_null_flag())
    {
      return(plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = 'No data points for Fuel'))
    }
    main_plot_baseload(util_fuel(), best_model(), bdbid_n(), 'Fuel', b_name())
    })

  output$plot_norm_fuel <- renderPlotly({
    flag = length(util_fuel()$normalized_usage[!is.na(util_fuel()$normalized_usage)])
    if(flag)
    {
      return(plot_normalized_graph(util_fuel(), 'Fuel', b_name()))
    }
      plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = 'No data points for Fuel')
    })

  ################################################
  ################ FUEL TABLES ###################
  ################################################

  output$post_fuel <- renderTable({
    n = ifelse(flag_func(best_model(), bdbid_n(), 'Fuel'), subset(best_model()$n, best_model()$bdbid == bdbid_n() & best_model()$energy_type == 'Fuel'), 0)
    post_output_df = post_output(post_df(), bdbid_n(), 'Fuel')
    post_output_df = post_col(post_output_df, n, 'Fuel')
    }, align = 'c', rownames = TRUE, colnames = TRUE, width = "auto", digits = 7)

  output$stat_df_fuel <- renderTable({
    stat_table(best_model(), bdbid_n(), 'Fuel')
  }, align = 'c', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$params_df_fuel <- renderTable({
    params_table(best_model(), bdbid_n(), 'Fuel')
  }, align = 'c', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$norm_saving_fuel <- renderTable({
    construct_saving_table(normalized_saving(), 'Fuel', bdbid_n(), 'normalized')
  }, align = 'c', colnames = TRUE, width = "auto", digits = 7)

  output$adjust_saving_fuel <- renderTable({
    construct_saving_table(adjust_saving(), 'Fuel', bdbid_n(), 'adjusted')
  }, align = 'c', colnames = TRUE, width = "auto", digits = 7)

  ################################################
  ############### multi plots ####################
  ################################################

  output$plots <- renderUI({
    plot_output_list <- lapply(1:length(unique(temp_df()$bdbid)), function(i) {
      lapply(1:3, function(j){
      bdbid_n <- unique(temp_df()$bdbid)[i]

      if(j == 1)
      {
        plotname <- paste("plot", bdbid_n, sep="")
        output[[plotname]] <- renderPlotly({
          b_name = get_building_name(binfo_df(), bdbid_n)
          util = subset(temp_df(), bdbid == bdbid_n & energy_type == 'Elec')
          main_plot_model(util, best_model(), bdbid_n, 'Elec', b_name)
          #main_plot(temp_df(), best_model(), bdbid_n, 'Elec', b_name)
        })
        plotlyOutput(plotname)
      }else if(j == 2)
      { tablename1 <- paste("table", bdbid_n, sep="")
        output[[tablename1]] <- renderTable({
          stat_param_table(best_model(), bdbid_n, 'Elec')
          }, align = 'c', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)
        tableOutput(tablename1)
      }else
      { 
        linebreak <- paste("break", bdbid_n, sep="") 
        output[[linebreak]] <- renderUI({HTML("<br/>")})
        uiOutput(linebreak)
      }
    })
    }
    )

    do.call(tagList, plot_output_list)
  })

  output$plots2 <- renderUI({
    plot_output_list <- lapply(1:length(unique(temp_df()$bdbid)), function(i) {
      lapply(1:3, function(j){
      bdbid_n <- unique(temp_df()$bdbid)[i]

      if(j == 1)
      {
        plotname2 <- paste("plot2", bdbid_n, sep="")
        output[[plotname2]] <- renderPlotly({
          b_name = get_building_name(binfo_df(), bdbid_n)
          util = subset(temp_df(), bdbid == bdbid_n & energy_type == 'Fuel')
          main_plot_model(util, best_model(), bdbid_n, 'Fuel', b_name)
        })
        plotlyOutput(plotname2)
      }else if(j == 2)
      { tablename2 <- paste("table2", bdbid_n, sep="")
        output[[tablename2]] <- renderTable({
          stat_param_table(best_model(), bdbid_n, 'Fuel')
          }, align = 'c', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)
        tableOutput(tablename2)
      }else
      { 
        linebreak2  <- paste("break2", bdbid_n, sep="") 
        output[[linebreak2]] <- renderUI({HTML("<br/>")})
        uiOutput(linebreak2)
      }
    })
    }
    )

    do.call(tagList, plot_output_list)
  })
  

  ##############################################
  ########## BUILDING INFO AND HELP ############
  ##############################################

  output$binfo_df1 <- renderTable({
    binfo_output_list()$binfo_df1
  }, align = 'l', colnames = FALSE, width = "auto")

  output$binfo_df2 <- renderTable({
    binfo_output_list()$binfo_df2
  }, align = 'l', colnames = FALSE, rownames = TRUE, width = "auto")

  output$binfo_df12 <- renderTable({
    binfo_output_list()$binfo_df1
  }, align = 'l', colnames = FALSE, width = "auto")

  output$binfo_df22 <- renderTable({
    binfo_output_list()$binfo_df2
  }, align = 'l', colnames = FALSE, rownames = TRUE, width = "auto")

  output$help <- renderTable({
    help_table()
  }, align = 'l', colnames = TRUE, width = "550")

}