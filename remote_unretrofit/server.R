library(shiny)
library(scales)
library(rdrop2)
library(plotly)

server <- function(input, output, session) {

  dd_df <- reactive({
    withProgress(message = 'Getting data', style = 'notification', value = 0.25, {
    setProgress(0.5)
    temp = get_dropdown_info_2()
    setProgress(1)
    })
    temp
  })

  output$cat_ui <- renderUI({
    tagList(
      selectInput('category', 'Choose Category', unique(dd_df()$category), selected = dd_df()$category[1], multiple = FALSE,selectize = TRUE, width = NULL, size = NULL)
    )
  })

  p_df <- reactive({
    get_dd_period(dd_df(), input$category)
    })

  output$point_ui<- renderUI({
    tagList(
      selectInput('period', 'Choose period', c(24), selected = subset(p_df()$p1, p_df()$p1 == 24), multiple = FALSE,selectize = TRUE, width = NULL, size = NULL)
    )
  })

  fy_df <- reactive({
    get_dd_year(p_df(), input$period)
    })

  output$fy_ui <- renderUI({
    tagList(
      selectInput('year', 'Choose fiscal year', fy_df()$fy0, multiple = FALSE,selectize = TRUE, width = NULL, size = NULL)
    )
  })



  best_model <- reactive({
    path= subset(fy_df()$path, fy_df()$fy0 == input$year)
    fileName=paste("all_models_",input$category,"_fy",input$year,"_p",input$period,".csv",sep="")
    df = tryCatch(drop_read_csv(paste(path,fileName,sep="/")), error = function(e){return(NULL)})
    if (is.null(df))
    {
      fileName=paste("best_models_",input$category,"_fy",input$year,"_p",input$period,".csv",sep="")
      df = tryCatch(drop_read_csv(paste(path,fileName,sep="/")), error = function(e){return(NULL)})
    }
    return(df)
    })

  temp_df <- reactive({
    path=subset(fy_df()$path, fy_df()$fy0 == input$year)
    fileName=paste("utility_",input$category,"_fy",input$year,"_p",input$period,".csv",sep="")
    df = tryCatch(drop_read_csv(paste(path,fileName,sep="/")), error = function(e){return(NULL)})


    if(!is.null(df))
    {
      util_cols = c("using_fuel_oil", "using_sqft")
      df = missing_cols_handler(util_cols, df)
    }
    return(df)
    })

  binfo_df <- reactive({
    #path=subset(fy_df()$path, fy_df()$fy0 == input$year)
    #fileName=paste("buildings_info_",input$category,"_fy",input$year,"_p",input$period,".csv",sep="")
    #df = tryCatch(drop_read_csv(paste(path,fileName,sep="/")), error = function(e){return(NULL)})

    load("building_info.RData", df <- new.env()) 
    df = df$binfo_df
    return(df)
    })

  post_df <- reactive({
    path=subset(fy_df()$path, fy_df()$fy0 == input$year)
    fileName=paste("post_models_",input$category,"_fy",input$year,"_p",input$period,".csv",sep="")
    df = tryCatch(drop_read_csv(paste(path,fileName,sep="/")), error = function(e){return(NULL)})
    return(df)
  })

  co2eui_df <- reactive({
    path=subset(fy_df()$path, fy_df()$fy0 == input$year)
    fileName=paste("co2eui_rank_",input$category,"_fy",input$year,"_p",input$period,".csv",sep="")
    df = tryCatch(drop_read_csv(paste(path,fileName,sep="/")), error = function(e){return(NULL)})
    return(df)
  })

  breakdown_df <- reactive({
    path=subset(fy_df()$path, fy_df()$fy0 == input$year)
    fileName=paste("co2e_breakdown_",input$category,"_fy",input$year,"_p",input$period,".csv",sep="")
    df = tryCatch(drop_read_csv(paste(path,fileName,sep="/")), error = function(e){return(NULL)})
    return(df)
  })


  b_df <- reactive({
    if(!is.null(binfo_df()))
    { 
      #temp_index = unique(match(temp_df()$bdbid, binfo_df()$bdbid))
      #b_name = binfo_df()$building_name[temp_index]
      #b_id = binfo_df()$bdbid[temp_index]
      #comb = paste(b_id, b_name, sep = ' - ')
      b_df = construct_bdbid_name_func(temp_df(), binfo_df())
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

  energy <- reactive({
    temp_df()$energy_type[temp_df()$bdbid == bdbid_n()]
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


  figure <- reactive({
    require(ggplot2)
    temp_list = list()
    for (energy_n in c('Elec', 'Fuel'))
    {   
        b_name = get_building_name(binfo_df(), bdbid_n())
        if (length(best_model()$energy_type[best_model()$bdbid == bdbid_n()]) == 2)
        {   
            model_fig = list(final_figure = main_plot(temp_df(), best_model(), bdbid_n(), energy_n, b_name))
            model_fig$final_comb_figure = elec_fuel_graph_func(temp_df(), best_model(), bdbid_n(), height = input$height_ef, width = input$width_ef)
        }else
        { 
            #p_empty = ggplot() + geom_blank() + xlim(0, 10) + ylim(0, 10) + labs(title = paste('Only One energy type for', bdbid_n(), sep=" "))
            p_empty = plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = paste('Only one energy type for', bdbid_n()))
            model_fig = list(final_figure = main_plot(temp_df(), best_model(), bdbid_n(), energy_n, b_name), final_comb_figure = p_empty)
        }

        stat_df = stat_table(best_model(), bdbid_n(), energy_n)
        params_df = params_table(best_model(), bdbid_n(), energy_n)
        per_num_list = per_num_func(post_df(), bdbid_n(), energy_n)

        n = ifelse(flag_func(best_model(), bdbid_n(), energy_n), subset(best_model()$n, best_model()$bdbid == bdbid_n() & best_model()$energy_type == energy_n), 0)
        post_output_df = post_output(post_df(), bdbid_n(), energy_n)
        post_output_df = post_col(post_output_df, n, energy_n)


        flag_area = check_sqft_na(binfo_output_list()$binfo_df2)

        if(!is.null(binfo_df()) & length(post_output_df) & flag_area)
        {
          post_output_df = post_gross(post_output_df, binfo_output_list()$binfo_df2['Gross Square Feet',], energy_n)
        }else if(is.null(binfo_df()) & length(post_output_df))
        {
          post_output_df$Unretrofit = as.character(post_output_df$Unretrofit)
          post_output_df$Unretrofit[2:11] = as.character(round(as.numeric(post_output_df$Unretrofit[2:11]), 2))
          post_output_df$Unretrofit = prettyNum(post_output_df$Unretrofit, big.mark = ",", format = 'f')
        }

        if (bdbid_n() %in% unique(co2eui_df()$bdbid))
        {   
            co2_rank = co2_rank_get(co2eui_df(), bdbid_n())
            #co2_fig = co2eui_figure(co2_rank)
            co2_fig = percent_figure(co2_rank)
        }else
        {
            co2_fig = plotly_empty(type = 'scatter', mode = 'markers')
        }

        co2_numeric_table = co2_numeric_rank(co2eui_df(), bdbid_n())
        breakdown_table = co2_breakdown(breakdown_df(), bdbid_n())
        fuel_oil_flag = flag_fuel_oil(breakdown_df(), bdbid_n())

        temp_list[[energy_n]] = list(model = model_fig$final_figure, post_output_df = post_output_df,
          percent_fig = per_num_list$percent_fig, numeric_df = per_num_list$numeric_df, stat_df = stat_df, params_df = params_df,
          elec_fuel = model_fig$final_comb_figure, co2_fig = co2_fig, co2_numeric_table = co2_numeric_table, breakdown_table = breakdown_table, fuel_oil_flag = fuel_oil_flag)
    }
    temp_list
  })

  binfo_output_df2 <- reactive({
    binfo_df2 = binfo_output_list()$binfo_df2
    binfo_df2[3,] = round(as.numeric(binfo_df2[3,]), 2)
    binfo_df2[3,] = prettyNum(binfo_df2[3,], big.mark = ",", format = 'f')
    binfo_df2
  })
  
  output$help <- renderTable({
    help_table()
  }, align = 'l', colnames = TRUE, width = "550")

  output$binfo_df1 <- renderTable({
    binfo_output_list()$binfo_df1
  }, align = 'l', colnames = FALSE, width = "auto")

  output$binfo_df2 <- renderTable({
    binfo_output_df2()
  }, align = 'l', colnames = FALSE, rownames = TRUE, width = "auto")


  output$post <- renderTable({
    figure()$'Elec'$post_output_df
  }, align = 'c', rownames = TRUE, colnames = TRUE, width = "auto", digits = 7)

  output$numeric_df <- renderTable({
      figure()$'Elec'$numeric_df
  }, align = 'c', rownames = TRUE, colnames = TRUE, width = "auto", digits = NULL)

  output$stat_df <- renderTable({
      figure()$'Elec'$stat_df
  }, align = 'c', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$params_df <- renderTable({
      figure()$'Elec'$params_df
  }, align = 'c', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$co2_df <- renderTable({
    figure()$'Elec'$co2_numeric_table
    }, align = 'c', rownames = TRUE, colnames = TRUE, width = "auto", digits = NULL)

  output$breakdown_df <- renderTable({
    figure()$'Elec'$breakdown_table
    }, align = 'c', rownames = TRUE, colnames = FALSE, width = "auto", digits = NULL)

  output$fuel_oil <- renderText({
    fuel_oil_message(figure()$'Elec'$fuel_oil_flag)
  })


  output$plot2 <- renderPlotly({figure()$'Elec'$model})
  output$plot3 <- renderPlotly({
    if (flag_func(temp_df(), bdbid_n(), 'Elec'))
    { 
      util = subset(temp_df(), bdbid == bdbid_n() & energy_type == 'Elec')
      plot_timeseries_2(util, 'Elec')
    }else
    {
      plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = 'No usage points for Elec')
    }
  })
  output$plot1 <- renderPlotly({figure()$'Elec'$percent_fig})
  output$plot4 <- renderPlotly({figure()$'Elec'$elec_fuel})
  output$plot5 <- renderPlotly({figure()$'Elec'$co2_fig})

  output$post2 <- renderTable({
      figure()$'Fuel'$post_output_df
   }, align = 'c', rownames = TRUE, colnames = TRUE, width = "auto", digits = 0)
  output$numeric_df2 <- renderTable({
      figure()$'Fuel'$numeric_df
  }, align = 'c', rownames = TRUE, colnames = TRUE, width = "auto", digits = NULL)

  output$stat_df2 <- renderTable({
      figure()$'Fuel'$stat_df
  }, align = 'c', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$params_df2 <- renderTable({
      figure()$'Fuel'$params_df
  }, align = 'c', rownames = FALSE, colnames = TRUE, width = "auto", digits = NULL)


  output$plot22 <- renderPlotly({figure()$'Fuel'$model})
  output$plot32 <- renderPlotly({
    if (flag_func(temp_df(), bdbid_n(), 'Fuel'))
    { 
      util = subset(temp_df(), bdbid == bdbid_n() & energy_type == 'Fuel')
      plot_timeseries_2(util, 'Fuel')
    }else
    {
      plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = 'No usage points for Fuel')
    }
  })
  output$plot12 <- renderPlotly({figure()$'Fuel'$percent_fig})
  output$plot52 <- renderPlotly({figure()$'Fuel'$co2_fig})

  output$binfo_df12 <- renderTable({
    binfo_output_list()$binfo_df1
  }, align = 'l', colnames = FALSE, width = "auto")

  output$binfo_df22 <- renderTable({
    binfo_output_df2()
  }, align = 'l', colnames = FALSE, rownames = TRUE, width = "auto")

  output$co2_df2 <- renderTable({
    figure()$'Fuel'$co2_numeric_table
    }, align = 'c', rownames = TRUE, colnames = TRUE, width = "auto", digits = NULL)

  output$breakdown_df2 <- renderTable({
    figure()$'Fuel'$breakdown_table
    }, align = 'c', rownames = TRUE, colnames = FALSE, width = "auto", digits = NULL)

  output$fuel_oil2 <- renderText({
    fuel_oil_message(figure()$'Fuel'$fuel_oil_flag)
  })

  output$fuel_oil3 <- renderText({
    fuel_oil_message(figure()$'Fuel'$fuel_oil_flag)
  })

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

  output$plots <- renderUI({
    plot_output_list <- lapply(1:length(unique(temp_df()$bdbid)), function(i) {
      lapply(1:3, function(j){
      bdbid_n <- unique(temp_df()$bdbid)[i]

      if(j == 1)
      {
        plotname <- paste("plot_", bdbid_n, sep="")
        output[[plotname]] <- renderPlotly({
          b_name = get_building_name(binfo_df(), bdbid_n)
          main_plot(temp_df(), best_model(), bdbid_n, 'Elec', b_name)
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
        plotname2 <- paste("plot2_", bdbid_n, sep="")
        output[[plotname2]] <- renderPlotly({
          b_name = get_building_name(binfo_df(), bdbid_n)
          main_plot(temp_df(), best_model(), bdbid_n, 'Fuel', b_name)
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

  output$plots3 <- renderUI({

    boo <- unique(best_model()$bdbid)
    temp_bdbid = c()
    for (k in boo)
    {
      if (length(subset(best_model()$energy_type, best_model()$bdbid == k)) == 2)
      {
        temp_bdbid = c(temp_bdbid, k)
      }else
      {
        next
      }
    }


    plot_output_list <- lapply(1:length(temp_bdbid), function(i) {

      lapply(1:2, function(j){
      bdbid_n <- temp_bdbid[i]

      if(j == 1)
      {
        plotname3 <- paste("plot3_", bdbid_n, sep="")
        output[[plotname3]] <- renderPlotly({
          b_name = get_building_name(binfo_df(), bdbid_n)
          elec_fuel_graph_func(temp_df(), best_model(), bdbid_n, b_name)
        })
        plotlyOutput(plotname3)
      }else
      { 
        linebreak3  <- paste("break3", bdbid_n, sep="") 
        output[[linebreak3]] <- renderUI({HTML("<br/><br/>")})
        uiOutput(linebreak3)
      }
    })
    })

    do.call(tagList, plot_output_list)
  })

}



