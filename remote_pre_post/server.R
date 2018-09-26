library(shiny)
library(plotly)
library(rdrop2)


server <- function(input, output, session) {


  dd_df <- reactive({
    get_dropdown_info_2()
  })

  output$cat_ui <- renderUI({
    tagList(
      selectInput('category', 'Choose Category', unique(dd_df()$category), selected = dd_df()$category[1], multiple = FALSE,selectize = TRUE, width = NULL, size = NULL)
    )
  })


  p_df <- reactive({
    get_dd_date(dd_df(), input$category)
    })

  output$point_ui<- renderUI({
    tagList(
      selectInput('period', 'Choose period', p_df()$p0, multiple = FALSE,selectize = TRUE, width = NULL, size = NULL)
    )
  })


  best_model <- reactive({
    path = subset(p_df()$path, p_df()$p0 == input$period)
    fileName=paste("all_models_",input$category,"_",input$period,".csv",sep="")
    df = tryCatch(drop_read_csv(paste(path,fileName,sep="/")), error = function(e){return(NULL)})
    if (is.null(df))
    {
       fileName=paste("best_models_",input$category,"_",input$period,".csv",sep="")
      df = tryCatch(drop_read_csv(paste(path,fileName,sep="/")), error = function(e){return(NULL)})
    }
    return(df)
    })

  temp_df <- reactive({
    path = subset(p_df()$path, p_df()$p0 == input$period)
    fileName=paste("utility_",input$category,"_",input$period,".csv",sep="")
    df = tryCatch(drop_read_csv(paste(path,fileName,sep="/")), error = function(e){return(NULL)})
    return(df)
    })

  uncertainty_df <- reactive({
    path = subset(p_df()$path, p_df()$p0 == input$period)
    fileName=paste("savings_uncertainty_",input$category,"_",input$period,".csv",sep="")
    df = tryCatch(drop_read_csv(paste(path,fileName,sep="/")), error = function(e){return(NULL)})
    return(df)
  })

  savings_df <- reactive({
    path = subset(p_df()$path, p_df()$p0 == input$period)
    fileName=paste("total_savings_",input$category,"_",input$period,".csv",sep="")
    df = tryCatch(drop_read_csv(paste(path,fileName,sep="/")), error = function(e){return(NULL)})
    return(df)
  })

  post_df <- reactive({
    path = subset(p_df()$path, p_df()$p0 == input$period)
    fileName=paste("post_models_",input$category,"_",input$period,".csv",sep="")  # post_models_{}_{}
    df = tryCatch(drop_read_csv(paste(path,fileName,sep="/")), error = function(e){return(NULL)})
    return(df)
  })

  binfo_df <- reactive({
    path = subset(p_df()$path, p_df()$p0 == input$period)
    fileName=paste("building_info_",input$category,"_",input$period,".csv",sep="")
    df = tryCatch(drop_read_csv(paste(path,fileName,sep="/")), error = function(e){return(NULL)})
    return(df)
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


  figure <- reactive({
    require(ggplot2)
    temp_list = list()
    b_name = get_building_name(binfo_df(), bdbid_n())
    for (energy_n in c('Elec', 'Fuel'))
    {   
        if (flag_func(temp_df(), bdbid_n(), energy_n))
        { 
          util = subset(temp_df(), bdbid == bdbid_n() & energy_type == energy_n)
          model_fig = main_plot_model(util, best_model(), bdbid_n(), energy_n, b_name)
          adjust_fig = main_plot_baseload(util, best_model(), bdbid_n(), energy_n, b_name)
        }else
        {
          model_fig = plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = paste('No data points for', energy_n, 'for', b_name))
          adjust_fig = plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = paste('No data points for', energy_n, 'for', b_name))
        }

        savings = savings_table(uncertainty_df(), savings_df(),bdbid_n(), energy_n)

        n = ifelse(flag_func(best_model(), bdbid_n(), energy_n), subset(best_model()$n, best_model()$bdbid == bdbid_n() & best_model()$energy_type == energy_n), 0)
        post_output_df = post_output(post_df(), bdbid_n(), energy_n)
        post_output_df = post_col(post_output_df, n, energy_n)

        stat_df = stat_table(best_model(), bdbid_n(), energy_n)
        params_df = params_table(best_model(), bdbid_n(), energy_n)

        temp_list[[energy_n]]=list(adjust = adjust_fig, model = model_fig, savings = savings, post_output_df = post_output_df, stat_df = stat_df, params_df = params_df)
    }
    temp_list
  })
  
  output$help <- renderTable({
    help_table()
  }, align = 'l', colnames = TRUE, width = "550")

  output$savings <- renderTable({
    figure()$'Elec'$savings
  }, align = 'c', colnames = TRUE, width = "auto", digits = 7)

  output$post <- renderTable({
    figure()$'Elec'$post_output_df
  }, align = 'c', rownames = TRUE, colnames = TRUE, width = "auto", digits = 7)

  output$stat_df <- renderTable({
    figure()$'Elec'$stat_df
  }, align = 'c', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$params_df <- renderTable({
    figure()$'Elec'$params_df
  }, align = 'c', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)


  output$plot1 <- renderPlotly({figure()$'Elec'$adjust})
  output$plot2 <- renderPlotly({figure()$'Elec'$model})
  output$plot3 <- renderPlotly({
    if (flag_func(temp_df(), bdbid_n(), 'Elec'))
    { 
      util = subset(temp_df(), bdbid == bdbid_n() & energy_type == 'Elec')
      plot_timeseries_2(util, 'Elec')
    }else
    {
      plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = 'No data points for Elec')
    }
  })


  output$savings2 <- renderTable({
    figure()$'Fuel'$savings
  }, align = 'c', colnames = TRUE, width = "auto", digits = 7)

  output$post2 <- renderTable({
    figure()$'Fuel'$post_output_df
  }, align = 'c', rownames = TRUE, colnames = TRUE, width = "auto", digits = 7)

  output$stat_df2 <- renderTable({
    figure()$'Fuel'$stat_df
  }, align = 'c', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$params_df2 <- renderTable({
    figure()$'Fuel'$params_df
  }, align = 'c', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)


  output$plot12 <- renderPlotly({figure()$'Fuel'$adjust})
  output$plot22 <- renderPlotly({figure()$'Fuel'$model})
  output$plot32 <- renderPlotly({
    if (flag_func(temp_df(), bdbid_n(), 'Fuel'))
    { 
      util = subset(temp_df(), bdbid == bdbid_n() & energy_type == 'Fuel')
      plot_timeseries_2(util, 'Fuel')
    }else
    {
      plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = 'No data points for Fuel')
    }
  })

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

}