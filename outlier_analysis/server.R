library(shiny)
library(plotly)
#library(devtools)
#install_github('tinnaing347/bRema')
#library(bRema)

server <- function(input, output, session) {
	best_model <- reactive({
    	inFile <- input$file1

    	if (is.null(inFile))
      		return(NULL)
    read.csv(inFile$datapath)
    })

  	temp_df <- reactive({
    	inFile2 <- input$file2

    	if (is.null(inFile2))
     	 return(NULL)
    read.csv(inFile2$datapath)
    })

    binfo_df <- reactive({
    	inFile7 <- input$file7

    	if (is.null(inFile7))
      		return(NULL)

    read.csv(inFile7$datapath)
    })

    prev_df <- reactive({
      inFile9 <- input$file9

      if (is.null(inFile9))
       return(NULL)

      df = read.csv(inFile9$datapath)
      util_elec = previous_year(temp_df(), df, 'Elec')
      util_fuel = previous_year(temp_df(), df, 'Fuel')
      list(util_elec = util_elec, util_fuel = util_fuel)
    })

    agg_df <- reactive({
      aggregate_func(temp_df())
      })

    outlier_df<- reactive({
      out_elec = mahala_ellipse_func(temp_df(), 'Elec', bdbid_n())
      out_fuel = mahala_ellipse_func(temp_df(), 'Fuel', bdbid_n())
      final = rbind(out_elec, out_fuel)
      final = final[with(final, order(bdbid, energy_type)),]
      rownames(final) <- seq(length=nrow(final))
      return(final)
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
  	for (energy_n in c('Elec', 'Fuel')){

        source_n = ifelse(energy_n == 'Elec', 'E', 'F')
        #p_empty = plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = paste('Only one energy type for', bdbid_n()))
        model_fig = main_plot(temp_df(), best_model(), bdbid_n(), energy_n, bdbid_n(), source_n = source_n)
        stat_df = stat_table(best_model(), bdbid_n(), energy_n)
        params_df = params_table(best_model(), bdbid_n(), energy_n)

        temp_list[[energy_n]] = list(model = model_fig, stat_df = stat_df, params_df = params_df)
  	}
    temp_list
  })

  output$stat_df <- renderTable({
      figure()$'Elec'$stat_df
  }, align = 'c', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$params_df <- renderTable({
      figure()$'Elec'$params_df
  }, align = 'c', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$plot2 <- renderPlotly({figure()$'Elec'$model})

  temp_func_elec <- bind_values()
  temp_func_fuel <- bind_values()

  outlier_df_elec <- reactive({
      df_list_elec <- event_data("plotly_click", source = 'E')
      df_0_elec <- data.frame(OAT = df_list_elec$x, usage = df_list_elec$y) 
      df_final_elec <- temp_func_elec(df_0_elec)
  })

  outlier_df_fuel <- reactive({
      df_list_fuel <- event_data("plotly_click", source = 'F')
      df_0_fuel <- data.frame(OAT = df_list_fuel$x, usage = df_list_fuel$y) 
      df_final_fuel <- temp_func_fuel(df_0_fuel)
  })


  output$outlier_elec <- renderPrint({
    as.data.frame(outlier_df_elec())
    })

  output$outlier_fuel <- renderPrint({
    as.data.frame(outlier_df_fuel())
    })

  result_new_elec <- eventReactive(input$graph_elec,{
    if(length(outlier_df_elec()))
    {
      inter_outlier_ls = inter_outlier_func(temp_df(), outlier_df_elec(), bdbid_n(), 'Elec', TRUE, 'U')
      temp_func_elec(data.frame())
      if (length(inter_outlier_ls$final_result))
      {
        final_output = outlier_output_handler(inter_outlier_ls$final_result, inter_outlier_ls$new_util, inter_outlier_ls$out_util, 'Elec', "User's")
      }else
      {
        final_output = outlier_output_error(inter_outlier_ls$new_util, inter_outlier_ls$out_util, 'Elec', "User's")
      }
      list(stat = final_output$stat, params = final_output$params, figure = final_output$figure)
    }else
    { 
      list(stat = data.frame(), params = data.frame(), figure = plotly_empty(type = 'scatter', mode = 'markers'))
    }

  })


  result_new_fuel <- eventReactive(input$graph_fuel,{
    if(length(outlier_df_fuel()))
    { 
      inter_outlier_ls = inter_outlier_func(temp_df(), outlier_df_fuel(), bdbid_n(), 'Fuel', TRUE, 'U')
      temp_func_fuel(data.frame()) #this is just reseting

      if (length(inter_outlier_ls$final_result))
      {
        final_output = outlier_output_handler(inter_outlier_ls$final_result, inter_outlier_ls$new_util, inter_outlier_ls$out_util, 'Fuel', "User's")
      }else
      {
        final_output = outlier_output_error(inter_outlier_ls$new_util, inter_outlier_ls$out_util, 'Fuel', "User's")
      }
      list(stat = final_output$stat, params = final_output$params, figure = final_output$figure)
    }else
    {
      list(stat = data.frame(), params = data.frame(), figure = plotly_empty(type = 'scatter', mode = 'markers'))
    }
  })

  mahala_elec <- reactive({
    if (flag_func(temp_df(), bdbid_n(), 'Elec'))
    {
      inter_outlier_ls = inter_outlier_func(temp_df(), outlier_df(), bdbid_n(), 'Elec', FALSE, 'M')
      if (length(inter_outlier_ls$final_result))
      {
        final_output = outlier_output_handler(inter_outlier_ls$final_result, inter_outlier_ls$new_util, inter_outlier_ls$out_util, 'Elec', 'Mahalanobis')
      }else
      {
        final_output = outlier_output_error(inter_outlier_ls$new_util, inter_outlier_ls$out_util, 'Elec', 'Mahalanobis')
      }
    }else
    {
      final_output = graph_df_error_handler(bdbid_n(), 'Elec')
    }
    list(stat = final_output$stat, params = final_output$params, figure = final_output$figure)
    })

  ellipse_elec <- reactive({
    if (flag_func(temp_df(), bdbid_n(), 'Elec'))
    {
      inter_outlier_ls = inter_outlier_func(temp_df(), outlier_df(), bdbid_n(), 'Elec', FALSE, 'E')
      if (length(inter_outlier_ls$final_result))
      {
        final_output = outlier_output_handler(inter_outlier_ls$final_result, inter_outlier_ls$new_util, inter_outlier_ls$out_util, 'Elec', 'Ellipse')
      }else
      {
        final_output = outlier_output_error(inter_outlier_ls$new_util, inter_outlier_ls$out_util, 'Elec', 'Ellipse')
      }
    }else
    {
      final_output = graph_df_error_handler(bdbid_n(), 'Elec')
    }
    list(stat = final_output$stat, params = final_output$params, figure = final_output$figure)
    })

  mahala_fuel <- reactive({
    if (flag_func(temp_df(), bdbid_n(), 'Fuel'))
    {
      inter_outlier_ls = inter_outlier_func(temp_df(), outlier_df(), bdbid_n(), 'Fuel', FALSE, 'M')
      if (length(inter_outlier_ls$final_result))
      {
        final_output = outlier_output_handler(inter_outlier_ls$final_result, inter_outlier_ls$new_util, inter_outlier_ls$out_util, 'Fuel', 'Mahalanobis')
      }else
      {
        final_output = outlier_output_error(inter_outlier_ls$new_util, inter_outlier_ls$out_util, 'Fuel', 'Mahalanobis')
      }
    }else
    {
      final_output = graph_df_error_handler(bdbid_n(), 'Fuel')
    }

    list(stat = final_output$stat, params = final_output$params, figure = final_output$figure)
    })

  ellipse_fuel <- reactive({
    if (flag_func(temp_df(), bdbid_n(), 'Fuel'))
    {
      inter_outlier_ls = inter_outlier_func(temp_df(), outlier_df(), bdbid_n(), 'Fuel', FALSE, 'E')
      if (length(inter_outlier_ls$final_result))
      {
        final_output = outlier_output_handler(inter_outlier_ls$final_result, inter_outlier_ls$new_util, inter_outlier_ls$out_util, 'Fuel', 'Ellipse')
      }
      else
      {
        final_output = outlier_output_error(inter_outlier_ls$new_util, inter_outlier_ls$out_util, 'Fuel', 'Ellipse')
      }
    }else
    {
      final_output = graph_df_error_handler(bdbid_n(), 'Fuel')
    }
    list(stat = final_output$stat, params = final_output$params, figure = final_output$figure)
    })

  prev_result_elec <- reactive({
    if (flag_func(temp_df(), bdbid_n(), 'Elec') & !is.null(prev_df()))
    {
      new_util = subset(prev_df()$util_elec, prev_df()$util_elec$bdbid == bdbid_n() & prev_df()$util_elec$energy_type == 'Elec') #subsetting from previous_df, include previous year tag in estimated column
      inter_result = run_outlier_model(new_util)
      final_result = best_model_main_func(inter_result, 'Elec')
      if((length(final_result)))
      {
        final_output = prev_agg_output_handler(new_util, final_result, 'Elec', 'Prev')
      }else
      {
        final_output = prev_agg_output_error(new_util, 'Elec', 'Prev')
      }
    }else
    {
      final_output = graph_df_error_handler(bdbid_n(), 'Elec')
    }

    list(stat = final_output$stat, params = final_output$params, figure = final_output$figure)
    })

  prev_result_fuel <- reactive({
    if(flag_func(temp_df(), bdbid_n(), 'Fuel') & !is.null(prev_df()))
    {
        new_util = subset(prev_df()$util_fuel, prev_df()$util_fuel$bdbid == bdbid_n() & prev_df()$util_fuel$energy_type == 'Fuel') #subsetting from previous_df, include previous year tag in estimated column
        inter_result = run_outlier_model(new_util)
        final_result = best_model_main_func(inter_result, 'Fuel')
        if((length(final_result)))
        {
          final_output = prev_agg_output_handler(new_util, final_result, 'Fuel', 'Prev')
        }else
        {
          final_output = prev_agg_output_error(new_util, 'Fuel', 'Prev')
        }
    }else
    {
      final_output = graph_df_error_handler(bdbid_n(), 'Fuel')
    }
    list(stat = final_output$stat, params = final_output$params, figure = final_output$figure)
    })

  agg_result_elec <- reactive({

    if (flag_func(temp_df(), bdbid_n(), 'Elec'))
    {
      new_util = subset(agg_df(), agg_df()$bdbid == bdbid_n() & agg_df()$energy_type == 'Elec') #subsetting from previous_df, include previous year tag in estimated column
      inter_result = run_outlier_model(new_util)
      final_result = best_model_main_func(inter_result, 'Elec')
      if((length(final_result)))
      {
        final_output = prev_agg_output_handler(new_util, final_result, 'Elec', 'Agg')
      }else
      {
        final_output = prev_agg_output_error(new_util, 'Elec', 'Agg')
      }
    }else
    {
      final_output = graph_df_error_handler(bdbid_n(), 'Elec')
    }
    list(stat = final_output$stat, params = final_output$params, figure = final_output$figure)
    })

  agg_result_fuel <- reactive({
    if(flag_func(temp_df(), bdbid_n(), 'Fuel'))
    {
        new_util = subset(agg_df(), agg_df()$bdbid == bdbid_n() & agg_df()$energy_type == 'Fuel') #subsetting from previous_df, include previous year tag in estimated column
        inter_result = run_outlier_model(new_util)
        final_result = best_model_main_func(inter_result, 'Fuel')
        if((length(final_result)))
        {
          final_output = prev_agg_output_handler(new_util, final_result, 'Fuel', 'Agg')
        }else
        {
          final_output = prev_agg_output_error(new_util, 'Fuel', 'Agg')
        }
    }else
    {
      final_output = graph_df_error_handler(bdbid_n(), 'Fuel')
    }
    list(stat = final_output$stat, params = final_output$params, figure = final_output$figure)
  })

  output$plot_out <- renderPlotly({result_new_elec()$figure})

  output$plot_out2 <- renderPlotly({result_new_fuel()$figure})

  output$params_df_out <- renderTable({
      result_new_elec()$params
  }, align = 'c', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$stat_df_out <- renderTable({
      result_new_elec()$stat
  }, align = 'c', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$params_df_out2 <- renderTable({
      result_new_fuel()$params
  }, align = 'c', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$stat_df_out2 <- renderTable({
      result_new_fuel()$stat
  }, align = 'c', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$stat_df2 <- renderTable({
      figure()$'Fuel'$stat_df
  }, align = 'c', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$params_df2 <- renderTable({
      figure()$'Fuel'$params_df
  }, align = 'c', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)
  
  output$plot22 <- renderPlotly({figure()$'Fuel'$model})

  output$m_param <- renderTable({
      mahala_elec()$params
  }, align = 'c', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$m_stat <- renderTable({
      mahala_elec()$stat
  }, align = 'c', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$m_plot <- renderPlotly({mahala_elec()$figure})

  output$e_param <- renderTable({
      ellipse_elec()$params
  }, align = 'c', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$e_stat <- renderTable({
      ellipse_elec()$stat
  }, align = 'c', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$e_plot <- renderPlotly({ellipse_elec()$figure})

  output$m_param2 <- renderTable({
      mahala_fuel()$params
  }, align = 'c', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$m_stat2 <- renderTable({
      mahala_fuel()$stat
  }, align = 'c', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$m_plot2 <- renderPlotly({mahala_fuel()$figure})

  output$e_param2 <- renderTable({
      ellipse_fuel()$params
  }, align = 'c', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$e_stat2 <- renderTable({
      ellipse_fuel()$stat
  }, align = 'c', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$e_plot2 <- renderPlotly({ellipse_fuel()$figure})

  output$elec_param_combined <- renderTable({
      if(length(outlier_df_elec()))
      {
        param_stat_combined(figure()$'Elec'$stat_df, result_new_elec()$stat,  mahala_elec()$stat, ellipse_elec()$stat, prev_result_elec()$stat, agg_result_elec()$stat)
      }else
      {
        param_stat_combined(figure()$'Elec'$stat_df, data.frame(),  mahala_elec()$stat, ellipse_elec()$stat, prev_result_elec()$stat, agg_result_elec()$stat)
      }
  }, align = 'c', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$fuel_param_combined <- renderTable({
      if(length(outlier_df_fuel()))
      {
        param_stat_combined(figure()$'Fuel'$stat_df, result_new_fuel()$stat,  mahala_fuel()$stat, ellipse_fuel()$stat, prev_result_fuel()$stat, agg_result_fuel()$stat)
      }else
      {
        param_stat_combined(figure()$'Fuel'$stat_df, data.frame(),  mahala_fuel()$stat, ellipse_fuel()$stat, prev_result_fuel()$stat, agg_result_fuel()$stat)
      }
  }, align = 'c', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$p_e_plot <- renderPlotly({
    prev_result_elec()$figure
    })

  output$p_e_param <- renderTable({
    prev_result_elec()$params
  }, align = 'c', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$p_e_stat <- renderTable({
      prev_result_elec()$stat
  }, align = 'c', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$p_f_plot <- renderPlotly({prev_result_fuel()$figure})

  output$p_f_param <- renderTable({
      prev_result_fuel()$params
  }, align = 'c', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$p_f_stat <- renderTable({
      prev_result_fuel()$stat
  }, align = 'c', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)




  output$a_e_plot <- renderPlotly({
    agg_result_elec()$figure
    })

  output$a_e_param <- renderTable({
    agg_result_elec()$params
  }, align = 'c', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$a_e_stat <- renderTable({
      agg_result_elec()$stat
  }, align = 'c', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$a_f_plot <- renderPlotly({agg_result_fuel()$figure})

  output$a_f_param <- renderTable({
      agg_result_fuel()$params
  }, align = 'c', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$a_f_stat <- renderTable({
      agg_result_fuel()$stat
  }, align = 'c', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

}