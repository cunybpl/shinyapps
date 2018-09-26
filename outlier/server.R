library(shiny)
library(plotly)
library(devtools)
install_github('tinnaing347/bRema')
library(bRema)

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

  figure <- reactive({
  	require(ggplot2)
  	temp_list = list()
  	for (energy_n in c('Elec', 'Fuel')){

        source_n = ifelse(energy_n == 'Elec', 'E', 'F')
        #p_empty = plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = paste('Only one energy type for', bdbid_n()))
        model_fig = main_plot(temp_df(), best_model(), bdbid_n(), energy_n, source_n = source_n)
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
    utility = subset(temp_df(), temp_df()$bdbid == bdbid_n() & temp_df()$energy_type == 'Elec')
    new_util = outlier_free(utility, outlier_df_elec()) #use this for plotting
    inter_result = run_outlier_model(new_util)
    final_result = choose_best(inter_result, 'Elec')
    temp_func_elec(data.frame())
    figure = out_main_plot(final_result, new_util, 'Elec')
    stat = stat_table_out(final_result$stats, final_result$stat_test$pop_test, final_result$model, nrow(new_util))
    params = params_table_out(final_result$model, final_result$params, final_result$cp1, final_result$cp2)
    list(stat = stat, params = params, figure = figure)
  })


  result_new_fuel <- eventReactive(input$graph_fuel,{
    utility = subset(temp_df(), temp_df()$bdbid == bdbid_n() & temp_df()$energy_type == 'Fuel')
    new_util = outlier_free(utility, outlier_df_fuel()) #use this for plotting
    inter_result = run_outlier_model(new_util)
    final_result = choose_best(inter_result, 'Fuel')
    temp_func_fuel(data.frame())
    figure = out_main_plot(final_result, new_util, 'Fuel')
    list(figure = figure)
    stat = stat_table_out(final_result$stats, final_result$stat_test$pop_test, final_result$model, nrow(new_util))
    params = params_table_out(final_result$model, final_result$params, final_result$cp1, final_result$cp2)
    list(stat = stat, params = params, figure = figure)
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

}