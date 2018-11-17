library(shiny)
library(plotly)

server <- function(input, output){
	inter_org_df <- reactive({
	    inFile <- input$interval_file

	    if (is.null(inFile))
	      return(NULL)

	    df = read.csv(inFile$datapath)
	    colnames(df)[colnames(df) == 'Interval End'] <-  'date'
	    df
  	})

  inter_null <- reactive({is.null(inter_org_df())})

  oat_org_df <- reactive({
      inFile <- input$oat_file

      if (is.null(inFile))
        return(NULL)

      df = read.csv(inFile$datapath)
      colnames(df)[colnames(df) == 'Interval End'] <-  'date'
      df
    })

  	meter_vec <- reactive({
  		if(inter_null())
  		{
  			return(NULL)
  		}
  		colnames(inter_org_df())[2:ncol(inter_org_df())]
  	})

  	temp_df <-reactive({
  		prepare_data_func(inter_org_df())
  		})


  	output$meter_id <- renderUI({
	    tagList(
	      selectInput('meter', 'Choose Meter Type', meter_vec(), selected = NULL, multiple = FALSE,selectize = TRUE, width = NULL, size = NULL)
	    )
  	})

    interval_n <- reactive({
      abs(temp_df()$hour_point[2] - temp_df()$hour_point[1])
      })

  	inter_df <- reactive({
  		if(inter_null())
  		{
  			return(NULL)
  		}
  		df = temp_df()[,c('date', input$meter, 'y_m_d', 'year', 'week_number', 'date', 'hour','hour_point','weekday','weekday_flag')]
  		colnames(df)[colnames(df) == input$meter] <-  'demand'
  		df = calc_usage_func(df, interval_n())
  		df$demand = as.numeric(df$demand)
  		df
  	})

    mean_df <- reactive({
      calc_mean_sd_func(inter_df())
      })

    time_vec <- reactive({
      get_time_vec_oat(temp_df())
      })

    oat_df <- reactive({
      prepare_oat_func(oat_org_df(), time_vec())
      })

    agg_oat <- reactive({week_match_func(oat_df(), 'OAT')})

  	output$year_id <- renderUI({
	    tagList(
	      selectInput('year', 'Choose year Type', unique(inter_df()$year), selected = NULL, multiple = FALSE,selectize = TRUE, width = NULL, size = NULL)
	    )
  	})

  	year_df <- reactive({
  		subset(inter_df(), inter_df()$year == input$year)
  		})

  	agg_df <- reactive({week_match_func(inter_df())})

    cout_df <- reactive({
      if(inter_null())
      {
        return(NULL)
      }
      count_func(inter_df(), interval_n())
      })

    weekly_plot_list <- reactive({
      if(is.null(oat_org_df()) | inter_null())
      {
        p_empty = plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = paste("Please upload reqired files."))
        return(list(p_2d = p_empty, p_3d = p_empty, p_color = p_empty))
      }
      main_whole_week_oat_plot(inter_df(), agg_oat())
      })

  	output$hour_week_plot <- renderPlotly({
  		plot_weekly_hourly_average(inter_df(), agg_df())
  		})

  	output$elec_multi_plot <- renderUI({
      if(inter_null())
      {
        output$plot_null <- renderPlotly({
          p_empty = plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = paste("Please upload reqired files."))
          })
        return(tagList(plotlyOutput('plot_null')))
      }

	    plot_output_list <- lapply(1:length(unique(year_df()$week_number)), function(i) {
	      lapply(1:2, function(j){
	      week_i <- unique(year_df()$week_number)[i]

	      if(j == 1)
	      {
	        plotname <- paste("plot", week_i, sep="")
	        output[[plotname]] <- renderPlotly({
	          plot_weekly(year_df(), week_i, interval_n())
	        })
	        plotlyOutput(plotname)
	      }else
	      { 
	        linebreak <- paste("break", week_i, sep="") 
	        output[[linebreak]] <- renderUI({HTML("<br/>")})
	        uiOutput(linebreak)
	      }
	    })
	    }
	    )

	    do.call(tagList, plot_output_list)
  })

  output$timeseries_plot <- renderPlotly({
    if(is.null(oat_org_df()) | inter_null())
    {
        return(plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = paste("Please upload reqired files.")))
    }
    plot_timeseries_agg_func(oat_df(), inter_df(), agg_oat(), agg_df())
    })

  output$avg_weekload_plot <- renderPlotly({
    if(inter_null())
    {
      return(plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = paste("Please upload reqired files.")))
    }
    main_plot_ci(mean_df())
    })

  output$all_weekload_plot_2d <- renderPlotly({
    weekly_plot_list()$p_2d
    })

  output$all_weekload_plot_3d <- renderPlotly({
    weekly_plot_list()$p_3d
    })

  output$color_heat_plot <- renderPlotly({
    weekly_plot_list()$p_color
    })

  output$count_header <- renderText({
    paste('Data Information For Meter:', input$meter)
    })

  output$day_count <- renderTable({
    cout_df()$day_df
    }, align = 'c', rownames = FALSE, colnames = TRUE, width = "auto", digits = NULL)

  output$hour_count <- renderTable({
    cout_df()$hour_df
    }, align = 'c', rownames = FALSE, colnames = TRUE, width = "auto", digits = NULL)

  output$approx_count <- renderTable({
    cout_df()$approx_df
    }, align = 'c', rownames = TRUE, colnames = TRUE, width = "auto", digits = NULL)

  output$data_req_df <- renderTable({
    make_data_req_table()
    }, align = 'l', rownames = FALSE, colnames = TRUE, width = "auto", digits = NULL)

}