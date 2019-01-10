library(shiny)
library(plotly)
library(lubridate)

#if(!("bRema" %in% rownames(installed.packages())))
#{ 
#  library(devtools)
#  install_github('tinnaing347/bRema')
#}

library(devtools)
#install_github('cunybpl/bRema')
library(bRema)


server <- function(input, output, session) {
  energy_type <- reactive({input$energy})

  util_df <- reactive({
    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath)
  })


  bdbid_n <- reactive({
    unique(util_df()$bdbid)
  })

  output$first <- renderUI({
    tagList(
      selectInput('bdbid', 'Choose Facility', bdbid_n(), selected = NULL, multiple = FALSE,selectize = TRUE, width = NULL, size = NULL)
    )
  })

  energy <- reactive({
    util_df()$energy_type[util_df()$bdbid == input$bdbid]
  })

  output$second <- renderUI({
    tagList(
      selectInput('energy', 'Choose energy type', energy(), selected = NULL, multiple = FALSE,selectize = TRUE, width = NULL, size = NULL)
    )
  })


  observeEvent(input$prevBin, {
        current <- which(bdbid_n() == input$bdbid)
        if(current > 1){
            updateSelectInput(session, "bdbid",
                              choices = bdbid_n(),
                              selected = bdbid_n()[current - 1])
        }
        #click_n() = click_n() + 1
  })
    
  observeEvent(input$nextBin, {
        current <- which(bdbid_n() == input$bdbid)
        if(current < length(bdbid_n())){
            updateSelectInput(session, "bdbid",
                              choices = bdbid_n(),
                              selected = bdbid_n()[current + 1])
        }
        #click_n() = click_n() + 1
  })

  best_df <- eventReactive(input$run, {
    df = batch_run(util_df())$best_result_df
    showNotification("Finished running modeller", duration = 60, type = 'message')
    return(df)
    })

  observe({best_df()$model_type[1]})

  post_df <- reactive({
    main_post_model(util_df(), best_df(), input$lean_flag)
    })

  observe({
    post_df()$model_type[1]
    showNotification("Finished running post modeller", duration = 60, type = 'message')
    })

  output$best_download <- downloadHandler(
    filename = function() {
      paste(input$best_name, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(best_df(), file, row.names = FALSE)
    }
  )

  output$post_download <- downloadHandler(
    filename = function() {
      paste(input$post_name, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(post_df(), file, row.names = FALSE)
    }
  )

  result <- reactive({ 

    util = subset(util_df(), util_df()$bdbid == input$bdbid & util_df()$energy_type == input$energy)
    if (input$step)
    {
      result = run_model(util, plot_flag = TRUE, step = c(10,2,0.5))
    }else
    {
      result = run_model(util, plot_flag = TRUE)
    }
    list(result = result, util = util)
    })

  figure_list <- reactive({
    model = input$model
    model_figure = result()$result[[model]]$figure
    params = result_table(result()$result, model)
    stats = result()$result[[model]]$stats
    list(model_figure = model_figure, params = params, stats = stats)
  })

  output$plot3 <- renderPlotly({
      plot_timeseries_2(result()$util, input$energy, input$sqft)
  })

  output$params <- renderTable({
    figure_list()$params
    }, rownames = TRUE, colnames = FALSE, width = "auto", digits = -7)
  output$stats <- renderTable({
    figure_list()$stats
    }, rownames = TRUE, colnames = FALSE, width = "auto", digits = -7)
  output$model_figure <- renderPlotly({
    figure_list()$model_figure
  })

}