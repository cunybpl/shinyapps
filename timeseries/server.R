library(shiny)
library(plotly)

options(shiny.maxRequestSize=30*1024^2) 

server <- function(input, output, session) {

  temp_df <- reactive({
    inFile2 <- input$file2

    if (is.null(inFile2))
      return(NULL)

    read.csv(inFile2$datapath)
    })

  col_names <- reactive({
    colnames(temp_df())
    })

  output$col_second <- renderUI({
      tagList(
        selectInput('y_cols_2_vec', 'Choose Secondary Columns', col_names(), selected = NULL, multiple = TRUE,selectize = TRUE, width = NULL, size = NULL)
      )
  })

  final_fig <- reactive({
  	main_handler(temp_df(), input$y_cols_2_vec, input$plot_title, input$y_title, input$y_title_2, interval = input$interval, interval_type = input$interval_type)
  	})

  date_range <- reactive({
    result = datetime_col_handler(input$daterange1, input$start_time, input$end_time)
    result
    })

  figure <- reactive({
    main_handler(temp_df(), input$y_cols_2_vec, input$plot_title, input$y_title, input$y_title_2, date_range(), interval = input$interval, interval_type = input$interval_type)
    })

  output$plot1 <- renderPlotly({final_fig()})
  output$plot2 <- renderPlotly({figure()})
}






