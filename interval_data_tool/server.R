library(shiny)
library(plotly)


server <- function(input, output) {
  inter_df <- reactive({
    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)

    read.csv(inFile$datapath)
  })

  meter_name <- reactive({
    if (is.null(inter_df())){
      c('Please Upload Interval Data File')
    }else{
      colnames(inter_df())[2:(ncol(inter_df()))]
    }
  })

  output$month_id <- renderUI({
    tagList(
      selectInput('month', 'Choose Month', c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'), selected = NULL, multiple = FALSE,selectize = TRUE, width = NULL, size = NULL)
    )
  })


  output$meter_id <- renderUI({
    tagList(
      selectInput('meter', 'Choose Meter Type', meter_name(), selected = NULL, multiple = FALSE,selectize = TRUE, width = NULL, size = NULL)
    )
  })


  figure <- eventReactive(input$click,{
    require(plotly)
    df = fixing_time(inter_df())
    inter_meter = interval_main(df, as.character(input$meter))
    month_n = get_month(input$month)
    w = interval_weekly(inter_meter, month_n)
    m = interval_monthly(inter_meter, month_n)
    heat_df = clean_data_heatmap(inter_df())
    average_df = get_monthly_time_averages(inter_df())

    list(w1 = w$w1, w2 = w$w2, w3 = w$w3, w4 = w$w4, month = m, heat_figure = heatmap(heat_df), average_fig = figure_average(average_df))
  })
  

  output$plot1 <- renderPlotly({figure()$w1})
  output$plot2 <- renderPlotly({figure()$w2})
  output$plot3 <- renderPlotly({figure()$w3})
  output$plot4 <- renderPlotly({figure()$w4})
  output$plot5 <- renderPlotly({figure()$month})
  output$plot6 <- renderPlotly({figure()$heat_figure})
  output$plot7 <- renderPlotly({figure()$average_fig})
}