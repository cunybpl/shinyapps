library(shiny)
library(DT)

options(shiny.maxRequestSize=30*1024^2) 
server <- function(input, output, session) {

  temp_df <- reactive({
    inFile2 <- input$file2

    if (is.null(inFile2))
      return(NULL)

    read.csv(inFile2$datapath)
    })

  oat_df <- reactive({
    inFile3 <- input$file3

    if (is.null(inFile3))
    {
      return(NULL) 
    }

    return(read.csv(inFile3$datapath))
    })

  utility_df <- eventReactive(input$run, {
    df = main_utility_handler(temp_df(), oat_df())
    showNotification("Finished generating utility file.", duration = 60, type = 'message')
    return(df)})

  output$util_table <- DT::renderDataTable({
    DT::datatable(data.table::setDT(utility_df()))
  })
}