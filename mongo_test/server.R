library(shiny)
library(mongolite)

server <- function(input, output){
	 
  mongo_url <- reactive({
    mongo_info = get_key()
    paste("mongodb+srv://", mongo_info$usr, ":", mongo_info$pwd, "@cluster0-mrfjh.mongodb.net/test?retryWrites=true", sep = "")
    })


  df <- reactive({
      mongo<- mongo(collection = "best_model", db = "bema_db", url = mongo_url(), verbose = TRUE)
      mongo$find("{}")[1:5,1:5]
    })

  output$blah <- renderText({mongo_url()})

  output$bema_out <- renderTable({
    df()
    }, align = 'l', rownames = FALSE, colnames = TRUE, width = "auto", digits = NULL)

}