library(shiny)

server <- function(input, output) {
	output$message <- renderText({
    'Please reload if this message is showing up.'
  })
}