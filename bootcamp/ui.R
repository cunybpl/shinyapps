library(shiny)
ui <- fluidPage(
	br(),
	tags$img(src = "gip.gif", width = "1000px", height = "562px"),
	br(),
	tags$audio(src = "song.mp3", type = "audio/mp3", controls = NA)
)
