library(shiny)
library(plotly)
library(devtools)
install_github('tinnaing347/bRema')
library(bRema)


ui <- fluidPage(
	headerPanel('Unretrofit Graph'),
	sidebarPanel(
		fileInput("file2", "Choose Utility CSV File", accept = c("text/csv","text/comma-separated-values,text/plain", ".csv")),
    	fileInput("file1", "Choose Best Model CSV/All Model CSV File", accept = c("text/csv","text/comma-separated-values,text/plain", ".csv")),
    	fileInput("file7", "Choose Building Info CSV File", accept = c("text/csv","text/comma-separated-values,text/plain", ".csv")),
    	uiOutput("first")
	),
	mainPanel(
		tabsetPanel(
			tabPanel('Elec',
				br(),
				plotlyOutput('plot2'),
            	br(),
            	actionButton(inputId = "graph_elec", label = "Run"),
            	verbatimTextOutput("outlier_elec"),
            	br(),
            	tableOutput('params_df'),
            	br(),
            	tableOutput('stat_df'),
            	br(),
            	plotlyOutput('plot_out'),
                br(),
                tableOutput('params_df_out'),
                br(),
                tableOutput('stat_df_out')
			),
			tabPanel('Fuel',
				br(),
				plotlyOutput('plot22'),
            	br(),
                actionButton(inputId = "graph_fuel", label = "Run"),
                verbatimTextOutput("outlier_fuel"),
                br(),
            	tableOutput('params_df2'),
            	br(),
            	tableOutput('stat_df2'),
            	br(),
                plotlyOutput('plot_out2'),
                br(),
                tableOutput('params_df_out2'),
                br(),
                tableOutput('stat_df_out2')
			)
		)
	)
)