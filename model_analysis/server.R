library(plotly)
library(shiny)
library(rdrop2)

server <- function(input, output, session){

	dd_df <- reactive({
	    withProgress(message = 'Getting data', style = 'notification', value = 0.25, {
	    setProgress(0.5)
	    temp = get_dropdown_info_2()
	    setProgress(1)
	    })
	    temp
  	})

  dd_path <-reactive({
    paste(dd_df()$child_path, '/period24/2017',sep ='')
  })

  category <- reactive({'FDNY'})
  year <- reactive({2017})
  period <- reactive({24})

	modeller <- reactive({
	    #path= subset(fy_df()$path, fy_df()$fy0 == input$year)
	    fileName=paste("modeller_",category(),"_fy",year(),"_p",period(),".csv",sep="")
	    df = tryCatch(drop_read_csv(paste(dd_path(),fileName,sep="/")), error = function(e){return(NULL)})
	    return(df)
	})


	temp_df <- reactive({
	    #path=subset(fy_df()$path, fy_df()$fy0 == input$year)
	    fileName=paste("utility_",category(),"_fy",year(),"_p",period(),".csv",sep="")
	    df = tryCatch(drop_read_csv(paste(dd_path(),fileName,sep="/")), error = function(e){return(NULL)})
	    return(df)
    })

    binfo_df <- reactive({
	    #path=subset(fy_df()$path, fy_df()$fy0 == input$year)
	    fileName=paste("buildings_info_",category(),"_fy",year(),"_p",period(),".csv",sep="")
	    df = tryCatch(drop_read_csv(paste(dd_path(),fileName,sep="/")), error = function(e){return(NULL)})
	    return(df)
    })

    fields <- reactive({
    	create_response_id(unique(modeller()$bdbid))
    	})

  output$plots <- renderUI({
    plot_output_list <- lapply(1:length(unique(modeller()$bdbid)), function(i) {
      lapply(1:3, function(j){
      bdbid_n <- unique(modeller()$bdbid)[i]

      if(j == 1)
      {
        plotname <- paste("plot", bdbid_n, sep="")
        output[[plotname]] <- renderPlotly({
          b_name = get_building_name(binfo_df(), bdbid_n)
          main_plot(temp_df(), modeller(), best_model = NULL, bdbid_n, 'Elec', b_name)
        })
        plotlyOutput(plotname)
      }else if(j == 2)
      { res_in <- paste("res_in", bdbid_n, sep="")
        res_out <- paste("res_out", bdbid_n, sep="")
        output[[res_out]] <- renderUI({
          tagList(
				radioButtons(inputId = res_in, label = 'Choose Models', selected = 'NULL', inline = TRUE, width = NULL, choiceNames = c('NULL','3PC','4P'), choiceValues = c('NULL','3PC','4P'))
			)
          })
        uiOutput(res_out)
      }else
      { 
        linebreak <- paste("break", bdbid_n, sep="") 
        output[[linebreak]] <- renderUI({HTML("<hr/>")})
        uiOutput(linebreak)
      }
    })
    }
    )

    do.call(tagList, plot_output_list)
  })

	formData <- reactive({
      data <- sapply(fields()
            , function(x) input[[x]])
      data
    })
    
    # When the Submit button is clicked, save the form data
    observeEvent(input$submit, {
      saveData(formData(), unique(modeller()$bdbid))
      temp = loadData()
      saveData2(temp, unique(modeller()$bdbid))
      showNotification("Submitted Sucessfully", duration = 7, type = 'message')
    })



}