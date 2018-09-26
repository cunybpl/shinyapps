library(shiny)
library(ggplot2)
library(devtools)
install_github('tinnaing347/bRema')
library(bRema)



#keyword for model -> (A,B,C,D,E) <- (2P, 3PC, 3PH, 4P, 5P)


server <- function(input, output, session) {
  energy_type <- reactive({input$energy})

  util_df <- reactive({
    inFile <- input$file1

    if (is.null(inFile))
      return(read.csv('utility_Library_fy2017_p24.csv'))
    read.csv(inFile$datapath)
  })

  output$first <- renderUI({
    tagList(
      selectInput('bdbid', 'Choose Facility', unique(util_df()$bdbid), selected = NULL, multiple = FALSE,selectize = TRUE, width = NULL, size = NULL)
    )
  })


  b_df <- reactive({
      bdbid = unique(util_df()$bdbid)
      b_df = data.frame(bdbid = bdbid, name = bdbid)
      return(b_df)
  })

  bdbid_n <- reactive({
    b_df()$bdbid[b_df()$name == input$bdbid]
  })

  observeEvent(input$prevBin, {
        current <- which(b_df()$bdbid == bdbid_n())
        if(current > 1){
            updateSelectInput(session, "bdbid",
                              choices = b_df()$name,
                              selected = b_df()$name[current - 1])
        }
        #click_n() = click_n() + 1
  })
    
  observeEvent(input$nextBin, {
        current <- which(b_df()$bdbid  == bdbid_n())
        if(current < length(b_df()$name)){
            updateSelectInput(session, "bdbid",
                              choices = b_df()$name,
                              selected = b_df()$name[current + 1])
        }
        #click_n() = click_n() + 1
  })

  #write logic
  output$range_3pc <- renderUI({
    tagList(
      sliderInput("cp_3pc", "Change-point Range", min = 45, max = 70, value = 45)
    ) 
  })

  output$range_3ph <- renderUI({
    tagList(
      sliderInput("cp_3ph", "Change-point Range", min = 45, max = 70, value = 45)
    ) 
  })

  output$range_4p <- renderUI({
    tagList(
      sliderInput("cp_4p", "Change-point Range", min = 45, max = 70, value = 45)
    ) 
  })

  output$range_5p1 <- renderUI({
    tagList(
      sliderInput("cp_5p1", "Change-point (Left) Range", min = 45, max = 70, value = 45)
    ) 
  })

  output$range_5p2 <- renderUI({
    tagList(
      sliderInput("cp_5p2", "Change-point (Right) Range", min = 45, max = 70, value = 45)
    ) 
  })

  util <- reactive({
    subset(util_df(), util_df()$bdbid == bdbid_n() & util_df()$energy_type == 'Elec')
    })

  best_3PC <- reactive({
    run_ind_model(util(), '3PC', input$cp_3pc, cp2 =0)
  })

  all_result_3pc <- reactive({
    all_table_handler(best_3PC())
    })

  plot_3pc <- reactive({
    main_plot_handler(util(), best_3PC(), 'Elec', source_n = '')
    })

  output$params_3pc <- renderTable({
    all_result_3pc()$params
    }, rownames = FALSE, colnames = TRUE, width = "auto", digits = 4)

  output$stats_3pc <- renderTable({
    all_result_3pc()$stats
    }, rownames = FALSE, colnames = TRUE, width = "auto", digits = 4)

  output$test_3pc <- renderTable({
    all_result_3pc()$stat_test
    }, rownames = FALSE, colnames = TRUE, width = "auto", digits = 4)

  output$model_figure_3pc <- renderPlotly({
    plot_3pc()
  })


  best_3PH <- reactive({
    run_ind_model(util(), '3PH', input$cp_3ph, cp2 =0)
  })

  all_result_3ph <- reactive({
    all_table_handler(best_3PH())
    })

  plot_3ph <- reactive({
    main_plot_handler(util(), best_3PH(), 'Elec', source_n = '')
    })

  output$params_3ph <- renderTable({
    all_result_3ph()$params
    }, rownames = FALSE, colnames = TRUE, width = "auto", digits = 4)

  output$stats_3ph <- renderTable({
    all_result_3ph()$stats
    }, rownames = FALSE, colnames = TRUE, width = "auto", digits = 4)

  output$test_3ph <- renderTable({
    all_result_3ph()$stat_test
    }, rownames = FALSE, colnames = TRUE, width = "auto", digits = 4)

  output$model_figure_3ph <- renderPlotly({
    plot_3ph()
  })



  best_4P <- reactive({
    run_ind_model(util(), '4P', input$cp_4p, cp2 =0)
  })

  all_result_4p <- reactive({
    all_table_handler(best_4P())
    })

  plot_4p <- reactive({
    main_plot_handler(util(), best_4P(), 'Elec', source_n = '')
    })

  output$params_4p <- renderTable({
    all_result_4p()$params
    }, rownames = FALSE, colnames = TRUE, width = "auto", digits = 4)

  output$stats_4p <- renderTable({
    all_result_4p()$stats
    }, rownames = FALSE, colnames = TRUE, width = "auto", digits = 4)

  output$test_4p <- renderTable({
    all_result_4p()$stat_test
    }, rownames = FALSE, colnames = TRUE, width = "auto", digits = 4)

  output$model_figure_4p <- renderPlotly({
    plot_4p()
  })



  best_5P <- reactive({
    run_ind_model(util(), '5P', input$cp_5p1, input$cp_5p2)
  })

  all_result_5p <- reactive({
    all_table_handler(best_5P())
    })

  plot_5p <- reactive({
    main_plot_handler(util(), best_5P(), 'Elec', source_n = '')
    })

  output$params_5p <- renderTable({
    all_result_5p()$params
    }, rownames = FALSE, colnames = TRUE, width = "auto", digits = 4)

  output$stats_5p <- renderTable({
    all_result_5p()$stats
    }, rownames = FALSE, colnames = TRUE, width = "auto", digits = 4)

  output$test_5p <- renderTable({
    all_result_5p()$stat_test
    }, rownames = FALSE, colnames = TRUE, width = "auto", digits = 4)

  output$model_figure_5p <- renderPlotly({
    plot_5p()
  })


  best_2P <- reactive({
    run_ind_model(util(), '2P', cp1 = 0, cp2 =0)
  })

  all_result_2p <- reactive({
    all_table_handler(best_2P())
    })

  plot_2p <- reactive({
    main_plot_handler(util(), best_2P(), 'Elec', source_n = '')
    })

  output$params_2p <- renderTable({
    all_result_2p()$params
    }, rownames = FALSE, colnames = TRUE, width = "auto", digits = 4)

  output$stats_2p <- renderTable({
    all_result_2p()$stats
    }, rownames = FALSE, colnames = TRUE, width = "auto", digits = 4)

  output$test_2p <- renderTable({
    all_result_2p()$stat_test
    }, rownames = FALSE, colnames = TRUE, width = "auto", digits = 4)

  output$model_figure_2p <- renderPlotly({
    plot_2p()
  })

  output$range_3pc2 <- renderUI({
    tagList(
      sliderInput("cp_3pc2", "Change-point Range", min = 45, max = 70, value = 45)
    ) 
  })

  output$range_3ph2 <- renderUI({
    tagList(
      sliderInput("cp_3ph2", "Change-point Range", min = 45, max = 70, value = 45)
    ) 
  })

  output$range_4p2 <- renderUI({
    tagList(
      sliderInput("cp_4p2", "Change-point Range", min = 45, max = 70, value = 45)
    ) 
  })

  output$range_5p12 <- renderUI({
    tagList(
      sliderInput("cp_5p12", "Change-point (Left) Range", min = 45, max = 70, value = 45)
    ) 
  })

  output$range_5p22 <- renderUI({
    tagList(
      sliderInput("cp_5p22", "Change-point (Right) Range", min = 45, max = 70, value = 45)
    ) 
  }) 


  util2 <- reactive({
    subset(util_df(), util_df()$bdbid == bdbid_n() & util_df()$energy_type == 'Fuel')
    })

  best_3PC2 <- reactive({
    run_ind_model(util2(), '3PC', input$cp_3pc2, cp2 =0)
  })

  all_result_3pc2 <- reactive({
    all_table_handler(best_3PC2())
    })

  plot_3pc2 <- reactive({
    main_plot_handler(util2(), best_3PC2(), 'Fuel', source_n = '')
    })

  output$params_3pc2 <- renderTable({
    all_result_3pc2()$params
    }, rownames = FALSE, colnames = TRUE, width = "auto", digits = 4)

  output$stats_3pc2 <- renderTable({
    all_result_3pc2()$stats
    }, rownames = FALSE, colnames = TRUE, width = "auto", digits = 4)

  output$test_3pc2 <- renderTable({
    all_result_3pc2()$stat_test
    }, rownames = FALSE, colnames = TRUE, width = "auto", digits = 4)

  output$model_figure_3pc2 <- renderPlotly({
    plot_3pc2()
  })


  best_3PH2 <- reactive({
    run_ind_model(util2(), '3PH', input$cp_3ph2, cp2 =0)
  })

  all_result_3ph2 <- reactive({
    all_table_handler(best_3PH2())
    })

  plot_3ph2 <- reactive({
    main_plot_handler(util2(), best_3PH2(), 'Fuel', source_n = '')
    })

  output$params_3ph2 <- renderTable({
    all_result_3ph2()$params
    }, rownames = FALSE, colnames = TRUE, width = "auto", digits = 4)

  output$stats_3ph2 <- renderTable({
    all_result_3ph2()$stats
    }, rownames = FALSE, colnames = TRUE, width = "auto", digits = 4)

  output$test_3ph2 <- renderTable({
    all_result_3ph2()$stat_test
    }, rownames = FALSE, colnames = TRUE, width = "auto", digits = 4)

  output$model_figure_3ph2 <- renderPlotly({
    plot_3ph2()
  })



  best_4P2 <- reactive({
    run_ind_model(util2(), '4P', input$cp_4p2, cp2 =0)
  })

  all_result_4p2 <- reactive({
    all_table_handler(best_4P2())
    })

  plot_4p2 <- reactive({
    main_plot_handler(util2(), best_4P2(), 'Fuel', source_n = '')
    })

  output$params_4p2 <- renderTable({
    all_result_4p2()$params
    }, rownames = FALSE, colnames = TRUE, width = "auto", digits = 4)

  output$stats_4p2 <- renderTable({
    all_result_4p2()$stats
    }, rownames = FALSE, colnames = TRUE, width = "auto", digits = 4)

  output$test_4p2 <- renderTable({
    all_result_4p2()$stat_test
    }, rownames = FALSE, colnames = TRUE, width = "auto", digits = 4)

  output$model_figure_4p2 <- renderPlotly({
    plot_4p2()
  })



  best_5P2 <- reactive({
    run_ind_model(util(), '5P', input$cp_5p12, input$cp_5p22)
  })

  all_result_5p2 <- reactive({
    all_table_handler(best_5P2())
    })

  plot_5p2 <- reactive({
    main_plot_handler(util2(), best_5P2(), 'Fuel', source_n = '')
    })

  output$params_5p2 <- renderTable({
    all_result_5p2()$params
    }, rownames = FALSE, colnames = TRUE, width = "auto", digits = 4)

  output$stats_5p2 <- renderTable({
    all_result_5p2()$stats
    }, rownames = FALSE, colnames = TRUE, width = "auto", digits = 4)

  output$test_5p2 <- renderTable({
    all_result_5p2()$stat_test
    }, rownames = FALSE, colnames = TRUE, width = "auto", digits = 4)

  output$model_figure_5p2 <- renderPlotly({
    plot_5p2()
  })


  best_2P2 <- reactive({
    run_ind_model(util2(), '2P', cp1 = 0, cp2 =0)
  })

  all_result_2p2 <- reactive({
    all_table_handler(best_2P2())
    })

  plot_2p2 <- reactive({
    main_plot_handler(util2(), best_2P2(), 'Fuel', source_n = '')
    })

  output$params_2p2 <- renderTable({
    all_result_2p2()$params
    }, rownames = FALSE, colnames = TRUE, width = "auto", digits = 4)

  output$stats_2p2 <- renderTable({
    all_result_2p2()$stats
    }, rownames = FALSE, colnames = TRUE, width = "auto", digits = 4)

  output$test_2p2 <- renderTable({
    all_result_2p2()$stat_test
    }, rownames = FALSE, colnames = TRUE, width = "auto", digits = 4)

  output$model_figure_2p2 <- renderPlotly({
    plot_2p2()
  })


}