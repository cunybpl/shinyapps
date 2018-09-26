library(plotly)
library(shiny)
library(rdrop2)

ui <- fluidPage(
  headerPanel('Survey for 3PC and 4P models'),
  sidebarPanel(
    textInput("user_name", "Name", ""),
    textInput("misc_note", "Note (optional)", ""),
    actionButton("submit", "Submit")
  ),
  mainPanel(
    tabsetPanel(
        tabPanel("Main",
            br(),
            h4('AFTER SUBMITTING, DO NOT CLOSE THE WINDOW UNTIL SUBMISSION NOTIFICATION SHOWS UP AT THE BOTTOM RIGHT CONNER OF THE DISPLAY.', style = "color:red"),
            #h4("PLEASE DO NOT CLICK ANYTHING ON THIS PAGE WHEN SHINY IS LOADING DROPDOWN MENUS."),
            h5("There are 44 facilities. Thus, please finish it in one sitting.", style = "color:red"),
            h5("Also do not let the app idle. Otherwise you will have to start all over again.", style = "color:red"),
            tags$ul(
                tags$li("Fill out your first name after the graphs are displayed."),
                tags$li("Choose between 3PC and 4P for all facilities and press 'submit'."),

                tags$li("Please submit only once. However, if you are submitting again, please fill out the note to let the author know. For example, 'second submission'."),
                tags$li('The legends in the graph are clickable.')
            ),
            br(),
            h3("Choose between 3PC and 4P", align = "center"),
            uiOutput('plots')
        )
    )
  )
)