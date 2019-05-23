library(shiny)
library(TrenaViz)
#----------------------------------------------------------------------------------------------------
state <- new.env(parent=emptyenv())
#----------------------------------------------------------------------------------------------------
ui <- basicPage(

  shiny::tags$head(shiny::tags$style(HTML('
       .modal-body, .modal-content, .modal-dialog{
          width: 1200px;
          height: 1000px;
          }'))),

   actionButton(inputId="dumbPlotButton", label = "Plot"),
   h5("Choose TF:"),
   selectInput("tfSelector", NULL,  c("", "HES7", "LYL1"))

)
#----------------------------------------------------------------------------------------------------
server <- function(input, output) {

   observeEvent(input$dumbPlotButton, ignoreInit=TRUE, {
      })


   observeEvent(input$dumbPlotButton, ignoreInit=TRUE, {
      load("gata2.model.RData")
      networkView <- NetworkView("GATA2", tbl.model, tbl.regions, mtx)
      state$networkView <- networkView
      createPage(networkView)
      displayPage(networkView)
      })

} # server
#----------------------------------------------------------------------------------------------------
runApp(shinyApp(ui, server))
