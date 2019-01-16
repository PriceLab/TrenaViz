library(shiny)
library(TrenaViz)
# project <- TrenaProjectAD()
#----------------------------------------------------------------------------------------------------
state <- new.env(parent=emptyenv())
#----------------------------------------------------------------------------------------------------
ui <- basicPage(

  tags$head(tags$style(HTML('
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

  observeEvent(input$displayMotifButton, ignoreInit=TRUE, {
     output$motifRenderingPanel <- renderPlot({
        renderLogos(state$bindingSitesManager)
        })
     })


   observeEvent(input$tfSelector, ignoreInit=TRUE, {
      tf <- input$tfSelector
      if(nchar(tf) == 0) return();
      printf("tf: %s",   tf)
      bsm <- BindingSitesManager(tf, "Hsapiens", "hg38") # , input, output)
      state$bindingSitesManager <- bsm
      dialog <- createDialog(bsm)
      showModal(dialog)
     })

} # server
#----------------------------------------------------------------------------------------------------
runApp(shinyApp(ui, server))
