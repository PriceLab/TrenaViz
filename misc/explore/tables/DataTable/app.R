library(shiny)
library(DT)

ui <- fluidPage(
      fluidRow(
         column(10,
                actionButton("displayTableButton", "Display Table, then click on rows",
                             style="margin: 20px"),
                h3(textOutput("rowCountTextOutput"), style="margin-left: 100px"),
                div(DT::dataTableOutput("tableDisplay"), style="margin: 20px")
                )
         ) # fluidRow
    ) # fluidPage

server <- function(input, output){
   observeEvent(input$tableDisplay_rows_selected, {
      selectedTableRow <- isolate(input$tableDisplay_rows_selected)
      output$rowCountTextOutput <- renderText(sprintf("row %d", selectedTableRow))
      printf("row click!");
      printf("selectedTableRow: %d", selectedTableRow)
      }) # observe row selection event

   observeEvent(input$displayTableButton, {
      output$tableDisplay <- DT::renderDataTable(mtcars[1:8,],
                                          width="800px",
                                          class='nowrap display',
                                          selection="single",
                                          extensions="FixedColumns",
                                          options=list(scrollX=TRUE,
                                                       scrollY="500px",
                                                       dom='t',
                                                       paging=FALSE,
                                                       autoWdth=FALSE,
                                                       fixedColumns=list(leftColumns=1)
                                                       ))

      })
   }

app <- shinyApp(ui, server)
