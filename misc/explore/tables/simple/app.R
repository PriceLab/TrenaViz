library(shiny)

ui <- fluidPage(
      fluidRow(
         column(10,
                actionButton("goButton", "Go!"),
                h3(textOutput("myTextOutput")),
                tableOutput("table")
                )
         ) # fluidRow
    ) # fluidPage

server <- function(input, output) {
   observeEvent(input$goButton, {
      chars <- LETTERS[sample(1:26, 6)]
      string <- paste(chars, collapse="")
      printf("string: %s", string)
      output$myTextOutput <- renderText(string)
      output$table <- renderTable(mtcars)
      })
   }

app <- shinyApp(ui, server)
