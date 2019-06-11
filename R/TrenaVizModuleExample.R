# TrenaVizModuleExample.R
#------------------------------------------------------------------------------------------------------------------------
#' import shiny
#' @name ModuleExample
#' @rdname ModuleExample
#' @aliases ModuleExample
#------------------------------------------------------------------------------------------------------------------------
# library(TrenaProject)
# library(cyjShiny)
#------------------------------------------------------------------------------------------------------------------------
.ModuleExample <- setClass("ModuleExample",
                             representation = representation(
                                quiet="logical",
                                state="environment")
                                )
#------------------------------------------------------------------------------------------------------------------------
setGeneric('setMessage',  signature='obj', function(obj, newValue) standardGeneric('setMessage'))
#------------------------------------------------------------------------------------------------------------------------
#' Create an ModuleExample object
#'
#' @description
#' a shiny app
#'
#' @rdname ModuleExample
#'
#' @param message  A character string, one of the supported species names:  hsapiens, mmuscuulus
#'
#' @return An object of the ModuleExample class
#'
#' @export
#'
ModuleExample <- function(message, quiet=TRUE)
{
   state <- new.env(parent=emptyenv())
   state$message <- message

   .ModuleExample(state=state,
                  quiet=quiet)

} # ModuleExample
#------------------------------------------------------------------------------------------------------------------------
#' assign a new message to be display
#'
#' @rdname setMessage
#'
#' @param newValue  A character string
#'
#' @export
#'
setMethod('setMessage', 'ModuleExample',
          function(obj, newValue){
             obj@state$message <- newValue
          })
#------------------------------------------------------------------------------------------------------------------------
setMethod("show", "ModuleExample",

    function(object){
        cat(paste("a ModuleExample object from the TrenaViz package:", "\n"))
        cat(sprintf("  message: %s\n", obj@state$message))
        })

#------------------------------------------------------------------------------------------------------------------------
#' create and return the control-rich UI
#'
#' @rdname createPage
#' @aliases createPage
#'
#' @param obj An object of class ModuleExample
#'
#' @export
#'
setMethod("createPage", "ModuleExample",

      function(obj) {
         fluidPage(id="ModuleExamplePageContent",
          fluidRow(
             actionButton(inputId="hideMessageButton", label="Hide"),
             actionButton(inputId="showMessageButton", label="Show")
             ),
          fluidRow(column(width=12, htmlOutput("messageDisplayWidget")))
          )
       })

#------------------------------------------------------------------------------------------------------------------------
#' display the page
#'
#' @rdname displayPage
#' @aliases displayPage
#'
#' @param obj An object of class ModuleExample
#' @param tf  character string, the geneSymbol name of the transcription factor
#'
#' @export
#'
setMethod("displayPage", "ModuleExample",

     function(obj){
         printf("ModuleExample displayPage")
         removeUI(selector="#ModuleExamplePageContent", immediate=TRUE)
         insertUI(selector="#ModuleExamplePage", where="beforeEnd", createPage(obj), immediate=TRUE)
         #js$cyjSetupResize();
         js$cyjShinySetWidth();
         later(function(){fit(session, 300)}, 1000)
         })

#------------------------------------------------------------------------------------------------------------------------
#' add shiny event handlers
#'
#' @rdname addEventHandlers
#' @aliases addEventHandlers
#'
#' @param obj An object of class ModuleExample
#' @param session a Shiny session object
#' @param input a Shiny input object
#' @param output a Shiny output object
#'
#' @export
#'
setMethod("addEventHandlers", "ModuleExample",

     function(obj, session, input, output){

        obj@state$session <- session
        obj@state$input <- input
        obj@state$output <- output

        observeEvent(input$messageSelector, ignoreInit=TRUE, {
           print("message selector changed")
           obj@state$message <- input$messageSelector
           })

        observeEvent(input$showMessageButton, ignoreInit=TRUE, {
           shinyjs::show("messageDisplayWidget")
           print("show message")
           })

        observeEvent(input$hideMessageButton, ignoreInit=TRUE, {
           printf("hide message")
           shinyjs::hide("messageDisplayWidget")
           })

        observeEvent(input$viewModuleExampleButton, ignoreInit=FALSE, {
          printf("view module example")
          updateTabItems(session, "sidebarMenu", selected="moduleExampleTab")
          output$messageDisplayWidget <- renderText(obj@state$message)
          })

     }) # addEventHandlers

#------------------------------------------------------------------------------------------------------------------------
