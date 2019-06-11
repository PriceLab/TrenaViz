# FimoeDatabaseModelBuilder.R
#------------------------------------------------------------------------------------------------------------------------
#' import shiny
#' @name FimoDatabaseModelBuilder
#' @rdname FimoDatabaseModelBuilder
#' @aliases FimoDatabaseModelBuilder
#------------------------------------------------------------------------------------------------------------------------
# library(TrenaProject)
# library(cyjShiny)
#------------------------------------------------------------------------------------------------------------------------
.FimoDatabaseModelBuilder <- setClass("FimoDatabaseModelBuilder",
                             representation = representation(
                                quiet="logical",
                                state="environment")
                                )
#------------------------------------------------------------------------------------------------------------------------
setGeneric('setMessageX',  signature='obj', function(obj, newValue) standardGeneric('setMessageX'))
#------------------------------------------------------------------------------------------------------------------------
#' Create an FimoDatabaseModelBuilder object
#'
#' @description
#' a shiny app
#'
#' @rdname FimoDatabaseModelBuilder
#'
#' @param message  A character string, one of the supported species names:  hsapiens, mmuscuulus
#'
#' @return An object of the FimoDatabaseModelBuilder class
#'
#' @export
#'
FimoDatabaseModelBuilder <- function(message, quiet=TRUE)
{
   state <- new.env(parent=emptyenv())
   state$message <- message

   .FimoDatabaseModelBuilder(state=state, quiet=quiet)

} # FimoDatabaseModelBuilder
#------------------------------------------------------------------------------------------------------------------------
#' assign a new message to be display
#'
#' @rdname setMessageX
#'
#' @param newValue  A character string
#'
#' @export
#'
setMethod('setMessageX', 'FimoDatabaseModelBuilder',

     function(obj, newValue){
          obj@state$message <- newValue
          })

#------------------------------------------------------------------------------------------------------------------------
setMethod("show", "FimoDatabaseModelBuilder",

    function(object){
        cat(paste("a FimoDatabaseModelBuilder object from the TrenaViz package:", "\n"))
        cat(sprintf("  message: %s\n", obj@state$message))
        })

#------------------------------------------------------------------------------------------------------------------------
#' create and return the control-rich UI
#'
#' @rdname createPage
#' @aliases createPage
#'
#' @param obj An object of class FimoDatabaseModelBuilder
#'
#' @export
#'
setMethod("createPage", "FimoDatabaseModelBuilder",

      function(obj) {
         fluidPage(id="FimoDatabaseModelBuilderPageContent",
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
#' @param obj An object of class FimoDatabaseModelBuilder
#' @param tf  character string, the geneSymbol name of the transcription factor
#'
#' @export
#'
setMethod("displayPage", "FimoDatabaseModelBuilder",

     function(obj){
         printf("FimoDatabaseModelBuilder displayPage")
         removeUI(selector="#FimoDatabaseModelBuilderPageContent", immediate=TRUE)
         insertUI(selector="#FimoDatabaseModelBuilderPage", where="beforeEnd", createPage(obj), immediate=TRUE)
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
#' @param obj An object of class FimoDatabaseModelBuilder
#' @param session a Shiny session object
#' @param input a Shiny input object
#' @param output a Shiny output object
#'
#' @export
#'
setMethod("addEventHandlers", "FimoDatabaseModelBuilder",

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

        observeEvent(input$viewFimoDatabaseModelBuilderButton, ignoreInit=FALSE, {
          printf("view builder ")
          updateTabItems(session, "sidebarMenu", selected="fimoDatabaseModelBuilderTab")
          output$messageDisplayWidget <- renderText(obj@state$message)
          })

     }) # addEventHandlers

#------------------------------------------------------------------------------------------------------------------------
