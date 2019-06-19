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
                                state="environment",
                                quiet="logical"
                                )
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
ModuleExample <- function(quiet=TRUE)
{
   state <- new.env(parent=emptyenv())
   state$trenaProject <= NULL
   state$targetGene <- NULL
   state$currentGenomicRegion <- NULL

   .ModuleExample(state=state, quiet=quiet)

} # ModuleExample
#------------------------------------------------------------------------------------------------------------------------
#' supply tissue-specific data
#'
#' @rdname setTrenaProject
#' @aliases setTrenaProject
#'
#' @param obj An object of class FimoModelWidget
#' @param trenaProject an instance of a concrete subclass of TrenaProject
#'
#' @export

setMethod("setTrenaProject", "ModuleExample",

    function(obj, trenaProject){
        obj@state$trenaProject <- trenaProject
        })

#------------------------------------------------------------------------------------------------------------------------
#' supply tissue-specific data
#'
#' @rdname setTrenaProject
#' @aliases setTrenaProject
#'
#' @param obj An object of class FimoModelWidget
#' @param trenaProject an instance of a concrete subclass of TrenaProject
#'
#' @export

setMethod("setTrenaProject", "ModuleExample",

    function(obj, trenaProject){
        obj@state$trenaProject <- trenaProject
        })

#------------------------------------------------------------------------------------------------------------------------
#' set the chromosomal locus of current interestg
#'
#' @rdname setGenomicRegion
#' @aliases setGenomciRegion
#'
#' @param obj An object of class FimoModelWidget
#' @param newRegion a character string
#'
#' @export

setMethod("setGenomicRegion", "ModuleExample",

    function(obj, tbl.region){
       regionString <- with(tbl.region, sprintf("%s:%d-%d", chrom, start, end))
       printf("new genomicRegion: %s", regionString)
       if(!is.null(js$setModuleExampleGenomicRegionDisplay))  # not initialized on very early calls
           js$setModuleExampleGenomicRegionDisplay(regionString)

       })

#------------------------------------------------------------------------------------------------------------------------
setMethod("show", "ModuleExample",

    function(object){
        cat(paste("a ModuleExample object from the TrenaViz package:", "\n"))
        })

#------------------------------------------------------------------------------------------------------------------------
setGeneric('.moduleExampleCreatePage',  signature='obj', function(obj) standardGeneric('.moduleExampleCreatePage'))
#' create and return the control-rich UI
#'
#' @rdname createPage
#' @aliases createPage
#'
#' @param obj An object of class ModuleExample
#'
#### export
#'
setMethod(".moduleExampleCreatePage", "ModuleExample",

      function(obj) {
         fluidPage(id="ModuleExamplePageContent",
            extendShinyjs(script=system.file(package="TrenaViz", "js", "moduleExample.js")),
            fluidRow(
               actionButton(inputId="hideMessageButton", label="Hide"),
               actionButton(inputId="showMessageButton", label="Show")
               ),
            fluidRow(column(width=8, id="messageDisplayWidget",
                       h4("hello ModuleExample"),
                       h4(getProjectName(obj@state$trenaProject)),
                       h4(sprintf("targetGene: %s", getTargetGene(obj@state$trenaProject))),
                       h4("goodbye ModuleExample")
                       )),
            fluidRow(column(width=3,
                       h4(id="moduleExample_genomicRegionWidget", "undefined")))
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
         insertUI(selector="#ModuleExamplePage", where="beforeEnd",
                  .moduleExampleCreatePage(obj), immediate=TRUE)
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
          displayPage(obj)
          output$messageDisplayWidget <- renderText(obj@state$message)
          })

     }) # addEventHandlers

#------------------------------------------------------------------------------------------------------------------------
