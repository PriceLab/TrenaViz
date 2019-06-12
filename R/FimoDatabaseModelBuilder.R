# FimoeDatabaseModelBuilder.R
#------------------------------------------------------------------------------------------------------------------------
#' @import shiny
#' @import TrenaProject
#' @name FimoDatabaseModelBuilder
#' @rdname FimoDatabaseModelBuilder
#' @aliases FimoDatabaseModelBuilder
#------------------------------------------------------------------------------------------------------------------------
# library(TrenaProject)
# library(cyjShiny)
#------------------------------------------------------------------------------------------------------------------------
.FimoDatabaseModelBuilder <- setClass("FimoDatabaseModelBuilder",
                             representation = representation(
                                trenaProject="TrenaProject",
                                tbls.regulatoryRegions="list",
                                state="environment",
                                quiet="logical")
                                )
#------------------------------------------------------------------------------------------------------------------------
#' Create an FimoDatabaseModelBuilder object
#'
#' @description
#' a shiny app
#'
#' @rdname FimoDatabaseModelBuilder
#'
#' @param trenaProjectName character eg, "TrenaProjectErythropoiesis"
#' @param targetGene character standard geneSymbol name, eg, "GATA2"
#' @param genomicRegion character, eg "chr3:128,475,725-128,498,247"
#'
#' @return An object of the FimoDatabaseModelBuilder class
#'
#' @export
#'
FimoDatabaseModelBuilder <- function(trenaProject, targetGene, genomicRegion, tbls.regulatoryRegions, quiet=TRUE)
{
   state <- new.env(parent=emptyenv())

   state$targetGene <- targetGene
   state$genomicRegion <- genomicRegion

   .FimoDatabaseModelBuilder(trenaProject=trenaProject,
                             tbls.regulatoryRegions=tbls.regulatoryRegions,
                             state=state,
                             quiet=quiet)

} # FimoDatabaseModelBuilder
#------------------------------------------------------------------------------------------------------------------------
setMethod("show", "FimoDatabaseModelBuilder",

    function(object){
        cat(paste("a FimoDatabaseModelBuilder object from the TrenaViz package:", "\n"))
        cat(sprintf("  trena project: %s\n", getProjectName(obj@trenaProject)))
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
                   includeCSS(system.file(package="TrenaViz", "css", "fimoDatabaseModelBuilder.css")),
                   fluidRow(
                      column(width=7, offset=1, id="fimoModelBuilderTitleBox",
                             h4(id="fimoModeBuilder_title", "Build Gene Regulatory Model"),
                             h4(id="fimoModelBuilder_currentTrenaProject", sprintf("%s", getProjectName(obj@trenaProject))),
                             h4(id="fimoModelBuilder_currentTargetene", sprintf("Target GENE: %s", obj@state$targetGene)),
                             h4(id="fimoModelBuilder_currentGenomicRegion",
                               with(obj@state$genomicRegion, sprintf("In region: %s:%d-%d", chrom, start, end)))
                             )
                   ),
                   fluidRow(
                      column(width=6,
                             selectInput("expressionMatrixSelector", "Expression Matrix",
                                         c("", getExpressionMatrixNames(obj@trenaProject))),
                             selectInput("tfbsTrackSelector", "Restrict TFs to those binding in track: ",
                                         c("", "No restriction: all DNA in current region", names(obj@tbls.regulatoryRegions)))
                             ),
                      column(width=6,
                             sliderInput("fimoThresholdSelector", "FIMO motif match cutoff -log10(pVal)", 1, 10, value=4, step=0.1),
                             sliderInput("tfCorrelationThreshold", "TF/targetGene expression min correlation", 0, 1, value=0.4, step=0.1),
                             sliderInput("modelSizeSelector", "Regulatory model max size", 5, 200, value=10, step=1)
                             )
                      ), # fluidRow
                   fluidRow(
                      column(width=2, offset=0,
                             actionButton("buildModelButton", "Build Regulatory Model")
                             )),
                   fluidRow(
                      column(width=2, offset=0, id="fubar",
                             actionButton("viewNewModelButton", "View")
                             ))

                   ) # fluidPage

      }) # createPage

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
         printf("=== FimoDatabaseModelBuilder displayPage")
         removeUI(selector="#FimoDatabaseModelBuilderPageContent", immediate=TRUE)
         insertUI(selector="#FimoDatabaseModelBuilderPage", where="beforeEnd", createPage(obj), immediate=TRUE)
         later(function() {shinyjs::disable("buildModelButton")
                           shinyjs::disable("viewNewModelButton")}, 3)
         #js$cyjSetupResize();
         #js$cyjShinySetWidth();
         #later(function(){fit(session, 300)}, 1000)
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
