#' import shiny
#' @name GeneGeneExpressionPlotter
#' @rdname GeneGeneExpressionPlotter
#' @aliases GeneGeneExpressionPlotter
#------------------------------------------------------------------------------------------------------------------------
.GeneGeneExpressionPlotter <- setClass ("GeneGeneExpressionPlotter",
                                  representation = representation(
                                     quiet="logical",
                                     state="environment")
                                  )
#------------------------------------------------------------------------------------------------------------------------
#setGeneric("addEventHandlers",    signature="obj", function(obj, session, input, output) standardGeneric("addEventHandlers"))
setGeneric("setGenes",            signature="obj", function(obj, gene1, gene2) standardGeneric("setGenes"))
setGeneric("setMatrixForPlotting",           signature="obj", function(obj, matrix) standardGeneric("setMatrixForPlotting"))
# setGeneric("createPage",          signature="obj", function(obj) standardGeneric("createPage"))
# setGeneric("displayPage",         signature="obj", function(obj, tf) standardGeneric("displayPage"))
#------------------------------------------------------------------------------------------------------------------------
#' Create an GeneGeneExpressionPlotter object
#'
#' @description
#' a shiny app
#'
#' @rdname GeneGeneExpressionPlotter
#'
#' @param quiet A logical indicating whether or not the Trena object should print output
#'
#' @return An object of the GeneGeneExpressionPlotter class
#'
#' @export
#'
GeneGeneExpressionPlotter <- function(quiet=TRUE)
{
   state <- new.env(parent=emptyenv())
   state$TF <- NULL
   state$organism <- NULL
   state$genome <- NULL
   state$gene1 <- NULL
   state$gene2 <- NULL
   state$matrix <- NULL

   .GeneGeneExpressionPlotter(state=state,  quiet=quiet)

} # GeneGeneExpressionPlotter
#------------------------------------------------------------------------------------------------------------------------
#' specify the pair of genes, typically a TF and a targetGene
#'
#' @rdname setGenes
#' @aliases setGenes
#'
#' @param obj An object of class GeneGeneExpressionPlotter
#' @param gene1 A character string
#' @param gene2 A character string
#'
#' @export
#'
setMethod("setGenes", "GeneGeneExpressionPlotter",

     function(obj, gene1, gene2) {
        obj@state$gene1 <- gene1
        obj@state$gene2 <- gene2
        })

#------------------------------------------------------------------------------------------------------------------------
#' specity the expression matrix
#'
#' @rdname setMatrixForPlotting
#' @aliases setMatrixForPlotting
#'
#' @param obj An object of class GeneGeneExpressionPlotter
#' @param matrix the expression matrix
#'
#' @export
#'
setMethod("setMatrixForPlotting", "GeneGeneExpressionPlotter",

     function(obj, matrix) {
        obj@state$matrix <- matrix
        })

#------------------------------------------------------------------------------------------------------------------------
#' create and return the control-rich UI
#'
#' @rdname createPage
#' @aliases createPage
#'
#' @param obj An object of class GeneGeneExpressionPlotter
#'
#' @export
#'
setMethod("createPage", "GeneGeneExpressionPlotter",

      function(obj){
         div(id="GeneGeneExpressionPlotterPageContent")
            #extendShinyjs(script=system.file(package="TrenaViz", "js", "GeneGeneExpressionPlotter.js")),
       })

#------------------------------------------------------------------------------------------------------------------------
#' display the page
#'
#' @rdname displayPage
#' @aliases displayPage
#'
#' @param obj An object of class GeneGeneExpressionPlotter
#' @param tf  character string, the geneSymbol name of the transcription factor
#'
#' @export
#'
setMethod("displayPage", "GeneGeneExpressionPlotter",

     function(obj, tf){
         if(!obj@quiet)
            printf("GeneGeneExpressionPlotter displayPage, tf: %s",   tf)
         setTF(obj, tf)
         })

#------------------------------------------------------------------------------------------------------------------------
#' add shiny event handlers
#'
#' @rdname addEventHandlers
#' @aliases addEventHandlers
#'
#' @param obj An object of class BindingSitesManager
#' @param session a Shiny session object
#' @param input a Shiny input object
#' @param output a Shiny output object
#'
#' @export
#'
setMethod("addEventHandlers", "GeneGeneExpressionPlotter",

     function(obj, session, input, output){

       obj@state$session <- session
       obj@state$input <- input
       obj@state$output <- output

     }) # addEventHandlers

#------------------------------------------------------------------------------------------------------------------------
