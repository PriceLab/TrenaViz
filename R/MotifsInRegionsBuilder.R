#' import shiny
#' @name MotifsInRegionsBuilder
#' @rdname MotifsInRegionsBuilder
#' @aliases MotifsInRegionsBuilder
#------------------------------------------------------------------------------------------------------------------------
.MotifsInRegionsBuilder <- setClass ("MotifsInRegionsBuilder",
                                  representation = representation(
                                     quiet="logical",
                                     state="environment")
                                  )
#------------------------------------------------------------------------------------------------------------------------
# setGeneric("addEventHandlers",    signature="obj", function(obj, session, input, output) standardGeneric("addEventHandlers"))
# setGeneric("setOrganism",         signature="obj", function(obj, organism) standardGeneric("setOrganism"))
# setGeneric("setGenome",           signature="obj", function(obj, genomeName) standardGeneric("setGenome"))
# setGeneric("setGenomicRegion",    signature="obj", function(obj, tbl.region) standardGeneric("setGenomicRegion"))
# setGeneric("setTF",               signature="obj", function(obj, tf) standardGeneric("setTF"))
setGeneric("createMotifModelBuilderPage",          signature="obj", function(obj) standardGeneric("createMotifModelBuilderPage"))
# setGeneric("renderLogos",         signature="obj", function(obj, tfMappingOption) standardGeneric("renderLogos"))
# setGeneric("removeLogos",         signature="obj", function(obj) standardGeneric("removeLogos"))
# setGeneric("displayPage",         signature="obj", function(obj, tf) standardGeneric("displayPage"))
#------------------------------------------------------------------------------------------------------------------------
#' Create an MotifsInRegionsBuilder object
#'
#' @description
#' a shiny app
#'
#' @rdname MotifsInRegionsBuilder
#'
#' @param organism  A character string, one of the supported species names:  hsapiens, mmuscuulus
#' @param genome  A character string, one of the supported genome builds: hg38, mm10
#' @param quiet A logical indicating whether or not the Trena object should print output
#'
#' @return An object of the MotifsInRegionsBuilder class
#'
#' @export
#'
MotifsInRegionsBuilder <- function(quiet=TRUE)
{
   state <- new.env(parent=emptyenv())
   state$TF <- NULL
   state$region <- data.frame(chrom="chr1", start=1, end=2, stringsAsFactors=FALSE)
   state$regionString <- "chr1:1-2"
   state$organism <- NULL
   state$genome <- NULL

   .MotifsInRegionsBuilder(state=state,  quiet=quiet)

} # MotifsInRegionsBuilder
#------------------------------------------------------------------------------------------------------------------------
#' create and return the control-rich UI
#'
#' @rdname createMotifModelBuilderPage
#' @aliases createMotifModelBuilderPage
#'
#' @param obj An object of class MotifsInRegionsBuilder
#'
#' @export
#'
setMethod("createMotifModelBuilderPage", "MotifsInRegionsBuilder",

      function(obj) {
         div(id="motifsInRegionPageContent",
            fluidRow(
               column(6, offset=2, h3(id="motifsInRegionPageTitle", "Motifs in Region"))))

       })

#------------------------------------------------------------------------------------------------------------------------
#' display the page
#'
#' @rdname displayPage
#' @aliases displayPage
#'
#' @param obj An object of class MotifsInRegionsBuilder
#' @param tf  character string, the geneSymbol name of the transcription factor
#'
#' @export
#'
setMethod("displayPage", "MotifsInRegionsBuilder",

     function(obj, tf){
        printf("--- MotifsInRegionsBuilder::displayPage")
       })

#------------------------------------------------------------------------------------------------------------------------
#' add shiny event handlers
#'
#' @rdname addEventHandlers
#' @aliases addEventHandlers
#'
#' @param obj An object of class MotifsInRegionsBuilder
#' @param session a Shiny session object
#' @param input a Shiny input object
#' @param output a Shiny output object
#'
#' @export
#'
setMethod("addEventHandlers", "MotifsInRegionsBuilder",

     function(obj, session, input, output){

       printf("--- MotifsInRegionsBuilder::addEventHandlers")

       obj@state$session <- session
       obj@state$input <- input
       obj@state$output <- output
       })


#------------------------------------------------------------------------------------------------------------------------
