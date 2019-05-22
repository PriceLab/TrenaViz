#' import shiny
#' import cyjShiny
#' import TrenaProject
#' import graph
#' @name NetworkView
#' @rdname NetworkView
#' @aliases NetworkView
#------------------------------------------------------------------------------------------------------------------------
# library(TrenaProject)
# library(cyjShiny)
#------------------------------------------------------------------------------------------------------------------------
.NetworkView <- setClass("NetworkView",
                             representation = representation(
                                quiet="logical",
                                targetGene="character",
                                tbl.model="data.frame",
                                tbl.regulatoryRegions="data.frame",
                                expressionMatrix="matrix",
                                state="environment")
                                )
#------------------------------------------------------------------------------------------------------------------------
setGeneric("cyjLayout", signature='obj', function(obj) standardGeneric('cyjLayout'))
setGeneric('getGraph',  signature='obj', function(obj) standardGeneric('getGraph'))
#------------------------------------------------------------------------------------------------------------------------
#'  provide html layout to the caller
#'
#' @rdname cyjLayout
#' @aliases cyjLayout
#'
#' @param obj An object of class NetworkView
#'
#' @export
#'
setMethod('cyjLayout', 'NetworkView',

    function(obj){
       fluidPage(
          fluidRow(
             actionButton(inputId="fitNetworkButton", label="Fit"),
             actionButton(inputId="fitSelectedNodesButton", label="Fit Selection"),
             actionButton(inputId="removeNetworkButton", label="Remove Graph"),
             ),
          fluidRow(column(width=12, cyjShinyOutput('cyjShiny')))
          )
       })

#------------------------------------------------------------------------------------------------------------------------
setMethod('getGraph', 'NetworkView',

   function(obj){
      tbl.nodes <- data.frame(id=c("A", "B", "C"),
                              type=c("kinase", "TF", "glycoprotein"),
                              lfc=c(1, 1, 1),
                              count=c(0, 0, 0),
                              stringsAsFactors=FALSE)
      tbl.edges <- data.frame(source=c("A", "B", "C"),
                              target=c("B", "C", "A"),
                              interaction=c("phosphorylates", "synthetic lethal", "unknown"),
                              stringsAsFactors=FALSE)
      graph.json <- dataFramesToJSON(tbl.edges, tbl.nodes)
      targetGene <- obj@targetGene
      tbl.model <- obj@tbl.model
      tbl.reg <- obj@tbl.regulatoryRegions
      g <- .geneRegulatoryModelToGraph(target.gene, tbl.model, tbl.reg)
      printf("------- getGraph, model graph:")
      print(g)
      graph.json <- graphNELtoJSON(g)
      return(graph.json)
      })

#------------------------------------------------------------------------------------------------------------------------
#' Create an NetworkView object
#'
#' @description
#' a shiny app
#'
#' @rdname NetworkView
#'
#' @param organism  A character string, one of the supported species names:  hsapiens, mmuscuulus
#' @param genome  A character string, one of the supported genome builds: hg38, mm10
#' @param quiet A logical indicating whether or not the Trena object should print output
#'
#' @return An object of the NetworkView class
#'
#' @export
#'
NetworkView <- function(targetGene, tbl.model, tbl.regulatoryRegions, expressionMatrix, quiet=TRUE)
{
   state <- new.env(parent=emptyenv())
   .NetworkView(targetGene=targetGene,
                tbl.model=tbl.model,
                tbl.regulatoryRegions=tbl.regulatoryRegions,
                expressionMatrix=expressionMatrix,
                state=state,
                quiet=quiet)

} # NetworkView
#------------------------------------------------------------------------------------------------------------------------
setMethod("show", "NetworkView",

    function(object){
        cat(paste("a NetworkView object from the TrenaViz package:", "\n"))
        cat(sprintf("  targetGene: %s\n", obj@targetGene))
        cat(sprintf("  tbl.model: %d rows, %d columns\n", nrow(obj@tbl.model), ncol(obj@tbl.model)))
        cat(sprintf("  tbl.regulatoryRegions: %d rows, %d columns\n", nrow(obj@tbl.regulatoryRegions),
                                                                      ncol(obj@tbl.regulatoryRegions)))
        cat(sprintf("  expressionMatrix: %d rows, %d columns\n",      nrow(obj@expressionMatrix),
                                                                      ncol(obj@expressionMatrix)))
        })

#------------------------------------------------------------------------------------------------------------------------
#' create and return the control-rich UI
#'
#' @rdname createPage
#' @aliases createPage
#'
#' @param obj An object of class NetworkView
#'
#' @export
#'
setMethod("createPage", "NetworkView",

      function(obj) {
         fluidPage(id="networkViewPageContent",
          fluidRow(
             actionButton(inputId="fitNetworkButton", label="Fit"),
             actionButton(inputId="fitSelectedNodesButton", label="Fit Selection"),
             actionButton(inputId="removeNetworkButton", label="Remove Graph")
             ),
          fluidRow(column(width=12, cyjShinyOutput('cyjShiny')))
          )
        #cyjShinyOutput('cyjShiny', height=400)

       })

#------------------------------------------------------------------------------------------------------------------------
#' display the page
#'
#' @rdname displayPage
#' @aliases displayPage
#'
#' @param obj An object of class NetworkView
#' @param tf  character string, the geneSymbol name of the transcription factor
#'
#' @export
#'
setMethod("displayPage", "NetworkView",

     function(obj){
         printf("NetworkView displayPage")
         removeUI(selector="#networkViewPageContent", immediate=TRUE)
         insertUI(selector="#networkViewPage", where="beforeEnd", createPage(obj), immediate=TRUE)
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
#' @param obj An object of class NetworkView
#' @param session a Shiny session object
#' @param input a Shiny input object
#' @param output a Shiny output object
#'
#' @export
#'
setMethod("addEventHandlers", "NetworkView",

     function(obj, session, input, output){

        printf("--- NetworkView::addEventHandlers")
        obj@state$session <- session
        obj@state$input <- input
        obj@state$output <- output

        observeEvent(input$fitNetworkButton, ignoreInit=TRUE, {
           fit(session, 80)
           })

        observeEvent(input$fitSelectedNodesButton, ignoreInit=TRUE, {
           fitSelected(session, 80)
           })

        observeEvent(input$removeNetworkButton, ignoreInit=TRUE, {
           removeGraph(session)
           })

       observeEvent(input$viewNetworkButton, ignoreInit=FALSE, {
          printf("view network")
          updateTabItems(session, "sidebarMenu", selected="networkViewTab")
          # displayPage(obj)
          xyz <- "observing viewNetworkButton"
          output$cyjShiny <- renderCyjShiny({
             printf("--- renderCyjShiny, triggered by viewNetworkButton")
             cyjShiny(getGraph(obj), layoutName="cola", style_file=NULL, width=1000, height=1000)
             })
        })

     }) # addEventHandlers

#------------------------------------------------------------------------------------------------------------------------
.geneRegulatoryModelToGraph <- function(target.gene, tbl.model, tbl.reg)
{
   xyz <- ".geneRegulatoryModelToGraph"

   browser()
   required.geneModelColumnNames <- c("tf", "pearson", "spearman", "betaLasso", "randomForest", "pcaMax", "concordance")
   required.regulatoryRegionsColumnNames <- c("motifName", "chrom", "motifStart", "motifEnd", "strand",
                                              "motifScore", "motifRelativeScore", "match",
                                              "distance.from.tss",
                                              "chromStart", "chromEnd", "seq", "status", "tf")
   stopifnot(all(required.geneModelColumnNames %in% colnames(tbl.model)))
   stopifnot(all(required.regulatoryRegionsColumnNames %in% colnames(tbl.reg)))

   printf("genes: %d, %d occurences of %d motifs", length(tbl.model$tf), length(tbl.reg$motifName),
          length(unique(tbl.reg$motifName)))

   g <- graphNEL(edgemode = "directed")

   nodeDataDefaults(g, attr = "type") <- "undefined"             # targetGene, tf, footprint
   nodeDataDefaults(g, attr = "label") <- "default node label"
   nodeDataDefaults(g, attr = "distance") <- 0
   nodeDataDefaults(g, attr = "pearson") <- 0
   nodeDataDefaults(g, attr = "randomForest") <- 0
   nodeDataDefaults(g, attr = "pcaMax") <- 0
   nodeDataDefaults(g, attr = "concordance") <- 0
   nodeDataDefaults(g, attr = "betaLasso") <- 0
   nodeDataDefaults(g, attr = "motif") <- ""
   nodeDataDefaults(g, attr = "xPos") <- 0
   nodeDataDefaults(g, attr = "yPos") <- 0

   edgeDataDefaults(g, attr = "edgeType") <- "undefined"

   tfs <- tbl.model$tf

   regRegions.names <- unlist(lapply(1:nrow(tbl.reg), function(i){
       distance.from.tss <- tbl.reg$distance.from.tss[i]
       region.size <- nchar(tbl.reg$match[i])
       motif.name <- tbl.reg$motifName[i]
       if(distance.from.tss < 0)
          sprintf("%s.fp.downstream.%05d.L%d.%s", target.gene, abs(distance.from.tss), region.size, motif.name)
        else
          sprintf("%s.fp.upstream.%05d.L%d.%s", target.gene, abs(distance.from.tss), region.size, motif.name)
        }))

   tbl.reg$regionName <- regRegions.names
   all.nodes <- unique(c(target.gene, tfs, regRegions.names))
   g <- addNode(all.nodes, g)

   nodeData(g, target.gene, "type") <- "targetGene"
   nodeData(g, tfs, "type")         <- "TF"
   nodeData(g, regRegions.names, "type")  <- "regulatoryRegion"
   nodeData(g, all.nodes, "label")  <- all.nodes
   nodeData(g, regRegions.names, "label") <- tbl.reg$motifName
   nodeData(g, regRegions.names, "distance") <- tbl.reg$distance
   nodeData(g, regRegions.names, "motif") <- tbl.reg$motifName

   nodeData(g, tfs, "pearson") <- tbl.model$pearson
   nodeData(g, tfs, "betaLasso") <- tbl.model$betaLasso
   nodeData(g, tfs, "randomForest") <- tbl.model$randomForest
   nodeData(g, tfs, "pcaMax") <- tbl.model$pcaMax
   nodeData(g, tfs, "concordance") <- tbl.model$concordance

   #browser()
   g <- addEdge(tbl.reg$tf, tbl.reg$regionName, g)
   edgeData(g,  tbl.reg$tf, tbl.reg$regionName, "edgeType") <- "bindsTo"

   g <- graph::addEdge(tbl.reg$regionName, target.gene, g)
   edgeData(g, tbl.reg$regionName, target.gene, "edgeType") <- "regulatorySiteFor"

   g

} # .geneRegulatoryModelToGraph
#------------------------------------------------------------------------------------------------------------------------
