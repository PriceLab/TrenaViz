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
                                tss="numeric",
                                tbl.model="data.frame",
                                tbl.regulatoryRegions="data.frame",
                                state="environment")
                                )
#------------------------------------------------------------------------------------------------------------------------
setGeneric('getGraph',  signature='obj', function(obj) standardGeneric('getGraph'))
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
      tss <- obj@tss
      g <- .geneRegulatoryModelToGraph(targetGene, tss, tbl.model, tbl.reg)
      g <- .addGeneModelLayout(g, xPos.span=1500)
      g
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
NetworkView <- function(targetGene, tss, tbl.model, tbl.regulatoryRegions, quiet=TRUE)
{
   state <- new.env(parent=emptyenv())
   .NetworkView(targetGene=targetGene,
                tss=tss,
                tbl.model=tbl.model,
                tbl.regulatoryRegions=tbl.regulatoryRegions,
                state=state,
                quiet=quiet)

} # NetworkView
#------------------------------------------------------------------------------------------------------------------------
setMethod("show", "NetworkView",

    function(object){
        cat(paste("a NetworkView object from the TrenaViz package:", "\n"))
        cat(sprintf("  targetGene: %s\n", obj@targetGene))
        cat(sprintf("  tss: %s\n", obj@tss))
        cat(sprintf("  tbl.model: %d rows, %d columns\n", nrow(obj@tbl.model), ncol(obj@tbl.model)))
        cat(sprintf("  tbl.regulatoryRegions: %d rows, %d columns\n", nrow(obj@tbl.regulatoryRegions),
                                                                      ncol(obj@tbl.regulatoryRegions)))
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
             actionButton(inputId="removeNetworkButton", label="Remove Graph"),
             actionButton(inputId="genomicLayoutButton", label="GenomicLayout")
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

        observeEvent(input$genomicLayoutButton, ignoreInit=TRUE, {
           setNodePositions(session, obj@state$tbl.pos)
           })

        observeEvent(input$viewNetworkButton, ignoreInit=FALSE, {
          printf("view network")
          updateTabItems(session, "sidebarMenu", selected="networkViewTab")
          # displayPage(obj)
          xyz <- "observing viewNetworkButton"
          output$cyjShiny <- renderCyjShiny({
             printf("--- renderCyjShiny, triggered by viewNetworkButton")
             style.file <- system.file(package="TrenaViz", "extdata", "trenaModelStyle.js")
             g <- getGraph(obj)
             obj@state$g <- g
             print(g)
             graph.json <- graphNELtoJSON(g)
             xPos <- nodeData(g, attr="xPos")
             yPos <- nodeData(g, attr="yPos")
             tbl.pos <- data.frame(id=names(xPos), x=as.numeric(xPos), y=as.numeric(yPos), stringsAsFactors=FALSE)
             obj@state$tbl.pos <- tbl.pos
             cyjShiny(graph.json, layoutName="cola", styleFile=style.file, width=1000, height=1000)
             })
        })

     }) # addEventHandlers

#------------------------------------------------------------------------------------------------------------------------
# by example:
#
# the incoming tbl.model presents these challenges:
#
#       gene betaLasso  lassoPValue pearsonCoeff  rfScore   betaRidge spearmanCoeff bindingSites
#  6    E2F3         0 7.124847e-07    0.8683105 2.936714  0.04945335     0.8149973           NA
#  45 HOXC13         0 3.987483e-02   -0.8640875 2.457541 -0.01531601    -0.7659080           NA
#  97 ZNF263         0 6.236969e-01    0.9003067 2.134046  0.04104303     0.6360153           NA
#  70  PRDM4         0 1.000000e+00    0.8984506 1.900193  0.03627523     0.7405583           NA
#
# and for which we want these results (first 4 rows only)
#
#         tf    pearson   spearman betaLasso randomForest
#  6    E2F3  0.8683105  0.8149973         0     2.936714
#  45 HOXC13 -0.8640875 -0.7659080         0     2.457541
#  97 ZNF263  0.9003067  0.6360153         0     2.134046
#  70  PRDM4  0.8984506  0.7405583         0     1.900193

.standardizeModelTable <- function(tbl.model)
{
   required.colNames <- c("tf", "pearson", "spearman", "betaLasso", "randomForest")
   colnames.in <- tolower(colnames(tbl.model))
   gene.col <- grep("^gene$", colnames.in)

   if(length(gene.col) > 0)
      colnames(tbl.model)[gene.col] <- "tf"

   pearson.col <- grep("pearson", colnames.in)
   if(length(pearson.col) > 0)
      colnames(tbl.model)[pearson.col] <- "pearson"

   spearman.col <- grep("spearman", colnames.in)
   if(length(spearman.col) > 0)
      colnames(tbl.model)[spearman.col] <- "spearman"

   betaLasso.col <- grep("betalasso", colnames.in)
   if(length(betaLasso.col) > 0)
      colnames(tbl.model)[betaLasso.col] <- "betaLasso"

   rf.1.col <- grep("forest", colnames.in)
   rf.2.col <- grep("rfscore", colnames.in)

   if(length(rf.1.col) > 0)
      colnames(tbl.model)[rf.1.col] <- "randomForest"

   if(length(rf.2.col) > 0)
      colnames(tbl.model)[rf.2.col] <- "randomForest"

   tbl.out <- tbl.model[, required.colNames]
   tbl.out

} # .standardizeModelTable
#------------------------------------------------------------------------------------------------------------------------
# by example:
#
# one instance of the incoming tbl.reg presents these challenges:
#
#                                motifName                      loc  fp_start    fp_end               type                                     name length strand   sample_id method                provenance score1   score2   score3 score4 score5 score6 chrom            database          shortMotif geneSymbol pubmedID organism  source
# Hsapiens-HOCOMOCOv10-CLOCK_HUMAN.H10MO.D chr3:128077447-128077466 128077441 128077451 motif.in.footprint Hsapiens-HOCOMOCOv10-CLOCK_HUMAN.H10MO.D     20      + ENCSR000EMT   HINT lymphoblast_hint_16.minid     12 13.02250 1.81e-05     NA     NA     NA  chr3 lymphoblast_hint_16 CLOCK_HUMAN.H10MO.D      CLOCK 26586801 Hsapiens MotifDb
#  Hsapiens-HOCOMOCOv10-PURA_HUMAN.H10MO.D chr3:128417965-128417981 128417972 128417989 motif.in.footprint  Hsapiens-HOCOMOCOv10-PURA_HUMAN.H10MO.D     17      - ENCSR000EJK   HINT lymphoblast_hint_20.minid     12 12.04400 3.25e-05     NA     NA     NA  chr3 lymphoblast_hint_20  PURA_HUMAN.H10MO.D       PURA 26586801 Hsapiens MotifDb
#      Hsapiens-jaspar2016-HOXC11-MA0651.1 chr3:128617604-128617614 128617598 128617621 motif.in.footprint      Hsapiens-jaspar2016-HOXC11-MA0651.1     11      - ENCSR000DBZ   HINT lymphoblast_hint_20.minid     32 11.10000 7.99e-05     NA     NA     NA  chr3 lymphoblast_hint_20            MA0651.1     HOXC11 24194598 Hsapiens MotifDb
#         Hsapiens-jaspar2016-SP4-MA0685.1 chr3:128487237-128487253 128487253 128487267 motif.in.footprint         Hsapiens-jaspar2016-SP4-MA0685.1     17      - ENCSR000EJE   HINT lymphoblast_hint_16.minid     24  4.01124 8.85e-05     NA     NA     NA  chr3 lymphoblast_hint_16            MA0685.1        SP4 24194598 Hsapiens MotifDb
#      Hsapiens-jaspar2016-ZBTB7A-MA0750.1 chr3:128617856-128617867 128617847 128617892 motif.in.footprint      Hsapiens-jaspar2016-ZBTB7A-MA0750.1     12      + ENCSR000DCA   HINT lymphoblast_hint_16.minid     20 15.76400 2.28e-06     NA     NA     NA  chr3 lymphoblast_hint_16            MA0750.1     ZBTB7A 24194598 Hsapiens MotifDb
#
# from which we wish to extract:
#

#   chrom     start       end  tf                                  motif
# 1  chr3 128483072 128483461 MAZ Hsapiens-HOCOMOCOv10-MAZ_HUMAN.H10MO.A
# 2  chr3 128483072 128483461 SP4 Hsapiens-HOCOMOCOv10-SP4_HUMAN.H10MO.D
# 3  chr3 128483072 128483461 SP2 Hsapiens-HOCOMOCOv10-SP2_HUMAN.H10MO.C
# 4  chr3 128483072 128483461 SP3 Hsapiens-HOCOMOCOv10-SP3_HUMAN.H10MO.B
# 5  chr3 128483072 128483461 SP3 Hsapiens-SwissRegulon-SP3.SwissRegulon
# 6  chr3 128483072 128483461 SP1 Hsapiens-HOCOMOCOv10-SP1_HUMAN.H10MO.C
#
# and we want
#  chrom      start       end name distance motifName
#   chr3  128483072 128483461 MAZ Hsapiens-HOCOMOCOv10-MAZ_HUMAN.H10MO.A

#
#    regRegions.names <- unlist(lapply(1:nrow(tbl.reg), function(i){
#        distance.from.tss <- tbl.reg$distance.from.tss[i]
#        region.size <- nchar(tbl.reg$match[i])
#        motif.name <- tbl.reg$motifName[i]
#        if(distance.from.tss < 0)
#           sprintf("%s.fp.downstream.%05d.L%d.%s", targetGene, abs(distance.from.tss), region.size, motif.name)
#         else
#           sprintf("%s.fp.upstream.%05d.L%d.%s", targetGene, abs(distance.from.tss), region.size, motif.name)
#         }))
#
#    tbl.reg$regionName <- regRegions.names
#    all.nodes <- unique(c(targetGene, tfs, regRegions.names))
#    g <- addNode(all.nodes, g)
#
#    nodeData(g, targetGene, "type") <- "targetGene"
#    nodeData(g, tfs, "type")         <- "TF"
#    nodeData(g, regRegions.names, "type")  <- "regulatoryRegion"
#    nodeData(g, all.nodes, "label")  <- all.nodes
#    nodeData(g, regRegions.names, "label") <- tbl.reg$motifName
#    nodeData(g, regRegions.names, "distance") <- tbl.reg$distance
#    nodeData(g, regRegions.names, "motif") <- tbl.reg$motifName
#
.standardizeRegulatoryRegionsTable <- function(tbl.reg, targetGene, tss)
{
   locs <- lapply(tbl.reg$loc, parseChromLocString)
   tbl.rough <- do.call(rbind, lapply(locs, as.data.frame))
   tbl.rough$chrom <- as.character(tbl.rough$chrom)

   tbl.rough <- cbind(tbl.rough, tbl.reg[, c("motifName", "fp_start", "fp_end", "geneSymbol")])
   make.name <- function(tss, start, tf){
      distance.from.tss <- tss - start
      sprintf("%s:%d:%s", targetGene, distance.from.tss, tf)
      }

   regulatory.region.names <- unlist(lapply(1:nrow(tbl.reg),
                                     function(i) make.name(tss, tbl.rough$fp_start[i], tbl.rough$geneSymbol[i])))
   tbl.rough$name <- regulatory.region.names
   tbl.rough$distance <- tss - tbl.rough$fp_start
   tbl.rough$targetGene <- targetGene

   tbl.out <- tbl.rough[, c("chrom", "fp_start", "fp_end", "distance", "name", "targetGene", "geneSymbol", "motifName")]
   colnames(tbl.out) <- c("chrom", "start", "end", "distance", "name", "targetGene", "tf", "motif")
   rownames(tbl.out) <- NULL
   tbl.out

} # .standardizeRegulatoryRegionsTable
#------------------------------------------------------------------------------------------------------------------------
.geneRegulatoryModelToGraph <- function(targetGene, tss, tbl.model, tbl.reg)
{
   xyz <- ".geneRegulatoryModelToGraph"

   tbl.model <- .standardizeModelTable(tbl.model)
   tbl.reg <- .standardizeRegulatoryRegionsTable(tbl.reg, targetGene, tss)
   required.geneModelColumnNames <- c("tf", "pearson", "spearman", "betaLasso", "randomForest")
   required.regulatoryRegionsColumnNames <- c("chrom", "start", "end", "distance", "name", "targetGene", "tf", "motif")
   stopifnot(all(required.geneModelColumnNames %in% colnames(tbl.model)))
   stopifnot(all(required.regulatoryRegionsColumnNames %in% colnames(tbl.reg)))

   printf("genes: %d, %d occurences of %d motifs", length(tbl.model$tf), length(tbl.reg$motif),
          length(unique(tbl.reg$motif)))

   g <- graphNEL(edgemode = "directed")

   nodeDataDefaults(g, attr = "type") <- "undefined"             # targetGene, tf, footprint
   nodeDataDefaults(g, attr = "label") <- "default node label"
   nodeDataDefaults(g, attr = "distance") <- 0
   nodeDataDefaults(g, attr = "pearson") <- 0
   nodeDataDefaults(g, attr = "randomForest") <- 0
   nodeDataDefaults(g, attr = "betaLasso") <- 0
   nodeDataDefaults(g, attr = "motif") <- ""
   nodeDataDefaults(g, attr = "xPos") <- 0
   nodeDataDefaults(g, attr = "yPos") <- 0

   edgeDataDefaults(g, attr = "edgeType") <- "undefined"

   tfs <- tbl.model$tf

   all.nodes <- unique(c(targetGene, tfs, tbl.reg$name))
   g <- addNode(all.nodes, g)

   nodeData(g, targetGene, "type") <- "targetGene"
   nodeData(g, tfs, "type")         <- "TF"
   nodeData(g, tbl.reg$name, "type")  <- "regulatoryRegion"
   nodeData(g, all.nodes, "label")  <- all.nodes
   xyz <- "NetworkView assiging graph node data"
   nodeData(g, tbl.reg$name, "label") <- tbl.reg$motif
   nodeData(g, tbl.reg$name, "distance") <- tbl.reg$distance
   nodeData(g, tbl.reg$name, "motif") <- tbl.reg$motifName

   nodeData(g, tfs, "pearson") <- tbl.model$pearson
   nodeData(g, tfs, "betaLasso") <- tbl.model$betaLasso
   nodeData(g, tfs, "randomForest") <- tbl.model$randomForest

   g <- addEdge(tbl.reg$tf, tbl.reg$name, g)
   edgeData(g,  tbl.reg$tf, tbl.reg$name, "edgeType") <- "bindsTo"

   g <- graph::addEdge(tbl.reg$name, targetGene, g)
   edgeData(g, tbl.reg$name, targetGene, "edgeType") <- "regulatorySiteFor"

   g

} # .geneRegulatoryModelToGraph
#------------------------------------------------------------------------------------------------------------------------
.addGeneModelLayout <- function(g, xPos.span=1500)
{
    all.distances <- sort(unique(unlist(nodeData(g, attr='distance'), use.names=FALSE)))
    print(all.distances)

    fp.nodes <- nodes(g)[which(unlist(nodeData(g, attr="type"), use.names=FALSE) == "regulatoryRegion")]
    tf.nodes <- nodes(g)[which(unlist(nodeData(g, attr="type"), use.names=FALSE) == "TF")]
    targetGene.nodes <- nodes(g)[which(unlist(nodeData(g, attr="type"), use.names=FALSE) == "targetGene")]

     # add in a zero in case all of the footprints are up or downstream of the 0 coordinate, the TSS
    span.endpoints <- range(c(0, as.numeric(nodeData(g, fp.nodes, attr="distance"))))
    span <- max(span.endpoints) - min(span.endpoints)
    footprintLayoutFactor <- 1
    printf("initial:  span: %d  footprintLayoutFactor: %f", span, footprintLayoutFactor)

    footprintLayoutFactor <- xPos.span/span

    #if(span < 600)  #
    #   footprintLayoutFactor <- 600/span
    #if(span > 1000)
    #   footprintLayoutFactor <- span/1000

    printf("corrected:  span: %d  footprintLayoutFactor: %f", span, footprintLayoutFactor)

    xPos <- as.numeric(nodeData(g, fp.nodes, attr="distance")) * footprintLayoutFactor
    yPos <- 0
    nodeData(g, fp.nodes, "xPos") <- xPos
    nodeData(g, fp.nodes, "yPos") <- yPos

    adjusted.span.endpoints <- range(c(0, as.numeric(nodeData(g, fp.nodes, attr="xPos"))))
    printf("raw span of footprints: %d   footprintLayoutFactor: %f  new span: %8.0f",
           span, footprintLayoutFactor, abs(max(adjusted.span.endpoints) - min(adjusted.span.endpoints)))

    tfs <- names(which(nodeData(g, attr="type") == "TF"))

    for(tf in tfs){
       footprint.neighbors <- edges(g)[[tf]]
       if(length(footprint.neighbors) > 0){
          footprint.positions <- as.integer(nodeData(g, footprint.neighbors, attr="xPos"))
          new.xPos <- mean(footprint.positions)
          if(is.na(new.xPos)) browser()
          if(is.nan(new.xPos)) browser()
          #printf("%8s: %5d", tf, new.xPos)
          }
       else{
          new.xPos <- 0
          }
       nodeData(g, tf, "yPos") <- sample(300:1200, 1)
       nodeData(g, tf, "xPos") <- new.xPos
       } # for tf

    nodeData(g, targetGene.nodes, "xPos") <- 0
    nodeData(g, targetGene.nodes, "yPos") <- -200

    g

} # .addGeneModelLayout
#------------------------------------------------------------------------------------------------------------------------
