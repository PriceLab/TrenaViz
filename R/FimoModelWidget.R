# FimoModelWidget.R
#------------------------------------------------------------------------------------------------------------------------
#' @import shiny
#' @import TrenaProject
#' @name FimoModelWidget
#' @rdname FimoModelWidget
#' @aliases FimoModelWidget
#------------------------------------------------------------------------------------------------------------------------
# library(TrenaProject)
# library(cyjShiny)
#------------------------------------------------------------------------------------------------------------------------
.FimoModelWidget <- setClass("FimoModelWidget",
                             representation = representation(
                                trenaProject="TrenaProject",
                                tbls.regulatoryRegions="list",
                                state="environment",
                                quiet="logical")
                                )
#------------------------------------------------------------------------------------------------------------------------
#' Create an FimoModelWidget object
#'
#' @description
#' a shiny app
#'
#' @rdname FimoModelWidget
#'
#' @param trenaProjectName character eg, "TrenaProjectErythropoiesis"
#' @param targetGene character standard geneSymbol name, eg, "GATA2"
#' @param genomicRegion character, eg "chr3:128,475,725-128,498,247"
#'
#' @return An object of the FimoModelWidget class
#'
#' @export
#'
FimoModelWidget <- function(trenaProject, targetGene, genomicRegion, tbls.regulatoryRegions, quiet=TRUE)
{
   state <- new.env(parent=emptyenv())

   state$targetGene <- targetGene
   state$genomicRegion <- genomicRegion
   state$expressionMatrixName <- ""
   state$tfbsTrack <- ""
   state$fimoThreshold <- 4.0
   state$tfCorrelationThreshold <- 0.4
   state$modelSize <- 10

   .FimoModelWidget(trenaProject=trenaProject,
                             tbls.regulatoryRegions=tbls.regulatoryRegions,
                             state=state,
                             quiet=quiet)

} # FimoModelWidget
#------------------------------------------------------------------------------------------------------------------------
setMethod("show", "FimoModelWidget",

    function(object){
        cat(paste("a FimoModelWidget object from the TrenaViz package:", "\n"))
        cat(sprintf("  trena project: %s\n", getProjectName(obj@trenaProject)))
        })

#------------------------------------------------------------------------------------------------------------------------
setGeneric('.fimoBuilderCreatePage',  signature='obj', function(obj) standardGeneric('.fimoBuilderCreatePage'))
#------------------------------------------------------------------------------------------------------------------------
#' create and return the control-rich UI
#'
#' @rdname createPage
#' @aliases createPage
#'
#' @param obj An object of class FimoModelWidget
#'
#'
setMethod(".fimoBuilderCreatePage", "FimoModelWidget",

      function(obj) {
         fluidPage(id="FimoModelWidgetPageContent",
                   includeCSS(system.file(package="TrenaViz", "css", "fimoDatabaseModelBuilder.css")),
                   fluidRow(
                      column(width=7, offset=1, id="fimoModelBuilderTitleBox",
                             h4(id="fimoModelBuilder_currentTrenaProject", sprintf("%s", getProjectName(obj@trenaProject))),
                             h4(id="fimoModelBuilder_currentTargetene", sprintf("Target GENE: %s", obj@state$targetGene)),
                             h4(id="fimoModelBuilder_currentGenomicRegion",
                               with(obj@state$genomicRegion, sprintf("In region: %s:%d-%d", chrom, start, end)))
                             )
                   ),
                   fluidRow(
                      column(width=7,
                             selectInput("expressionMatrixSelector", "Expression Matrix",
                                         c("", getExpressionMatrixNames(obj@trenaProject))),
                             selectInput("tfbsTrackSelector", "Restrict TFs to those binding in track: ",
                                         c("", "No restriction: all DNA in current region", names(obj@tbls.regulatoryRegions)))
                             ),
                      column(width=5,
                             sliderInput("fimoThresholdSelector", "FIMO motif match cutoff -log10(pVal)", 1, 10, value=4, step=0.1),
                             sliderInput("tfCorrelationThresholdSelector", "TF/targetGene expression min correlation", 0, 1, value=0.4, step=0.1),
                             sliderInput("modelSizeSelector", "Regulatory model max size", 5, 200, value=10, step=1)
                             )
                      ), # fluidRow
                   fluidRow(
                      column(width=2, offset=0,
                             actionButton("buildModelButton", "Build Regulatory Model")
                             )),
                   #fluidRow(
                   #   column(width=2, offset=0, id="fubar",
                   #          actionButton("viewNewModelButton", "View")
                   #          ))
                   br(),br(),
                   wellPanel(style="overflow-y:scroll; height:200px", pre(id = "console"))
                   ) # fluidPage

      }) # createPage

#------------------------------------------------------------------------------------------------------------------------
#' display the page
#'
#' @rdname displayPage
#' @aliases displayPage
#'
#' @param obj An object of class FimoModelWidget
#' @param tf  character string, the geneSymbol name of the transcription factor
#'
#' @export
#'
setMethod("displayPage", "FimoModelWidget",

     function(obj){
        printf("=== FimoModelWidget displayPage")
        removeUI(selector="#FimoModelWidgetPageContent", immediate=TRUE)
        insertUI(selector="#FimoModelWidgetPage", where="beforeEnd", .fimoBuilderCreatePage(obj), immediate=TRUE)
        shinyjs::disable("buildModelButton")
        #shinyjs::disable("viewNewModelButton")
        })

#------------------------------------------------------------------------------------------------------------------------
#' add shiny event handlers
#'
#' @rdname addEventHandlers
#' @aliases addEventHandlers
#'
#' @param obj An object of class FimoModelWidget
#' @param session a Shiny session object
#' @param input a Shiny input object
#' @param output a Shiny output object
#'
#' @export
#'
setMethod("addEventHandlers", "FimoModelWidget",

     function(obj, session, input, output){

        obj@state$session <- session
        obj@state$input <- input
        obj@state$output <- output

        observeEvent(input$expressionMatrixSelector, ignoreInit=TRUE, {
           mtx.name <- input$expressionMatrixSelector
           obj@state$expressionMatrixName <- mtx.name
           enableBuildButton <- nchar(obj@state$expressionMatrixName) > 0 & nchar(obj@state$tfbsTrack) > 0
           if(enableBuildButton)
              shinyjs::enable("buildModelButton")
           else
              shinyjs::disable("buildModelButton")
           })

        observeEvent(input$tfbsTrackSelector, ignoreInit=TRUE, {
           tfbs.track <- input$tfbsTrackSelector
           obj@state$tfbsTrack <- tfbs.track
           enableBuildButton <- nchar(obj@state$expressionMatrixName) > 0 & nchar(obj@state$tfbsTrack) > 0
           if(enableBuildButton)
              shinyjs::enable("buildModelButton")
           else
              shinyjs::disable("buildModelButton")
           })

        observeEvent(input$viewFimoModelWidgetButton, ignoreInit=FALSE, {
          printf("view builder ")
          updateTabItems(session, "sidebarMenu", selected="fimoDatabaseModelBuilderTab")
          displayPage(obj)
          output$messageDisplayWidget <- renderText(obj@state$message)
          })

        observeEvent(input$fimoThresholdSelector, ignoreInit=FALSE, {
          printf("fimo threshold: %f", input$fimoThresholdSelector)
          obj@state$fimoThreshold <- input$fimoThresholdSelector
          })

        observeEvent(input$tfCorrelationThresholdSelector, ignoreInit=FALSE, {
          printf("tf correlation threshold: %f", input$tfCorrelationThresholdSelector)
          obj@state$tfCorrelationThreshold <- input$tfCorrelationThresholdSelector
          })

        observeEvent(input$modelSizeSelector, ignoreInit=FALSE, {
          printf("model size: %d", input$modelSizeSelector)
          obj@state$modelSize <- input$modelSizeSelector
          })

        observeEvent(input$buildModelButton, ignoreInit=TRUE, {
           printf("==== build model ")
           printf("  targetGene: %s", obj@state$targetGene)
           printf("  genomicRegion: %s", str(obj@state$genomicRegion))
           printf("  matrix: %s",  obj@state$expressionMatrixName)
           printf("  track:  %s",  obj@state$tfbsTrack)
           printf("  fimoThreshold: %f",  obj@state$fimoThreshold)
           printf("  tf correlation threshold: %f", obj@state$tfCorrelationThreshold)
           printf("  model size: %d",  obj@state$modelSize)
           db.name <- system.file(package="TrenaProjectErythropoiesis", "extdata", "fimoDBs",
                                  "gata2.gh.fimoBindingSites.sqlite")
           tbl.regions <- with(obj@state$genomicRegion, data.frame(chrom=chrom, start=start, end=end, stringsAsFactors=FALSE))
           track <- obj@state$tfbsTrack
           xyz <- "building recipe"
           if(track != "No restriction: all DNA in current region"){
              tbl.restriction <- obj@tbls.regulatoryRegions[[track]]
              tbl.ov <- as.data.frame(findOverlaps(GRanges(tbl.restriction), GRanges(tbl.regions)))
              tbl.regions <- tbl.restriction[unique(tbl.ov$queryHits),]
              }
           fimo.pvalue.threshold <- 1/10^(obj@state$fimoThreshold)
           tss <- getTranscriptsTable(obj@trenaProject, obj@state$targetGene)$tss[1]
           mtx <- getExpressionMatrix(obj@trenaProject, obj@state$expressionMatrixName)
           recipe <- list(title="fimo.firstTry",
                      type="fimo.database",
                      regions=tbl.regions,
                      geneSymbol=obj@state$targetGene,
                      tss=tss,
                      matrix=mtx,
                      db.host="localhost",
                      db.port=NA_integer_,
                      databases=list(db.name),
                      annotationDbFile=dbfile(org.Hs.eg.db),
                      motifDiscovery="fimoDatabase",
                      tfPool=allKnownTFs(),
                      tfMapping="MotifDB",
                      tfPrefilterCorrelation=obj@state$tfCorrelationThreshold,
                      maxModelSize=obj@state$modelSize,
                      fimoPValueThreshold=fimo.pvalue.threshold,
                      orderModelByColumn="rfScore",
                      solverNames=c("lasso", "lassopv", "pearson", "randomForest", "ridge", "spearman"))
           builder <- trenaSGM::FimoDatabaseModelBuilder(getGenome(obj@trenaProject),
                                                         obj@state$targetGene,
                                                         recipe)


      tryCatch({
          withCallingHandlers({
             message(sprintf("starting build"))
             x <- build(builder)
             message(sprintf("build complete"))
             browser()
             message(sprintf("model has %d tfs", nrow(x$model)))
             if(exists("state")){
                if(state$trenaVizRunning){
                   model.count <- length(state$models)
                   new.model.name <- names(state$models)[model.count]
                   new.table <- state$models[[model.count]]$model
                   displayModel(session, input, output, new.table, new.model.name)
                   updateTabItems(session, "sidebarMenu", select="igvAndTable")
                   }
                }
             },
           message=function(m){
              shinyjs::html(id="console", html=m$message, add=TRUE)
              })
           }, error=function(e){
               msg <- e$message
               print(msg)
               showModal(modalDialog(title="trena model building error", msg))
               }) # tryCatch

           }) # obseve buildModelButton

     }) # addEventHandlers

#------------------------------------------------------------------------------------------------------------------------
buildFimoDatabaseModel <- function(trenaProject, session, input, output)
{
   model.name <- sprintf("trena.model.%s", input$modelNameTextInput)
   message(sprintf("about to build '%s'", model.name))
   # browser()
   # xyz <- "tvHelpders::buildModel"
   footprint.database.host <- getFootprintDatabaseHost(trenaProject)
   footprint.database.names <- input$footprintDatabases
   tracks.to.intersect.with <- input$intersectWithRegions
   motifMapping <- isolate(input$motifMapping)
   if(tolower(motifMapping) == "motifdb + tfclass")
      motifMapping <- c("MotifDb", "TFClass")
   expressionMatrixName <- input$expressionSet
   message(sprintf("   mtx: %s", expressionMatrixName))
   full.roi <- state$chromLocRegion
   message(sprintf("   roi: %s", full.roi))
   chrom.loc <- trena::parseChromLocString(full.roi)
   message(sprintf("  fpdb: %s", paste(footprint.database.names, collapse=", ")))
   message(sprintf("   roi: %s", full.roi))
   message(sprintf("   mtx: %s", expressionMatrixName))
   message(printf("  intersect with: %s", paste(tracks.to.intersect.with, collapse=",")))

   #browser()
   tbl.gene <- subset(state$tbl.transcripts)[1,]
   strand <- tbl.gene$strand
   tss <- tbl.gene$start
   if(strand == "-")
      tss <- tbl.gene$endpos

   run.trenaSGM(trenaProject,
                model.name,
                chrom.loc$chrom, chrom.loc$start, chrom.loc$end,
                tss,
                expressionMatrixName,
                tracks.to.intersect.with,
                footprint.database.host,
                footprint.database.names,
                motifMapping)

} # buildDatabaseModel
#------------------------------------------------------------------------------------------------------------------------
