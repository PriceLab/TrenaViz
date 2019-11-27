#' @import TrenaProject
#' @import shiny
#' @import shinydashboard
#' @import shinyjs
#' @import later
#' @import yaml
#' @import igvShiny
#' @import DT
#' @import VariantAnnotation
#' @import trenaSGM
#' @importFrom  RColorBrewer brewer.pal
#' @import colourpicker
#' @import later
#' @import MotifDb
#' @import ggseqlogo
#' @importFrom GenomicScores gscores
#' @import phastCons7way.UCSC.hg38
#------------------------------------------------------------------------------------------------------------------------
#' @name TrenaViz
#' @rdname TrenaViz
#' @aliases TrenaViz
#------------------------------------------------------------------------------------------------------------------------
tbl.region <- data.frame(chrom="chr19", start=1036002, end=1142642, stringsAsFactors=FALSE)
bsm <- BindingSitesManager()
fimoBuilder <- FimoModelWidget()
genomicRegionString <- "" #with(tbl.regions, sprintf("%s:%d-%d", chrom, start, end))
#------------------------------------------------------------------------------------------------------------------------
state <- new.env(parent=emptyenv())
state$trenaVizRunning <- FALSE
state$models <- list()
state$tbl.chipSeq <- NULL
model.count <- 0   # for creating default model names
MAX.TF.COUNT <- 100   # we display the top max.tf.cout TFs coming back from trena

colors <- brewer.pal(8, "Dark2")
totalColorCount <- length(colors)
state$colorNumber <- 0
#------------------------------------------------------------------------------------------------------------------------
.TrenaViz <- setClass ("TrenaViz",
                    representation = representation(
                       projectName="character",
                       project="TrenaProject",
                       quiet="logical")
                    )
#------------------------------------------------------------------------------------------------------------------------
setGeneric('createUI',      signature='obj', function(obj) standardGeneric("createUI"))
setGeneric('createServer',  signature='obj', function(obj, session, input, output) standardGeneric("createServer"))
setGeneric('createApp',     signature='obj', function(obj, port=NA_integer_) standardGeneric("createApp"))
#------------------------------------------------------------------------------------------------------------------------
#' Create an TrenaViz object
#'
#' @description
#' a shiny app
#'
#' @rdname TrenaViz
#'
#' @param projectName A string indicating a TrenaProject subclass
#' @param quiet A logical indicating whether or not the Trena object should print output
#'
#' @return An object of the TrenaViz class
#'
#' @export
#'
TrenaViz <- function(projectName, quiet=TRUE)
{
   state$trenaVizRunning <- TRUE
   require(projectName, character.only=TRUE)
   initialization.command <- sprintf("trenaProject <- %s()", projectName)
   eval(parse(text=initialization.command))
   setTargetGene(trenaProject, "GATA2")
   genome.build <- getGenome(trenaProject)
   setGenome(bsm, genome.build)
   setTrenaProject(fimoBuilder, trenaProject)
   setTargetGene(fimoBuilder, "GATA2")
   tbls.regulatoryRegions <- get(load("~/github/TrenaViz/inst/devel/tbls.regulatoryRegions.RData"))
   setRegulatoryRegions(fimoBuilder, tbls.regulatoryRegions)

   organism <- "Hsapiens"
   if(genome.build == "mm10")
      organism <- "Mmusculus"
   setOrganism(bsm, organism) # getOrganism(trenaProject)
   #tbl.region <- data.frame(chrom="chr19", start=1036002, end=1142642, stringsAsFactors=FALSE)
   region.list <- getGeneRegion(trenaProject, 20)
   tbl.region <- with(region.list, data.frame(chrom=chrom, start=start, end=end, stringsAsFactors=FALSE))
   #printf("--- calling sgr(bsm) from TrenaViz ctor")
   #setGenomicRegion(bsm, tbl.region)

   dataManifest <- list()
   base.class.manifest.file <- system.file(package="TrenaProject", "extdata", "genomeAnnotation", "manifest.yaml")
   project.manifest.file <- system.file(package=projectName, "extdata", "genomeAnnotation", "manifest.yaml")

   if(file.exists(base.class.manifest.file))
      dataManifest <- c(dataManifest, yaml.load(readLines(base.class.manifest.file)))

   if(file.exists(project.manifest.file))
      dataManifest <- c(dataManifest, yaml.load(readLines(project.manifest.file)))

   setTargetGene(trenaProject, getSupportedGenes(trenaProject)[1])

   state$tbl.enhancers <- getEnhancers(trenaProject)
   state$tbl.dhs <- getEncodeDHS(trenaProject)
   state$tbl.transcripts <- getTranscriptsTable(trenaProject)
   state$dataManifest <- dataManifest
   state$chromLocRegion <- getGeneRegion(trenaProject, 20)$chromLocString

   obj <- .TrenaViz(projectName=projectName, project=trenaProject, quiet=quiet)

   obj

} # TrenaViz ctor
#------------------------------------------------------------------------------------------------------------------------
#' Create the user interface
#'
#' @rdname createUI
#' @aliases createUI
#'
#' @param obj An object of class TrenaViz
#'
#' @export
#'
setMethod('createUI', 'TrenaViz',

   function(obj){

      ui <- dashboardPage(
        dashboardHeader(title=sprintf("trena %s", getTargetGene(obj@project))),
        .createSidebar(obj),
        dashboardBody(
           includeCSS(system.file(package="TrenaViz", "css", "trenaViz.css")),
           useShinyjs(),
           extendShinyjs(script=system.file(package="TrenaViz", "js", "trenaViz.js")),
           extendShinyjs(script=system.file(package="TrenaViz", "js", "bindingSitesManager.js")),
           extendShinyjs(script=system.file(package="TrenaViz", "js", "fimoBuilder.js")),
          .createBody(obj@project))
       ) # dashboardPage
    return(ui)
   })

#------------------------------------------------------------------------------------------------------------------------
#' Create the shiny server
#'
#' @rdname createServer
#' @aliases createServer
#'
#' @param obj An object of class TrenaViz
#' @param session A shiny session
#' @param input A shiny input object
#' @param output A shiny output object
#'
#' @export
#'
setMethod('createServer', 'TrenaViz',

   function(obj, session, input, output){

      .createTrackFileUploader(session, input, output)
      addEventHandlers(bsm, session, input, output)

      js$installTrenaVizReturnKeyHandlers()

      observeEvent(input$sidebarMenu, {
         tabName <- input$sidebarMenu
         printf("sidebar menu selected: %s", tabName)
         if(tabName == "fimoDatabaseModelBuilderTab"){
            printf("--- creating fimoBuilder tab, adding event hanlders, setting genomic region")
            displayPage(fimoBuilder)
            addEventHandlers(fimoBuilder, session, input, output)
            current.region <- state[["chromLocRegion"]]
            #print("  creating a fresh fimoBuilder page")
            #print("want to call setGenomicRegion(fimoBuilder, ...")
            #print("  current.region for fimoBuilder:")
            print(current.region)  # a chromLocString like this: "chr3:128,480,214-128,487,293"
            chromLoc <- trena::parseChromLocString(current.region)
            tbl.region <- with(chromLoc, data.frame(chrom=chrom, start=start, end=end, stringsAsFactors=FALSE))
            setGenomicRegion(fimoBuilder, tbl.region)
            }
         })

      observeEvent(input$textInput_offListGene_widget_returnKey, ignoreInit=TRUE, {
         printf("--- textInput_offListGene_widget_returnKey event received")
         newGene <- isolate(input$textInput_offListGene)
         if(getGenome(obj@project) == "hg38")
            newGene <- toupper(newGene)
         legit <- recognizedGene(obj@project, newGene)
         if(!legit){
            msg <- sprintf("'%s' not found in current datasets.", newGene)
            showModal(modalDialog(title="gene nomination error", msg))
            }
         if(legit){
            shinyjs::html(selector=".logo", html=sprintf("trena %s", newGene), add=FALSE)
            success <- setTargetGene(obj@project, newGene)
            newRegion <- getGeneRegion(obj@project, flankingPercent=20)$chromLocString
            printf("--- new region for %s: %s", newGene, newRegion)
            showGenomicRegion(session, newRegion)
            state$tbl.enhancers <- getEnhancers(obj@project)
            state$tbl.dhs <- getEncodeDHS(obj@project)
            state$tbl.transcripts <- getTranscriptsTable(obj@project)
            }
         })


      observeEvent(input$setOffListGeneButton, ignoreInit=TRUE, {
         newGene <- toupper(isolate(input$offListGene))
         legit <- recognizedGene(obj@project, newGene)
         if(!legit){
            msg <- sprintf("'%s' not found in current datasets.", newGene)
            showModal(modalDialog(title="gene nomination error", msg))
            }
         if(legit){
            shinyjs::html(selector=".logo", html=sprintf("trena %s", newGene), add=FALSE)
            setTargetGene(obj@project, newGene)
            state$tbl.enhancers <- getEnhancers(obj@project)
            state$tbl.dhs <- getEncodeDHS(obj@project)
            state$tbl.transcripts <- getTranscriptsTable(obj@project)
            showGenomicRegion(session, getGeneRegion(obj@project, flankingPercent=20)$chromLocString)
            }
         })

   observeEvent(input$chooseGeneFromList, ignoreInit=TRUE, {
       #  TODO: consolidate this code with that above, setOffListGeneButton code
       newGene <- isolate(input$chooseGeneFromList)
       if(nchar(newGene) > 0){
          shinyjs::html(selector=".logo", html=sprintf("trena %s", newGene), add=FALSE)
          setTargetGene(obj@project, newGene)
          showGenomicRegion(session, getGeneRegion(obj@project, flankingPercent=20)$chromLocString)
          printf("new targetGene: %s", getTargetGene(obj@project))
          state$tbl.enhancers <- getEnhancers(obj@project)
          state$tbl.dhs <- getEncodeDHS(obj@project)
          state$tbl.transcripts <- getTranscriptsTable(obj@project)
          loadAndDisplayRelevantVariants(obj@project, session, newGene)
          later(function() {updateSelectInput(session, "chooseGeneFromList", selected=character(0))}, 1)
          } # if not empty string
       })


   output$igvShiny <- renderIgvShiny({
      printf("project name: %s", getProjectName(obj@project))
      printf("project targetGene: %s", getTargetGene(obj@project))
      region.list <- getGeneRegion(obj@project, flankingPercent=20)
      chromLocString <- region.list$chromLocString
      tbl.region <- with(region.list, data.frame(chrom=chrom, start=start, end=end, stringsAsFactors=FALSE))
      printf("renderIgvShiny, chrom loc?  %s", chromLocString)
      printf("--- calling sgr(bsm) from renderIgvShiny")
      setGenomicRegion(bsm, tbl.region)
      setGenomicRegion(fimoBuilder, tbl.region)
      options <- list(genomeName=getGenome(obj@project),
                      initialLocus=chromLocString,
                      displayMode="EXPANDED",
                      trackHeight=300)
      igvShiny(options) # , height=800)
      })

   output$table = DT::renderDataTable(data.frame(),
                                      width="800px",
                                      class='nowrap display',
                                      selection="single",
                                      extensions="FixedColumns",
                                      options=list(scrollX=TRUE,
                                                   scrollY="500px",
                                                   dom='t',
                                                   paging=FALSE,
                                                   autowWdth=FALSE,
                                                   fixedColumns=list(leftColumns=1)
                                                   ))

   observeEvent(input$table_rows_selected, {
      selectedTableRow <- isolate(input$table_rows_selected)
      dispatch.rowClickInModelTable(obj@project, session, input, output, selectedTableRow)
      }) # observe row selection event

   observeEvent(input$currentGenomicRegion, {
      new.region <- isolate(input$currentGenomicRegion)
      #printf("new region: %s", new.region)
      state[["chromLocRegion"]] <- new.region
      chromLoc <- trena::parseChromLocString(new.region)
      tbl.region <- with(chromLoc, data.frame(chrom=chrom, start=start, end=end, stringsAsFactors=FALSE))
      printf("--- calling sgr(bsm)(fimoBuilder) from TrenaViz:observeEvent, input$currentGenomicRegion")
      print(tbl.region)
      setGenomicRegion(bsm, tbl.region)
      setGenomicRegion(fimoBuilder, tbl.region)
      })

   setupIgvAndTableToggling(session, input);
   setupAddTrack(obj@project, session, input, output)
   setupDisplayRegion(obj@project, session, input, output)
   setupBuildModel(obj@project, session, input, output)
   setupDisplayMotifs(obj@project, session, input, output)
   })

#------------------------------------------------------------------------------------------------------------------------
.createBody <- function(project)
{
   body <- #fluidRow(
      tabItems(
         tabItem(tabName="igvAndTable",
            fluidRow(
               column(width=9, id="igvColumn", igvShinyOutput('igvShiny', height="2000px")),
               column(width=3, id="dataTableColumn",
                      selectInput("modelSelector", NULL,  "- no models yet -"),
                      DTOutput("table"),
                      br(),
                      #wellPanel(
                        h4("TF selection will display:"),
                      selectInput("selectRowAction", NULL,  c("(no action)",
                                                              "Motif-matched regulatory regions",
                                                              "Calculate Binding Sites",
                                                              "ChIP-seq hits"))
                      ) # column
               ) # fluidRow
            ), # tabItem 1
         .createBuildModelTab(project),
         .createFimoDatabaseModelBuilderTab(),
         .createVideoTab(),
         .createBindingSitesManagerTab()
         ) # tabItems

   return(body)

} # createBody
#------------------------------------------------------------------------------------------------------------------------
.createSidebar <- function(obj)
{
  dashboardSidebar(
    sidebarMenu(id="sidebarMenu",
       menuItem("IGV and Current Models", tabName = "igvAndTable"),
       menuItem("Build Footprint Model",  tabName = "buildFootprintModels"),
       menuItem("Build ATAC-seq Model",   tabName = "fimoDatabaseModelBuilderTab"),
       menuItem("Introductory video",     tabName = "video"),
       menuItem("Binding Sites Manager",  tabName = "bindingSitesManagerTab")
      ),
     #h5("Choose TF:"),
     #selectInput("tfSelector", NULL,  c("", "HES7", "LYL1", "IRF5", "SPI1", "CEBPA", "ELK3", "RUNX1")),

    conditionalPanel(id="igvTableWidgets",
        condition = "input.sidebarMenu == 'igvAndTable'",
        actionButton(inputId = "igvHideButton", label = "Toggle IGV"),
        actionButton(inputId = "tableHideButton", label = "Toggle table"),
        selectInput("chooseGeneFromList", "Choose Gene From List:", c("", getSupportedGenes(obj@project))),
        # div(style="display: inline-block;vertical-align:top; width:100px; margin:0px; !important;",
        textInput("textInput_offListGene", label="Enter off-list gene"), # , placeholder="enter off-list gene here"),
        #div(style="display: inline-block;vertical-align:top; width: 40px; margin-left:0px; margin-top:8px; !important;",
        #actionButton(inputId="setOffListGeneButton", label="Set off-list gene"),
        selectInput("addTrack", "Add Track:", .additionalTracksOnOffer(obj)),
        actionButton(inputId = "addTrackFromFileButton", label="Add Track from File..."),
        selectInput("displayGenomicRegion", "Display Genomic Region:",
                    c("",
                      "Proximal Promter -2500/+500" = "proximal.promoter.2500.500",
                      "Full Gene" = "fullGeneRegion",
                      "Full Enhancers Region" = "fullEnhancerRegion")),
        actionButton(inputId="removeUserAddedTracks", label="Remove Added Tracks")
        ) # conditionalPanel
     ) # dashboardSidebar

} # .createSidebar
#------------------------------------------------------------------------------------------------------------------------
.additionalTracksOnOffer <- function(obj)
{
   trackOfferings <- list("",
                          "DNase HS (encode, clustered)"="dhs",
                          "phast7" = "phast7")

   dm <- state$dataManifest

   for(i in seq_len(length(dm))){
      detailed.name <- names(dm)[i]
      desc <- dm[[i]]
      display.name <- desc$displayName
      trackOfferings[[display.name]] <- detailed.name
      xyz <- 99
      }

   genomicRegionNames <- getGenomicRegionsDatasetNames(obj@project)

   for(i in seq_len(length(genomicRegionNames))){
      trackName <- genomicRegionNames[i]
      new.list <- c(trackName)
      names(new.list) <- trackName
      trackOfferings <- c(trackOfferings, new.list)
      } # for i

   variantTrackNames <- getVariantDatasetNames(obj@project)

   for(i in seq_len(length(variantTrackNames))){
      trackName <- variantTrackNames[i]
      if(grepl("gwas", trackName, ignore.case=TRUE)){
         new.list <- c(trackName)
         names(new.list) <- trackName
         trackOfferings <- c(trackOfferings, new.list)
         } # if 'gwas' in the name
      if(grepl(".variants", trackName, ignore.case=TRUE)){
         new.list <- c(trackName)
         names(new.list) <- trackName
         trackOfferings <- c(trackOfferings, new.list)
         } # if '.variants' in the name
      } # for i

   return(trackOfferings)

} # .additionalTracksOnOffer
#------------------------------------------------------------------------------------------------------------------------
.createBuildModelTab <- function(project)
{
   footprintDatabases <- getFootprintDatabaseNames(project)
   expressionMatrixNames <- getExpressionMatrixNames(project)

   tab <- tabItem(tabName="buildFootprintModels",
             h4("Build trena regulatory model from DNase footprints in the genomic region currently displayed in IGV, with these constraints:"),
             br(),
             fluidRow(
                column(3, offset=1, checkboxGroupInput("footprintDatabases", "Footprint Databases",
                                                       footprintDatabases,
                                                       selected=footprintDatabases[1])),
                column(4, radioButtons("intersectWithRegions", "Intersect footprints with:",
                                             c("GeneHancer" = "genehancer",
                                               "Encode DHS" = "encodeDHS",
                                               "GeneHancer OR Encode DHS" = "geneHancerOrEncode",
                                               "GeneHancer AND Encode DHS" = "geneHancerAndEncode",
                                               "Use all footprints in region" = "allDNAForFootprints"),
                                       selected="allDNAForFootprints")),
                column(3, radioButtons("motifMapping", "Map motifs to TFs via:",
                                       c("MotifDb", "TFClass", "MotifDb + TFClass"),
                                       selected="MotifDb"))
                ),
             fluidRow(
                column(6, selectInput("expressionSet", "With this gene expression dataset:",
                                      c("", expressionMatrixNames))),
                column(5, textInput("modelNameTextInput", "Model name", width=500))),
             fluidRow(column(width=1, offset=4, actionButton("buildModelButton", "Build model"))),
             br(),br(),
             wellPanel(style="overflow-y:scroll; height:200px", pre(id = "console"))
             ) # tabItem

   return(tab)

} # .createBuildModelTab
#------------------------------------------------------------------------------------------------------------------------
.createVideoTab <- function()
{
   file <- system.file(package="TrenaViz", "extdata", "video", "videoTutorial.html")

   tab <- tabItem(tabName="video",
                  h4("Using TrenaViz: a video tutorial"),
                  includeHTML(file)
                  )
   return(tab)

} # .createVideoTab
#------------------------------------------------------------------------------------------------------------------------
# .createMotifTab <- function()
# {
#    xyz <- ".createMotifTab"
#    tab <- tabItem(tabName="motifTab",
#                   h4("Select a Motif"),
#                   actionButton(inputId = "displayMotifButton", label = "Display Motif"),
#                   plotOutput("motifPlotRenderingPanel", height=800)
#                   )
#    return(tab)
#
# } # .createMotifTab
#------------------------------------------------------------------------------------------------------------------------
.createBindingSitesManagerTab <- function()
{
   tabItem(tabName="bindingSitesManagerTab",
           fluidPage(id="bindingSitesManagerPage",
                     h3(id="bindingSitesManager_title", "Explore Binding Sites"),
                     h4(id="bindingSitesManager_currentTF", sprintf("TF: %s", "none yet specified")),
                     h4(id="bindingSitesManager_currentGenomicRegion", genomicRegionString),
                     textInput("textInput_exploreAnotherTF", label="Specify a new TF:"),
                     fluidRow(id="bindingSitesManagerPageContent")))

} # .createExperimentalTab
#------------------------------------------------------------------------------------------------------------------------
.createFimoDatabaseModelBuilderTab <- function()
{
   printf("creating fimoDatabaseModelBuilderTab")
   tabItem(tabName="fimoDatabaseModelBuilderTab",
           fluidPage(id="FimoModelWidgetPage",
                     h3(id="fimoDatabaseBuild_title", "Build Gene Regulatory Model Using Fimo Database"),
                     fluidRow(id="FimoDatabaseModelBuilderPageContent")))

} # .createFimoDatabaseModelBuilderTab
#------------------------------------------------------------------------------------------------------------------------
#' Create a runnable shiny app
#'
#' @rdname createApp
#' @aliases createApp
#'
#' @param obj An object of class TrenaViz
#' @param port An integer (e.g., 3838, 60041) NA_integer_ by default
#'
#' @export
#'
setMethod('createApp', 'TrenaViz',

  function(obj, port=NA_integer_){

     server <- function(session, input, output){
        x <- createServer(obj, session, input, output)
        }

     if(Sys.info()[["nodename"]] == "riptide.local"){
        shinyOptions <- list(host="0.0.0.0", port=port, launch.browser=TRUE)
     } else {
        shinyOptions=list(launch.browser=FALSE, host='0.0.0.0', port=port)
        }

     app <- shinyApp(createUI(obj), server, options=shinyOptions)

     return(app)

  })
#------------------------------------------------------------------------------------------------------------------------
