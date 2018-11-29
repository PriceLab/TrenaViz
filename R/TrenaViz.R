#' import shiny
#' import shinydashboard
#' import shinyjs
#' import later
#' import igvShiny
#' import DT
#' import VariantAnnotation
#' import trenaSGM
#' importFrom  RColorBrewer brewer.pal
#' import later
#------------------------------------------------------------------------------------------------------------------------
#' @name TrenaViz
#' @rdname TrenaViz
#' @aliases TrenaViz
#------------------------------------------------------------------------------------------------------------------------
state <- new.env(parent=emptyenv())
state$models <- list()
state$tbl.chipSeq <- NULL
model.count <- 0   # for creating default model names
MAX.TF.COUNT <- 50   # we display the top max.tf.cout TFs coming back from trena

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
setGeneric('createApp',     signature='obj', function(obj, ui, server) standardGeneric("createApp"))
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
   require(projectName, character.only=TRUE)
   initialization.command <- sprintf("trenaProject <- %s()", projectName)
   eval(parse(text=initialization.command))
   setTargetGene(trenaProject, getSupportedGenes(trenaProject)[1])

   state$tbl.enhancers <- getEnhancers(trenaProject)
   state$tbl.dhs <- getEncodeDHS(trenaProject)
   state$tbl.transcripts <- getTranscriptsTable(trenaProject)

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
        .createSidebar(obj@project),
        dashboardBody(
           tags$head(tags$style(HTML('
        .main-header .logo {
          font-family: "Georgia", Times, "Times New Roman", serif;
          font-weight: bold;
          font-size: 24px;
          }
        #igvShiny{
          background: white;
          border: 1px solid black;
          border-radius: 5px;
          margin: 5px;
          margin-right: 15px;
          overflow: hidden;
          }
        #table{
          border: 1px solid black;
          border-radius: 5px;
          }
       '))),
       .createBody(obj@project)),
       useShinyjs()
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

   observeEvent(input$chooseGene, ignoreInit=TRUE, {
       newGene <- isolate(input$chooseGene)
       setTargetGene(obj@project, newGene)
       showGenomicRegion(session, getGeneRegion(obj@project, flankingPercent=20))
       printf("new targetGene: %s", getTargetGene(obj@project))
       state$tbl.enhancers <- getEnhancers(obj@project)
       state$tbl.dhs <- getEncodeDHS(obj@project)
       state$tbl.transcripts <- getTranscriptsTable(obj@project)
       shinyjs::html(selector=".logo", html=sprintf("trena %s", newGene), add=FALSE)
       loadAndDisplayRelevantVariants(obj@project, session, newGene)
       })


   output$igvShiny <- renderIgvShiny({
      options <- list(genomeName="hg38",
                      initialLocus=getGeneRegion(obj@project, flankingPercent=20),
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
      })

   setupIgvAndTableToggling(session, input);
   setupAddTrack(obj@project, session, input, output)
   setupDisplayRegion(obj@project, session, input, output)
   setupBuildModel(obj@project, session, input, output)
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
                        selectInput("selectRowAction", NULL,  c("(no action)", "Footprints", "ChIP-seq hits",
                                                                sprintf("Plot expression, TF vs target gene")))
                      ) # column
               ) # fluidRow
            ), # tabItem 1
         .createBuildModelTab(project),
         .createVideoTab()
         ) # tabItems

   return(body)

} # createBody
#------------------------------------------------------------------------------------------------------------------------
.createSidebar <- function(trenaProject)
{
  dashboardSidebar(
    sidebarMenu(id="sidebarMenu",
       menuItem("IGV and Current Models", tabName = "igvAndTable"),
       menuItem("Build a new Model",      tabName = "buildModels"),
       menuItem("Introductory video",     tabName = "video")
      ),
    conditionalPanel(id="igvTableWidgets",
        condition = "input.sidebarMenu == 'igvAndTable'",
        actionButton(inputId = "igvHideButton", label = "Toggle IGV"),
        actionButton(inputId = "tableHideButton", label = "Toggle table"),
        selectInput("chooseGene", "Choose Gene:", c("", getSupportedGenes(trenaProject))),
        selectInput("addTrack", "Add Track:", .additionalTracksOnOffer(trenaProject)),
        selectInput("displayGenomicRegion", "Display Genomic Region:",
                    c("",
                      "Full Gene" = "fullGeneRegion",
                      "Full Enhancers Region" = "fullEnhancerRegion")),
        actionButton(inputId="removeUserAddedTracks", label="Remove Added Tracks")
        ) # conditionalPanel
     ) # dashboardSidebar

} # .createSidebar
#------------------------------------------------------------------------------------------------------------------------
.additionalTracksOnOffer <- function(trenaProject)
{
   trackOfferings <- list("",
                          "Enhancers"="enhancers",
                          "DNase HS (encode, clustered)"="dhs")

   variantTrackNames <- getVariantDatasetNames(trenaProject)

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

} # additionalTracksOnOffer
#------------------------------------------------------------------------------------------------------------------------
.createBuildModelTab <- function(project)
{
   footprintDatabases <- getFootprintDatabaseNames(project)
   expressionMatrixNames <- getExpressionMatrixNames(project)

   tab <- tabItem(tabName="buildModels",
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
#' Create a runnable shiny app
#'
#' @rdname createApp
#' @aliases createApp
#'
#' @param obj An object of class TrenaViz
#'
#' @export
#'
setMethod('createApp', 'TrenaViz',

  function(obj){

     server <- function(session, input, output){
        x <- createServer(obj, session, input, output)
        }

     shinyOptions=list(launch.browser=FALSE)
     if(Sys.info()[["nodename"]] == "riptide.local"){
        shinyOptions <- list(host="0.0.0.0", launch.browser=TRUE)
        }

     app <- shinyApp(createUI(obj), server, options=shinyOptions)
     return(app)
  })
#------------------------------------------------------------------------------------------------------------------------
