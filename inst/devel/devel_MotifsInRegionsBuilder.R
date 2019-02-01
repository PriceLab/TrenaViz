library(shiny)
library(shinydashboard)
library(TrenaViz)
library(shinyjs)
#------------------------------------------------------------------------------------------------------------------------
tbl.region <- data.frame(chrom="chr19", start=1036002, end=1142642, stringsAsFactors=FALSE)
genomicRegionString <- with(tbl.region, sprintf("%s:%d-%d", chrom, start, end))
mrb <- MotifsInRegionsBuilder()
#------------------------------------------------------------------------------------------------------------------------
.createSidebar <- function()
{
   dashboardSidebar(
   sidebarMenu(id="sidebarMenu",
       menuItem("main",                       tabName = "mainTab"),
       menuItem("Motifs in Region Builder",   tabName = "motifsInRegionsBuilderTab")
       )
    )

} # .createSidebar
#------------------------------------------------------------------------------------------------------------------------
.createMainTab <- function()
{
  tabItem(tabName="mainTab",
     div(
        actionButton("randomChromLocsButton", "New region")
        )
     )

} # .createMainTab
#------------------------------------------------------------------------------------------------------------------------
.createMotifsInRegionsBuilderTab <- function()
{
   tabItem(tabName="motifsInRegionsBuilderTab",
           fluidPage(id="motifsInRegionsBuilderPage",
                     h3(id="motifsInRegionsBuilder_title", "Build a Explore Binding Sites"),
                     h4(id="motifsInRegionsBuilder_currentTF", sprintf("TF: %s", "none yet specified")),
                     h4(id="motifsInRegionsBuilder_currentGenomicRegion", genomicRegionString),
                     textInput(inputId="textInput_exploreAnotherTF", label="Specify a new TF:"),
                     fluidRow(id="motifsInRegionsBuilderPageContent")))

} # .createExperimentalTab
#------------------------------------------------------------------------------------------------------------------------
.createBody <- function()
{
   dashboardBody(
      includeCSS(system.file(package="TrenaViz", "css", "trenaViz.css")),
      useShinyjs(),
      # extendShinyjs(script=system.file(package="TrenaViz", "js", "motifsInRegionsBuilder.js")),
      tabItems(
         .createMainTab(),
         .createMotifsInRegionsBuilderTab()
         ))

} # .createBody
#------------------------------------------------------------------------------------------------------------------------
ui <- dashboardPage(

   dashboardHeader(title="MotifsInRegionsBuilder devel"),
   .createSidebar(),
   .createBody()

)
#------------------------------------------------------------------------------------------------------------------------
server <- function(session, input, output)
{
   #setOrganism(mrb, "Hsapiens")
   #setGenome(mrb, "hg38")
   #tbl.region <- data.frame(chrom="chr19", start=1036002, end=1142642, stringsAsFactors=FALSE)
   #setGenomicRegion(mrb, tbl.region)

   #addEventHandlers(mrb, session, input, output)

       #-----------------------------------------------------------------------------
       # for testing only.   button is added to the ui by devel_MotifsInRegionsBuilder.R
       #-----------------------------------------------------------------------------

   printf("---- adding observer for randomChromLocsButton")
   observeEvent(input$randomChromLocsButton, ignoreInit=FALSE, {
      printf("setting new genomic regions")
      tbl.region <- data.frame(chrom="chr19",
                               start=1036002 + as.integer(100 * runif(1)),
                               end=1142642   +  as.integer(100 * runif(1)),
                               stringsAsFactors=FALSE)
      printf("new random region created, sent nowhere yet")
      #setGenomicRegion(mrb, tbl.region)
      })

} # server
#------------------------------------------------------------------------------------------------------------------------
runApp(shinyApp(ui, server))

