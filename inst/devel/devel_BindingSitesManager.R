library(shiny)
library(shinydashboard)
library(TrenaViz)
library(shinyjs)
#------------------------------------------------------------------------------------------------------------------------
tbl.region <- data.frame(chrom="chr19", start=1036002, end=1142642, stringsAsFactors=FALSE)
bsm <- BindingSitesManager("Hsapiens", "hg38", tbl.region)
# setGenomicRegion(bsm, tbl.region)
genomicRegionString <- with(tbl.region, sprintf("%s:%d-%d", chrom, start, end))
#------------------------------------------------------------------------------------------------------------------------
.createSidebar <- function()
{
   dashboardSidebar(
   sidebarMenu(id="sidebarMenu",
       menuItem("main",                    tabName = "mainTab"),
       menuItem("Binding Sites Manager",   tabName = "bindingSitesManagerTab")
       )
    )

} # .createSidebar
#------------------------------------------------------------------------------------------------------------------------
.createMainTab <- function()
{
  tabItem(tabName="mainTab",
     div(
        h5("Choose TF:"),
        selectInput("tfSelector", NULL,  c("", "HES7", "LYL1", "IRF5", "SPI1", "CEBPA", "ELK3", "RUNX1")),
        actionButton("randomChromLocsButton", "New region")
        )
     )

} # .createMainTab
#------------------------------------------------------------------------------------------------------------------------
.createBindingSitesManagerTab <- function()
{
   tabItem(tabName="bindingSitesManagerTab",
           fluidPage(id="bindingSitesManagerPage",
                     h3(id="bindingSitesManager_title", "Explore Binding Sites"),
                     h4(id="bindingSitesManager_currentTF", sprintf("TF: %s", "none yet specified")),
                     h4(id="bindingSitesManager_currentGenomicRegion", genomicRegionString),
                     textInput(inputId="textInput_exploreAnotherTF", label="Specify a new TF:"),
                     fluidRow(id="bindingSitesManagerPageContent")))

} # .createExperimentalTab
#------------------------------------------------------------------------------------------------------------------------
.createBody <- function()
{
   dashboardBody(
      includeCSS(system.file(package="TrenaViz", "css", "trenaViz.css")),
      useShinyjs(),
      extendShinyjs(script=system.file(package="TrenaViz", "js", "bindingSitesManager.js")),
      tabItems(
         .createMainTab(),
         .createBindingSitesManagerTab()
         ))

} # .createBody
#------------------------------------------------------------------------------------------------------------------------
ui <- dashboardPage(

   dashboardHeader(title="BindingSitesManager devel"),
   .createSidebar(),
   .createBody()

)
#------------------------------------------------------------------------------------------------------------------------
server <- function(session, input, output)
{
   addEventHandlers(bsm, session, input, output)


       #-----------------------------------------------------------------------------
       # for testing only.   button is added to the ui by devel_BindingSitesManager.R
       #-----------------------------------------------------------------------------

   printf("---- adding observer for randomChromLocsButton")
   observeEvent(input$randomChromLocsButton, ignoreInit=FALSE, {
      printf("setting new genomic regions")
      tbl.region <- data.frame(chrom="chr19",
                               start=1036002 + as.integer(100 * runif(1)),
                               end=1142642   +  as.integer(100 * runif(1)),
                               stringsAsFactors=FALSE)
      setGenomicRegion(bsm, tbl.region)
      })

} # server
#------------------------------------------------------------------------------------------------------------------------
app <- shinyApp(ui, server)

