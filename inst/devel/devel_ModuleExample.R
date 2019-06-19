library(shiny)
library(shinydashboard)
library(TrenaViz)
library(shinyjs)
library(TrenaProjectErythropoiesis)
#------------------------------------------------------------------------------------------------------------------------
trenaProject <- TrenaProjectErythropoiesis()
setTargetGene(trenaProject, "GATA2")
moduleExample <- ModuleExample()
setTrenaProject(moduleExample, trenaProject)
#------------------------------------------------------------------------------------------------------------------------
.createSidebar <- function()
{
   dashboardSidebar(
      sidebarMenu(id="sidebarMenu",
         menuItem("main",              tabName = "mainTab"),
         menuItem("Module Example",    tabName = "moduleExampleTab"),
         actionButton("updateGenomicRegionButton", "Update Genomic Region")
         )
      )

} # .createSidebar
#------------------------------------------------------------------------------------------------------------------------
.createMainTab <- function()
{
  tabItem(tabName="mainTab",
     div(
        h4("This is a standin for the main tab of TrenaViz")
        )
     )

} # .createMainTab
#------------------------------------------------------------------------------------------------------------------------
.createModuleExampleTab <- function()
{
   printf("creating moduleExampleTab")
   tabItem(tabName="moduleExampleTab",
           fluidPage(id="ModuleExamplePage",
                     h3(id="moduleExample_title", "Module Example"),
                     fluidRow(id="ModuleExamplePageContent")))

   #printf("trying to hide the messageDisplayWidget")
   #shinyjs::hide("messageDisplayWidget")
   #printf("after trying to hide the messageDisplayWidget")

} # .createModuleExampleTab
#------------------------------------------------------------------------------------------------------------------------
.createBody <- function()
{
   dashboardBody(
      includeCSS(system.file(package="TrenaViz", "css", "trenaViz.css")),
      includeCSS("moduleExample.css"),
      useShinyjs(),
      #extendShinyjs(script=system.file(package="TrenaViz", "js", "networkView.js")),
      tabItems(
         .createMainTab(),
         .createModuleExampleTab()
         ))

} # .createBody
#------------------------------------------------------------------------------------------------------------------------
ui <- dashboardPage(
   dashboardHeader(title="Module Example devel"),
   .createSidebar(),
   .createBody()
   )
#------------------------------------------------------------------------------------------------------------------------
server <- function(session, input, output)
{
   observeEvent(input$updateGenomicRegionButton, ignoreInit=TRUE, {
      chroms <- sort(sample(paste("chr", c(as.character(1:22), "X", "Y"), sep="")))
      chrom <- chroms[sample(1:24, 1)]
      start <- sample(1:50000000, 1)
      end <- start + sample(1:50000000, 1)
      tbl.region <- data.frame(chrom=chrom, start=start, end=end, stringsAsFactors=FALSE)
      setGenomicRegion(moduleExample, tbl.region)
      })

   observeEvent(input$sidebarMenu, ignoreInit=TRUE, {
      tabName <- input$sidebarMenu
      if(tabName == "moduleExampleTab"){
         printf("--- about to call displayPage(moduleExample)")
         displayPage(moduleExample)
         addEventHandlers(moduleExample, session, input, output)
         }
      })

} # server
#------------------------------------------------------------------------------------------------------------------------
runApp(shinyApp(ui, server))

