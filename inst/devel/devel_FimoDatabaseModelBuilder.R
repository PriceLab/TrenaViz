library(shiny)
library(shinydashboard)
library(TrenaViz)
library(shinyjs)
library(TrenaProjectErythropoiesis)
tp <- TrenaProjectErythropoiesis()

#------------------------------------------------------------------------------------------------------------------------
currentGenomicRegion <- list(chrom="chr3", start=128474310, end=128505841)  # the two main enhancer/promotres, total of 31kb
tbls.regulatoryRegions <- get(load("tbls.regulatoryRegions.RData"))
targetGene <- "GATA2"
fimoDatabaseModelBuilder <- FimoDatabaseModelBuilder(tp, targetGene, currentGenomicRegion, tbls.regulatoryRegions)
#------------------------------------------------------------------------------------------------------------------------
.createSidebar <- function()
{
   dashboardSidebar(
   sidebarMenu(id="sidebarMenu",
       menuItem("main",                   tabName = "mainTab"),
       menuItem("Fimo DB Model Builder",  tabName = "fimoDatabaseModelBuilderTab")
       )
    )

} # .createSidebar
#------------------------------------------------------------------------------------------------------------------------
.createMainTab <- function()
{
  tabItem(tabName="mainTab",
     div(
        h5("Choose Greeting:"),
        selectInput("messageSelector", NULL,  c("", "hello world!", "huy sqebeqsed!")),
        actionButton("viewFimoDatabaseModelBuilderButton", "View FimoDatabaseModelBuilder")
        )
     )

} # .createMainTab
#------------------------------------------------------------------------------------------------------------------------
.createFimoDatabaseModelBuilderTab <- function()
{
   printf("creating fimoDatabaseModelBuilderTab")
   tabItem(tabName="fimoDatabaseModelBuilderTab",
           fluidPage(id="FimoDatabaseModelBuilderPage",
                     h3(id="fimoDatabaseBuild_title", "Build Gene Regulatory Model Using Fimo Database"),
                     fluidRow(id="FimoDatabaseModelBuilderPageContent")))

} # .createFimoDatabaseModelBuilderTab
#------------------------------------------------------------------------------------------------------------------------
.createBody <- function()
{
   dashboardBody(
      includeCSS(system.file(package="TrenaViz", "css", "trenaViz.css")),
      includeCSS("fimoDatabaseModelBuilder.css"),
      useShinyjs(),
      #extendShinyjs(script=system.file(package="TrenaViz", "js", "networkView.js")),
      tabItems(
         .createMainTab(),
         .createFimoDatabaseModelBuilderTab()
         ))

} # .createBody
#------------------------------------------------------------------------------------------------------------------------
ui <- dashboardPage(
   dashboardHeader(title="Fimo Database Model Builder devel"),
   .createSidebar(),
   .createBody()
   )
#------------------------------------------------------------------------------------------------------------------------
server <- function(session, input, output)
{
   printf("calling fimoDatabaseModelBuilder::addEventHandlers")
   addEventHandlers(fimoDatabaseModelBuilder, session, input, output)

} # server
#------------------------------------------------------------------------------------------------------------------------
runApp(shinyApp(ui, server))

