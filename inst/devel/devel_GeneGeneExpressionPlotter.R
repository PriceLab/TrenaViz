library(shiny)
library(shinydashboard)
library(TrenaViz)
library(shinyjs)
#------------------------------------------------------------------------------------------------------------------------
plotter <- GeneGeneExpressionPlotter()
#------------------------------------------------------------------------------------------------------------------------
.createSidebar <- function()
{
   dashboardSidebar(
   sidebarMenu(id="sidebarMenu",
       menuItem("main",                    tabName = "mainTab"),
       menuItem("Plot Expression",         tabName = "expressionPlotTab")
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
        h5("Choose TargetGene:"),
        selectInput("targetGeneSelector", NULL,  c("", "HES7", "LYL1", "IRF5", "SPI1", "CEBPA", "ELK3", "RUNX1")),
        h5("Choose Expression Matrix:"),
        selectInput("expressionMatrixSelector", NULL, c("", "AAAA", "BBB"))

        )
     )

} # .createMainTab
#------------------------------------------------------------------------------------------------------------------------
.createExpressionPlotterTab <- function()
{
   tabItem(tabName="expressionPlotTab",
           fluidPage(id="expressionPlotPage",
                     h3(id="expressionPlot_title", "Explore Correlated Gene Expression")))

} # .createExpressionPlotterTab
#------------------------------------------------------------------------------------------------------------------------
.createBody <- function()
{
   dashboardBody(
      includeCSS(system.file(package="TrenaViz", "css", "trenaViz.css")),
      useShinyjs(),
      # extendShinyjs(script=system.file(package="TrenaViz", "js", "bindingSitesManager.js")),
      tabItems(
         .createMainTab(),
         .createExpressionPlotterTab()
         ))

} # .createBody
#------------------------------------------------------------------------------------------------------------------------
ui <- dashboardPage(

   dashboardHeader(title="GeneGeneExpressionPlotter devel"),
   .createSidebar(),
   .createBody()

)
#------------------------------------------------------------------------------------------------------------------------
server <- function(session, input, output)
{

   addEventHandlers(plotter, session, input, output)

       #-----------------------------------------------------------------------------
       # for testing only.   button is added to the ui by devel_GeneGeneExpressionPlotter.R
       #-----------------------------------------------------------------------------

} # server
#------------------------------------------------------------------------------------------------------------------------
runApp(shinyApp(ui, server))

