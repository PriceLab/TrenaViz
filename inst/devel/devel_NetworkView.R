library(shiny)
library(shinydashboard)
library(TrenaViz)
library(shinyjs)
library(cyjShiny)
#------------------------------------------------------------------------------------------------------------------------
f <- system.file(package="TrenaViz", "extdata", "model.and.regRegions.irf4.top5.RData")
load(f)
networkView <- NetworkView("IRF4", 391739, tbl.model, tbl.reg)
#load("gata2.model.RData")
#networkView <- NetworkView("GATA2", tbl.model, tbl.regions, mtx)
#------------------------------------------------------------------------------------------------------------------------
.createSidebar <- function()
{
   dashboardSidebar(
   sidebarMenu(id="sidebarMenu",
       menuItem("main",              tabName = "mainTab"),
       menuItem("Network View",      tabName = "networkViewTab")
       )
    )

} # .createSidebar
#------------------------------------------------------------------------------------------------------------------------
.createMainTab <- function()
{
  tabItem(tabName="mainTab",
     div(
        h5("Choose Regulatory Model:"),
        selectInput("tfSelector", NULL,  c("", "GATA2")),
        actionButton("viewNetworkButton", "View Network")
        )
     )

} # .createMainTab
#------------------------------------------------------------------------------------------------------------------------
.createNetworkViewTab <- function()
{
   printf("creating networkViewTab")
   tabItem(tabName="networkViewTab",
           createPage(networkView))

} # .createNetworkViewTab
#------------------------------------------------------------------------------------------------------------------------
.createBody <- function()
{
   dashboardBody(
      includeCSS(system.file(package="TrenaViz", "css", "trenaViz.css")),
      includeCSS("cyjShiny.css"),
      useShinyjs(),
      extendShinyjs(script=system.file(package="TrenaViz", "js", "networkView.js")),
      tabItems(
         .createMainTab(),
         .createNetworkViewTab()
         ))

} # .createBody
#------------------------------------------------------------------------------------------------------------------------
ui <- dashboardPage(
   dashboardHeader(title="NetworkView devel"),
   .createSidebar(),
   .createBody()
   )
#------------------------------------------------------------------------------------------------------------------------
server <- function(session, input, output)
{
   printf("calling networkView::addEventHandlers")
   addEventHandlers(networkView, session, input, output)

} # server
#------------------------------------------------------------------------------------------------------------------------
runApp(shinyApp(ui, server))

