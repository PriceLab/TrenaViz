library(shiny)
library(shinydashboard)
library(TrenaViz)
library(shinyjs)
library(cyjShiny)
#------------------------------------------------------------------------------------------------------------------------
load("gata2.model.RData")
networkView <- NetworkView("GATA2", tbl.model, tbl.regions, mtx)
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
   tabItem(tabName="networkViewTab",
           fluidPage(id="networkViewPage",
                     fluidRow(id="networkViewPageContent")))
       #    column(width=12,cyjShinyOutput('cyjShiny', height=400)))))

} # .createNetworkViewTab
#------------------------------------------------------------------------------------------------------------------------
.createBody <- function()
{
   dashboardBody(
      includeCSS(system.file(package="TrenaViz", "css", "trenaViz.css")),
      includeCSS("cyjShiny.css"),
      useShinyjs(),
      # extendShinyjs(script=system.file(package="TrenaViz", "js", "networkView.js")),
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

       #-----------------------------------------------------------------------------
       # for testing only.   button is added to the ui by devel_NetworkView.R
       #-----------------------------------------------------------------------------


    output$cyjShiny <- renderCyjShiny({
       cyjShiny(graph=graph.json, layoutName="preset")

       })


} # server
#------------------------------------------------------------------------------------------------------------------------
runApp(shinyApp(ui, server))

