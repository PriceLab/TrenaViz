library(shiny)
library(shinydashboard)
library(TrenaViz)
library(shinyjs)
#------------------------------------------------------------------------------------------------------------------------
moduleExample <- ModuleExample("hello trenaViz!")
#------------------------------------------------------------------------------------------------------------------------
.createSidebar <- function()
{
   dashboardSidebar(
   sidebarMenu(id="sidebarMenu",
       menuItem("main",              tabName = "mainTab"),
       menuItem("Module Example",      tabName = "moduleExampleTab")
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
        actionButton("viewModuleExampleButton", "View ModuleExample")
        )
     )

} # .createMainTab
#------------------------------------------------------------------------------------------------------------------------
.createModuleExampleTab <- function()
{
   printf("creating moduleExampleTab")
   tabItem(tabName="moduleExampleTab",
           createPage(moduleExample))

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
   printf("calling moduleExample::addEventHandlers")
   addEventHandlers(moduleExample, session, input, output)

} # server
#------------------------------------------------------------------------------------------------------------------------
runApp(shinyApp(ui, server))

