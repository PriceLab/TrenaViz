library(shiny)
library(shinydashboard)
#------------------------------------------------------------------------------------------------------------------------
.createSidebar <- function()
{
   dashboardSidebar(
   sidebarMenu(id="sidebarMenu",
       menuItem("main",        tabName = "mainTab"),
       menuItem("experimental tab",   tabName = "experimentalTab")
       )
    )

} # .createSidebar
#------------------------------------------------------------------------------------------------------------------------
.createExperimentalTab <- function()
{
   tabItem(tabName="experimentalTab", h4("experimental tab"))

} # .createExperimentalTab
#------------------------------------------------------------------------------------------------------------------------
.createBody <- function()
{
   dashboardBody(
      tabItems(
         tabItem(tabName="mainTab", h4("main tab")),
         .createExperimentalTab()
         ))

} # .createBody
#------------------------------------------------------------------------------------------------------------------------
ui <- dashboardPage(

  dashboardHeader(title="Dashboard Template"),
  .createSidebar(),
  .createBody()

)
#------------------------------------------------------------------------------------------------------------------------
server <- function(input, output){
}
#------------------------------------------------------------------------------------------------------------------------
app <- shinyApp(ui, server)

