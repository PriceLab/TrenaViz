library(shiny)
library(shinydashboard)
library(TrenaViz)
library(shinyjs)
#------------------------------------------------------------------------------------------------------------------------
bsm <- BindingSitesManager("Hsapiens", "hg38")
tbl.regions <- data.frame(chrom="chr19", start=1036002, end=1042642, stringsAsFactors=FALSE)
setGenomicRegion(bsm, tbl.regions)
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
        selectInput("tfSelector", NULL,  c("", "HES7", "LYL1", "IRF5", "SPI1", "CEBPA", "ELK3", "RUNX1"))
        )
     )

} # .createMainTab
#------------------------------------------------------------------------------------------------------------------------
.createBindingSitesManagerTab <- function()
{
   tabItem(tabName="bindingSitesManagerTab",
           fluidPage(id="bindingSitesManagerPage",
                     h4("Binding Sites Manager"),
                     fluidRow(id="bindingSitesManagerPageContent")))

} # .createExperimentalTab
#------------------------------------------------------------------------------------------------------------------------
.createBody <- function()
{
   dashboardBody(
      tabItems(
         .createMainTab(),
         .createBindingSitesManagerTab()
         ))

} # .createBody
#------------------------------------------------------------------------------------------------------------------------
ui <- dashboardPage(

   dashboardHeader(title="BindingSitesManager devel"),
   .createSidebar(),
   .createBody(),
   useShinyjs()

)
#------------------------------------------------------------------------------------------------------------------------
server <- function(session, input, output)
{
   addEventHandlers(bsm, session, input, output)

} # server
#------------------------------------------------------------------------------------------------------------------------
app <- shinyApp(ui, server)

