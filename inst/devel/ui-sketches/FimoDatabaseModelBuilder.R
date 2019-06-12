library(shiny)


#   the two main GATA2 enhancer/promotres, total of 31kb
tbl.region <- data.frame(chrom="chr3", start=128474310, end=128505841)
genomicRegionString <- with(tbl.region, sprintf("%s:%d-%d", chrom, start, end))


ui <- fluidPage(id="FimoDatabaseModelBuilderPageContent",
    includeCSS("../fimoDatabaseModelBuilder.css"),
    fluidRow(
        column(width=5, offset=1, id="fimoModelBuilderTitleBox",
          h4(id="fimoModeBuilder_title", "Build Gene Regulation Model"),
          h4(id="fimoModelBuilder_currentTargetene", sprintf("Target gene: %s", "GATA2")),
          h4(id="fimoModelBuilder_currentGenomicRegion", sprintf("In region: %s", genomicRegionString))
          )
        ),
    fluidRow(
       column(width=3,
          selectInput("trenaProjectSelector", "Project",  c("", "Erythropoiesis", "Lymphocyte")),
          selectInput("expressionMatrixSelector", "Expression Matrix",  c("", "mtx1", "mtx2-with-very-very-long-name"))),
        column(width=4,
          selectInput("tfbsTrackSelector", "Restrict TFs to those binding in track: ",
                      c("No restriction: all DNA in current region", "ATAC-seq", "GeneHancer")),
          sliderInput("fimoThresholdSelector", "FIMO motif match cutoff -log10(pVal)", 1, 10, value=4, step=0.1)
          ),
        column(width=5,
          sliderInput("modelSizeSelector", "Regulatory model max size", 5, 200, value=10, step=1),
          sliderInput("tfCorrelationThreshold", "Minimum TF/targetGene correlation", 0, 1, value=0.4, step=0.1)
               )
        ),
    fluidRow(
       column(width=2, offset=4,
          actionButton("calculateActionButton", "Build Regulatory Model")
          )),
    fluidRow(
       column(width=2, offset=4, id="fubar",
          actionButton("viewNewModelButton", "View")
          ))

    ) # fluidPage

server <- function(input, output){}

runApp(shinyApp(ui, server))


