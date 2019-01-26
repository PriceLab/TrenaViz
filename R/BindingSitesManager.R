#' import shiny
#' @name BindingSitesManager
#' @rdname BindingSitesManager
#' @aliases BindingSitesManager
#------------------------------------------------------------------------------------------------------------------------
.BindingSitesManager <- setClass ("BindingSitesManager",
                                  representation = representation(
                                     organism="character",
                                     genome="character",
                                     quiet="logical",
                                     state="environment")
                                  )
#------------------------------------------------------------------------------------------------------------------------
setGeneric("addEventHandlers",    signature="obj", function(obj, session, input, output) standardGeneric("addEventHandlers"))
setGeneric("setTF",               signature="obj", function(obj, tf) standardGeneric("setTF"))
setGeneric("createPage",          signature="obj", function(obj) standardGeneric("createPage"))
setGeneric("renderLogos",         signature="obj", function(obj, tfMappingOption) standardGeneric("renderLogos"))
setGeneric("removeLogos",         signature="obj", function(obj) standardGeneric("removeLogos"))
setGeneric("displayPage",         signature="obj", function(obj, tf) standardGeneric("displayPage"))
setGeneric("setGenomicRegion",    signature="obj", function(obj, tbl.regions) standardGeneric("setGenomicRegion"))
#------------------------------------------------------------------------------------------------------------------------
#' Create an BindingSitesManager object
#'
#' @description
#' a shiny app
#'
#' @rdname BindingSitesManager
#'
#' @param organism  A character string, one of the supported species names:  hsapiens, mmuscuulus
#' @param genome  A character string, one of the supported genome builds: hg38, mm10
#' @param quiet A logical indicating whether or not the Trena object should print output
#'
#' @return An object of the BindingSitesManager class
#'
#' @export
#'
BindingSitesManager <- function(organism, genome, quiet=TRUE)
{
   state <- new.env(parent=emptyenv())
   state$TF <- NULL
   state$regions <- data.frame()

   obj <- .BindingSitesManager(organism=organism, genome=genome, quiet=quiet, state=state)

   obj

} # BindingSitesManager
#------------------------------------------------------------------------------------------------------------------------
#' specity the tf (transcription factor) to work on
#'
#' @rdname setTF
#' @aliases setTF
#'
#' @param obj An object of class BindingSitesManager
#' @param TF character string, the transcription factor we are currently concerned with
#'
#' @export
#'
setMethod("setTF", "BindingSitesManager",

          function(obj, tf) {
             obj@state$TF <- tf
             })

#------------------------------------------------------------------------------------------------------------------------
#' specify the one or more genomic regions in which to look for binding sites
#'
#' @rdname setGenomicRegion
#' @aliases setGenomicRegion
#'
#' @param obj An object of class BindingSitesManager
#' @param TF character string, the transcription factor we are currently concerned with
#'
#' @export
#'
setMethod("setGenomicRegion", "BindingSitesManager",

          function(obj, tbl.regions) {
             obj@state$regions <- tbl.regions
             })

#------------------------------------------------------------------------------------------------------------------------
#' create and return the control-rich UI
#'
#' @rdname createPage
#' @aliases createPage
#'
#' @param obj An object of class BindingSitesManager
#'
#' @export
#'
setMethod("createPage", "BindingSitesManager",

     function(obj) {
        div(id="bindingSitesManagerPageContent",
            fluidRow(
               column(1, offset=4, h2(obj@state$TF))),
            br(),
            fluidRow(
               column(2, offset=1,
                      radioButtons("tfMotifMappingOptions", "TF-Motif Mapping Options",
                                  c("MotifDb", "TFClass", "both"), selected="MotifDb", inline=TRUE),
                      actionButton("displayMotifsButton", "Display Motifs")),
               column(3,
                      selectInput("motifChooser", "Choose Motif", c()),
                      selectInput("matchAlgorithmChooser", "Choose Match Algorithm",
                                  c("Biostrings matchPWM", "MOODS matchMotifs"),
                                  selected="MOODS matchMotifs",
                                  selectize=FALSE)),

               column(3,
                      conditionalPanel(
                         condition = "input.matchAlgorithmChooser == 'Biostrings matchPWM'",
                         sliderInput("matchThresholdSlider", "Match Threshold (0-1): ", min=0, max=1, value=0.9)),
                      conditionalPanel(
                         condition = "input.matchAlgorithmChooser == 'MOODS matchMotifs'",
                         sliderInput("matchThresholdSlider", "Match Threshold: (-log10(pVal))", min=0, max=10, value=3)),
                      fluidRow(actionButton("findMatchesButton", "Find Matches"),
                               actionButton("displayTrackButton", "Display Track")))
               ),
            fluidRow(id="motifPlottingRow",
                     plotOutput(outputId="motifRenderingPanel", height="1000px"))
            )
       })

#------------------------------------------------------------------------------------------------------------------------
#' show the control-rich UI
#'
#' @rdname renderLogos
#' @aliases renderLogos
#'
#' @param obj An object of class BindingSitesManager
#'
#' @export
#'
setMethod("renderLogos", "BindingSitesManager",

    function(obj, tfMappingOption){

      tf <- obj@state$TF
      mappingOptions <- tolower(tfMappingOption)
      if(tfMappingOption == "both")
        mappingOptions <- c("motifdb", "tfclass")

      pwms.tfClass.longNames <- c()
      motifNames.motifDb <- c()

      if("tfclass" %in% mappingOptions){
         motifNames.tfClass <- geneToMotif(MotifDb, tf, source="TFClass")$motif
         printf("tfClass motifs: %d", length(motifNames.tfClass))
         if(length(motifNames.tfClass) > 0)
            pwms.tfClass.longNames <- names(query(MotifDb, obj@organism, motifNames.tfClass))
         } # if tfclass

      if("motifdb" %in% mappingOptions){
         pwms.motifDb <- c()
         motifNames.motifDb <- rownames(geneToMotif(MotifDb, tf, source="MotifDb"))
         printf("motifDb motifs: %d", length(motifNames.motifDb))
         }

      pwm.names.unique <- unique(c(pwms.tfClass.longNames, motifNames.motifDb))
       if(length(pwm.names.unique) == 0){
          showNotification("No motifs found with specified mapping")
          return()
          }
      shinyjs::enable("motifChooser")
      updateSelectInput(obj@state$session, "motifChooser", choices=pwm.names.unique, selected=character(0))
      shinyjs::enable("displayTrackButton")
      all.pwms <- MotifDb[pwm.names.unique]
      printf("launching ggseqlogo, %d matrices", length(all.pwms))
      show("BindingSitesManager: renderLogos, about to call ggseqlogo")
      ggseqlogo(lapply(all.pwms, function(pwm) pwm))
      })

#------------------------------------------------------------------------------------------------------------------------
#' remove the image
#'
#' @rdname removeLogos
#' @aliases removeLogos
#'
#' @param obj An object of class BindingSitesManager
#'
#' @export
#'
setMethod("removeLogos", "BindingSitesManager",

    function(obj){
       shinyjs::enable("motifChooser")
       shinyjs::disable("displayTrackButton")
       removeUI(selector="#motifRenderingPanel > img", immediate=TRUE)
       })

#------------------------------------------------------------------------------------------------------------------------
#' display the page
#'
#' @rdname displayPage
#' @aliases displayPage
#'
#' @param obj An object of class BindingSitesManager
#' @param tf  character string, the geneSymbol name of the transcription factor
#'
#' @export
#'
setMethod("displayPage", "BindingSitesManager",

     function(obj, tf){
         removeLogos(obj)
         printf("tf: %s",   tf)
         setTF(obj, tf)
         removeUI(selector="#bindingSitesManagerPageContent", immediate=TRUE)
         insertUI(selector="#bindingSitesManagerPage", where="afterEnd", createPage(obj), immediate=TRUE)
         updateTabItems(obj@state$session, "sidebarMenu", select="bindingSitesManagerTab")
         removeLogos(obj)
         })
#------------------------------------------------------------------------------------------------------------------------
#' add shiny event handlers
#'
#' @rdname addEventHandlers
#' @aliases addEventHandlers
#'
#' @param obj An object of class BindingSitesManager
#' @param session a Shiny session object
#' @param input a Shiny input object
#' @param output a Shiny output object
#'
#' @export
#'
setMethod("addEventHandlers", "BindingSitesManager",

     function(obj, session, input, output){

       obj@state$session <- session
       obj@state$input <- input
       obj@state$output <- output

       observeEvent(input$clearLogosButton, ignoreInit=TRUE, {
          removeLogos(obj)
          })

        observeEvent(input$tfSelector, ignoreInit=TRUE, {
           tf <- input$tfSelector
           if(nchar(tf) == 0) return();
           displayPage(obj, tf)
           # removeLogos(obj)
           # if(nchar(tf) == 0) return();
           # printf("tf: %s",   tf)
           # setTF(obj, tf)
           # removeUI(selector="#bindingSitesManagerPageContent", immediate=TRUE)
           # insertUI(selector="#bindingSitesManagerPage", where="afterEnd", createPage(obj), immediate=TRUE)
           # updateTabItems(session, "sidebarMenu", select="bindingSitesManagerTab")
           # removeLogos(obj)
           })

        observeEvent(input$displayMotifsButton, ignoreInit=TRUE, {
            output$motifRenderingPanel <- renderPlot({
               printf("observing displayMotifsButton")
               tfMapping <- isolate(input$tfMotifMappingOptions)
               xyz <- "just before render logos"
               renderLogos(obj, tfMapping)
               })
          })

        observeEvent(input$findMatchesButton, ignoreInit=TRUE, {
           motif <- isolate(input$motifChooser)
           sequenceMatchAlgorithm <- isolate(input$matchAlgorithmChooser)
           matchThreshold <- isolate(input$matchThresholdSlider)
           motif.matrix <- as.list(MotifDb[motif])
           m4 <- MultiMethodMotifMatcher(obj@genome, motif.matrix, obj@state$regions, sequenceMatchAlgorithm, matchThreshold)
           tbl.hits <- matchMotifInSequence(m4)
           print(tbl.hits)
           #mm <- MotifMatcher("hg38", as.list(pwm.oi), quiet=TRUE)
           # matchThreshold <- 80
           # tbl.matches <- findMatchesByChromosomalRegion(mm, tbl.regions, pwmMatchMinimumAsPercentage=matchThreshold)
           # if(nrow(tbl.matches) > 0){
           #    tbl.tmp <- tbl.matches[, c("chrom", "motifStart", "motifEnd", "motifRelativeScore")]
           #    colnames(tbl.tmp) <- c("chrom", "start", "end", "value")
           #    state$colorNumber <- (state$colorNumber %% totalColorCount) + 1
           #    next.color <- colors[state$colorNumber]
           #    scale.bottom <- 0.9 * (matchThreshold/100)
           #    loadBedGraphTrack(session, tf.name, tbl.tmp, color=next.color, trackHeight=25, autoscale=FALSE,
           #    min=scale.bottom, max=1.0)
           #    }
           })
        observeEvent(input$displayTrackButton, ignoreInit=TRUE, {
            printf("display tracks")
            })

     }) # addEventHandlers

#------------------------------------------------------------------------------------------------------------------------
#          signature="obj", function(obj, session, input, output) standardGeneric("createPage"))
#
#       dialog <- bindingSitesOptionsDialog(tf.name)
#       showModal(dialog)
#       motifNames.tfClass <- geneToMotif(MotifDb, tf.name, source="TFClass")$motif
#       pwms.tfClass <- query(MotifDb, "", motifNames.tfClass)
#       motifNames.motifDb <- geneToMotif(MotifDb, tf.name, source="MotifDb")
#       full.motif.names <- rownames(geneToMotif(MotifDb, tf.name, source="MotifDb"))
#       pwms.motifDb <- MotifDb[full.motif.names]
#       pwm.name.oi <- c(names(pwms.tfClass), names(pwms.motifDb))[1]
#       pwm.oi <- MotifDb[pwm.name.oi]
#       tbl.regions <- with(chrom.loc, data.frame(chrom=chrom, start=start, end=end, stringsAsFactors=FALSE))
#       mm <- MotifMatcher("hg38", as.list(pwm.oi), quiet=TRUE)
#       matchThreshold <- 80
#       tbl.matches <- findMatchesByChromosomalRegion(mm, tbl.regions, pwmMatchMinimumAsPercentage=matchThreshold)
#       if(nrow(tbl.matches) > 0){
#          tbl.tmp <- tbl.matches[, c("chrom", "motifStart", "motifEnd", "motifRelativeScore")]
#          colnames(tbl.tmp) <- c("chrom", "start", "end", "value")
#          state$colorNumber <- (state$colorNumber %% totalColorCount) + 1
#          next.color <- colors[state$colorNumber]
#          scale.bottom <- 0.9 * (matchThreshold/100)
#          loadBedGraphTrack(session, tf.name, tbl.tmp, color=next.color, trackHeight=25, autoscale=FALSE,
#                            min=scale.bottom, max=1.0)
#          }
