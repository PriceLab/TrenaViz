#' import shiny
#' @name BindingSitesManager
#' @rdname BindingSitesManager
#' @aliases BindingSitesManager
#------------------------------------------------------------------------------------------------------------------------
.BindingSitesManager <- setClass ("BindingSitesManager",
                                  representation = representation(
                                     quiet="logical",
                                     state="environment")
                                  )
#------------------------------------------------------------------------------------------------------------------------
setGeneric("addEventHandlers",    signature="obj", function(obj, session, input, output) standardGeneric("addEventHandlers"))
setGeneric("setOrganism",         signature="obj", function(obj, organism) standardGeneric("setOrganism"))
setGeneric("setGenome",           signature="obj", function(obj, genomeName) standardGeneric("setGenome"))
setGeneric("setGenomicRegion",    signature="obj", function(obj, tbl.region) standardGeneric("setGenomicRegion"))
setGeneric("setTF",               signature="obj", function(obj, tf) standardGeneric("setTF"))
setGeneric("createPage",          signature="obj", function(obj) standardGeneric("createPage"))
setGeneric("renderLogos",         signature="obj", function(obj, tfMappingOption) standardGeneric("renderLogos"))
setGeneric("removeLogos",         signature="obj", function(obj) standardGeneric("removeLogos"))
setGeneric("displayPage",         signature="obj", function(obj, tf) standardGeneric("displayPage"))
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
BindingSitesManager <- function(quiet=TRUE)
{
   state <- new.env(parent=emptyenv())
   state$TF <- NULL
   state$region <- data.frame(chrom="chr1", start=1, end=2, stringsAsFactors=FALSE)
   state$regionString <- "chr1:1-2"
   state$organism <- NULL
   state$genome <- NULL

   .BindingSitesManager(state=state,  quiet=quiet)

} # BindingSitesManager
#------------------------------------------------------------------------------------------------------------------------
#' specity the organism: Hsapiens, Mmusculus, ...
#'
#' @rdname setOrganism
#' @aliases setOrganism
#'
#' @param obj An object of class BindingSitesManager
#' @param organism character string: Hsapiens, Mmusculus
#'
#' @export
#'
setMethod("setOrganism", "BindingSitesManager",

     function(obj, organism) {
        stopifnot(organism %in% c("Hsapiens", "Mmusculus"))
        obj@state$organism <- organism
        })

#------------------------------------------------------------------------------------------------------------------------
#' specity the genome build: hg38, mm10, ...
#'
#' @rdname setGenome
#' @aliases setGenome
#'
#' @param obj An object of class BindingSitesManager
#' @param genome build character string: hg38, mm10, ...
#'
#' @export
#'
setMethod("setGenome", "BindingSitesManager",

     function(obj, genomeName) {
        obj@state$genome <- genomeName
        })

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
        js$setBindingSitesManagerPageTitle(tf)
          # disable some buttons
        shinyjs::disable("findMatchesButton")
        shinyjs::disable("displayTrackButton")
        js$clear_textInput_exploreAnotherTF()
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

     function(obj, tbl.region) {
        if(all(tbl.region == obj@state$region))
           return()
        new.roi <- with(tbl.region, sprintf("%s:%d-%d", chrom, start, end))
        new.region.requested <- !(all(tbl.region == obj@state$region))
        obj@state$region <- tbl.region
        if(new.region.requested){
           #printf("--- BindingSitesManager, new genomic region: %s", new.roi)
           obj@state$region <- tbl.region
           obj@state$regionString <- new.roi
           genomicRegionsString <- sprintf("%s  (%d bases)", new.roi, with(tbl.region, 1 + end - start))
           if(!is.null(js$setBindingSitesManagerGenomicRegionDisplay))  # not initialized on very early calls
              js$setBindingSitesManagerGenomicRegionDisplay(genomicRegionsString)
           } # if new
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
            extendShinyjs(script=system.file(package="TrenaViz", "js", "bindingSitesManager.js")),
            fluidRow(
               column(6, offset=2, h3(id="bindingSitesManagerPageTitle",
                                      sprintf("Explore Binding Sites for %s: %s", obj@state$TF, obj@state$regionString)))),
            br(),
            fluidRow(
               column(3,
                      radioButtons("tfMotifMappingOptions", "TF-Motif Mapping Options",
                                  c("MotifDb", "TFClass", "both"), selected="MotifDb", inline=TRUE),
                      actionButton("displayMotifsButton", "Display Motifs")),
               column(3,
                      selectInput("motifChooser", "Choose Motif", c()),
                      selectInput("matchAlgorithmChooser", "Choose Match Algorithm",
                                  c("Biostrings matchPWM", "MOODS matchMotifs"),
                                  selected="MOODS matchMotifs",
                                  selectize=FALSE)),

               column(4,
                      conditionalPanel(
                         condition = "input.matchAlgorithmChooser == 'Biostrings matchPWM'",
                         sliderInput("biostringsMatchThresholdSlider", "Match Threshold (0-1): ",
                                     min=0, max=1, value=0.9, step=0.01)),
                      conditionalPanel(
                         condition = "input.matchAlgorithmChooser == 'MOODS matchMotifs'",
                         sliderInput("moodsMatchThresholdSlider", "Match Threshold: (-log10(pVal))",
                                     min=0.0, max=8.0, value=4.0, step=0.1)),
                      fluidRow(column(width=3, actionButton("findMatchesButton", "Find Matches")),
                               column(width=2, offset=0, textOutput(outputId="motifMatchCountDisplay")),
                               #column(width=3, offset=2, verbatimTextOutput(outputId="motifMatchCountDisplay", placeholder=TRUE)),
                               column(width=3, actionButton("displayTrackButton", "Display Track")))
                      )), # column3, fluidRow
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
         motifNames.tfClass <- geneToMotif(MotifDb, tf, source="TFClass", ignore.case=TRUE)$motif
         printf("tfClass motifs: %d", length(motifNames.tfClass))
         if(length(motifNames.tfClass) > 0)
            pwms.tfClass.longNames <- names(query(MotifDb, obj@state$organism, motifNames.tfClass))
         } # if tfclass

      if("motifdb" %in% mappingOptions){
         pwms.motifDb <- c()
         motifNames.motifDb <- rownames(geneToMotif(MotifDb, tf, source="MotifDb", ignore.case=TRUE))
         printf("motifDb motifs: %d", length(motifNames.motifDb))
         }

      pwm.names.unique <- unique(c(pwms.tfClass.longNames, motifNames.motifDb))
       if(length(pwm.names.unique) == 0){
          showNotification("No motifs found with specified mapping")
          return()
          }
      printf("about to update motifChooser selectInput with %d motif names", length(pwm.names.unique))
      updateSelectInput(obj@state$session, "motifChooser", choices=pwm.names.unique, selected=character(0))
      #shinyjs::enable("displayTrackButton")
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
       #shinyjs::enable("motifChooser")
       #shinyjs::disable("findMatchesButton")
       #shinyjs::disable("displayTrackButton")
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
         printf("BindingSitesManager displayPage, tf: %s",   tf)
         #browser()
         setTF(obj, tf)
         removeUI(selector="#bindingSitesManagerPageContent", immediate=TRUE)
         insertUI(selector="#bindingSitesManagerPage", where="afterEnd", createPage(obj), immediate=TRUE)
         updateTabItems(obj@state$session, "sidebarMenu", select="bindingSitesManagerTab")
         removeLogos(obj)
         #printf("about to go red")
         #js$pageRed()
         #printf("after going red")
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

       js$installBindingSitesManagerReturnKeyHandlers()

       observeEvent(input$tfSelector, ignoreInit=TRUE, {
          tf <- input$tfSelector
          if(nchar(tf) == 0) return();
          displayPage(obj, tf)
          })

        observeEvent(input$displayMotifsButton, ignoreInit=TRUE, {
           #js$installReturnHandler()
           #js$pageRed()
           printf("-- about to enable and disable")
           output$motifRenderingPanel <- renderPlot({
              printf("observing displayMotifsButton")
              tfMapping <- isolate(input$tfMotifMappingOptions)
              xyz <- "just before render logos"
              renderLogos(obj, tfMapping)
              })
           })

        observeEvent(input$textInput_exploreAnotherTF_widget_returnKey, {
           new.tf <- isolate(input$textInput_exploreAnotherTF)
           printf("explore this new TF: %s", new.tf)
           if(nchar(new.tf) == 0) return();
           displayPage(obj, new.tf)
           })

        observeEvent(input$findMatchesButton, ignoreInit=TRUE, {
           motif <- isolate(input$motifChooser)
           print(obj@state$region)
           output$motifMatchCountDisplay <- renderText({"   "})
           sequenceMatchAlgorithm <- isolate(input$matchAlgorithmChooser)
           matchThreshold <- isolate(input$biostringsMatchThresholdSlider)
           if(sequenceMatchAlgorithm == "MOODS matchMotifs")
              matchThreshold <- isolate(input$moodsMatchThresholdSlider)
           motif.matrix <- as.list(MotifDb[motif])
           printf("--- matching motif against current sequence")
           printf("    %s", sequenceMatchAlgorithm)
           printf("    %f", matchThreshold)
           printf("    %s", motif)
           m4 <- MultiMethodMotifMatcher(obj@state$genome, motif.matrix, obj@state$region, sequenceMatchAlgorithm, matchThreshold)
           tbl.hits <- matchMotifInSequence(m4)
           rowCountAsString <- sprintf("%d", nrow(tbl.hits))
           printf("hits: %s", rowCountAsString)
           output$motifMatchCountDisplay <- renderText({rowCountAsString})
           if(nrow(tbl.hits) == 0)
              shinyjs::disable("displayTrackButton")
           else
              shinyjs::enable("displayTrackButton")
           obj@state$motifHits <- tbl.hits
           #mm <- MotifMatcher("hg38", as.list(pwm.oi), quiet=TRUE)
           # matchThreshold <- 80
           # tbl.matches <- findMatchesByChromosomalRegion(mm, tbl.region, pwmMatchMinimumAsPercentage=matchThreshold)
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

        observeEvent(input$motifChooser, ignoreInit=FALSE, {
           currentValue <- input$motifChooser
           printf("observer detects motifChooser event: '%s'", currentValue)
           if(nchar(currentValue) == 0){
              shinyjs::disable("findMatchesButton")
              shinyjs::disable("displayTrackButton")
              }
           else{
              shinyjs::enable("findMatchesButton")
              shinyjs::enable("displayTrackButton")
              }
           })

        observeEvent(input$displayTrackButton, ignoreInit=TRUE, {
           printf("display tracks")
           next.color <- "purple"
           print(head(obj@state$motifHits))
           tbl.bg <- obj@state$motifHits[, c("chrom", "start", "end", "score")]
           #tbl.bg$score <- as.character(tbl.bg$score)
           #tbl.bg$score <- as.integer(tbl.bg$score)
           print(tbl.bg)
           #loadBedTrack(session, sprintf("Bi-%s", obj@state$TF), tbl.bg, color=next.color, trackHeight=50)
           updateTabItems(session, "sidebarMenu", select="igvAndTable")
           later(function(){loadBedGraphTrack(obj@state$session, obj@state$TF, tbl.bg,
                                              color=next.color, trackHeight=50, autoscale=TRUE, quiet=FALSE)}, 1)
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
#       tbl.region <- with(chrom.loc, data.frame(chrom=chrom, start=start, end=end, stringsAsFactors=FALSE))
#       mm <- MotifMatcher("hg38", as.list(pwm.oi), quiet=TRUE)
#       matchThreshold <- 80
#       tbl.matches <- findMatchesByChromosomalRegion(mm, tbl.region, pwmMatchMinimumAsPercentage=matchThreshold)
#       if(nrow(tbl.matches) > 0){
#          tbl.tmp <- tbl.matches[, c("chrom", "motifStart", "motifEnd", "motifRelativeScore")]
#          colnames(tbl.tmp) <- c("chrom", "start", "end", "value")
#          state$colorNumber <- (state$colorNumber %% totalColorCount) + 1
#          next.color <- colors[state$colorNumber]
#          scale.bottom <- 0.9 * (matchThreshold/100)
#          loadBedGraphTrack(session, tf.name, tbl.tmp, color=next.color, trackHeight=25, autoscale=FALSE,
#                            min=scale.bottom, max=1.0)
#          }
