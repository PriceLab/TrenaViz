#' import shiny
#' @name BindingSitesManager
#' @rdname BindingSitesManager
#' @aliases BindingSitesManager
#------------------------------------------------------------------------------------------------------------------------
.BindingSitesManager <- setClass ("BindingSitesManager",
                                  representation = representation(
                                     TF="character",
                                     organism="character",
                                     genome="character",
                                     quiet="logical")
                                  )
#------------------------------------------------------------------------------------------------------------------------
setGeneric("createPage",    signature="obj", function(obj) standardGeneric("createPage"))
setGeneric("renderLogos",   signature="obj", function(obj, tfMappingOption) standardGeneric("renderLogos"))
#------------------------------------------------------------------------------------------------------------------------
#' Create an BindingSitesManager object
#'
#' @description
#' a shiny app
#'
#' @rdname BindingSitesManager
#'
#' @param TF A geneSymbol string identifying a transcription factor
#' @param organism  A character string, one of the supported species names:  hsapiens, mmuscuulus
#' @param genome  A character string, one of the supported genome builds: hg38, mm10
#' @param quiet A logical indicating whether or not the Trena object should print output
#'
#' @return An object of the BindingSitesManager class
#'
#' @export
#'
BindingSitesManager <- function(tf, organism, genome, quiet=TRUE)
{
   obj <- .BindingSitesManager(TF=tf, organism=organism, genome=genome, quiet=quiet)

   obj

} # BindingSitesManager
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
               column(1, offset=4, h2(obj@TF))),
            br(),
            fluidRow(
                column(3, offset=3,
                       radioButtons("tfMotifMappingOptions", "TF-Motif Mapping Options",
                                          c("MotifDb", "TFClass", "both"), selected="MotifDb")),
                column(1, actionButton("displayMotifsButton", "Display Motifs")),
                column(1, actionButton("displayTrackButton", "Display Track"))),
            fluidRow(id="motifPlottingRow",
                plotOutput(outputId="motifRenderingPanel", height="1000px")))
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

      tf <- obj@TF
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
      all.pwms <- MotifDb[pwm.names.unique]
      printf("launching ggseqlogo, %d matrices", length(all.pwms))
      show("motifRenderingPanel")
      ggseqlogo(lapply(all.pwms, function(pwm) pwm))
      })

#------------------------------------------------------------------------------------------------------------------------
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
