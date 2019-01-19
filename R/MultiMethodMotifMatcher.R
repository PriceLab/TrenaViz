#' import shiny
#' @name MultiMethodMotifMatcher
#' @rdname MultiMethodMotifMatcher
#' @aliases MultiMethodMotifMatcher
#------------------------------------------------------------------------------------------------------------------------
.MultiMethodMotifMatcher <- setClass ("MultiMethodMotifMatcher",
                                  representation = representation(
                                     organism="character",
                                     genome="character",
                                     motif="character",
                                     sequenceMatchAlgorithm="character",
                                     matchThreshold="numeric",
                                     quiet="logical")
                                  )
#------------------------------------------------------------------------------------------------------------------------
setGeneric("match",  signature="obj", function(obj, session, input, output) standardGeneric("match"))
#------------------------------------------------------------------------------------------------------------------------
#' Create an MultiMethodMotifMatcher object
#'
#' @description
#' a shiny app
#'
#' @rdname MultiMethodMotifMatcher
#'
#' @param organism  A character string, one of the supported species names:  hsapiens, mmuscuulus
#' @param genome  A character string, one of the supported genome builds: hg38, mm10
#' @param quiet A logical indicating whether or not the Trena object should print output
#'
#' @return An object of the MultiMethodMotifMatcher class
#'
#' @export
#'
MultiMethodMotifMatcher <- function(genome, motif, sequenceMatchAlgorithm, matchThreshold, quiet=TRUE)
{

   obj <- .MultiMethodMotifMatcher(genome=genome, motif=motif,
                                   sequenceMatchAlgorithm=sequenceMatchAlgorithm,
                                   matchThreshold=matchThreshold, quiet=quiet)

   obj

} # MultiMethodMotifMatcher
#------------------------------------------------------------------------------------------------------------------------
