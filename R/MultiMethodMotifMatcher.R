#' import trena
#' import motifmatchr
#' import universalmotif
#' import TFBSTools
#'
#' @name MultiMethodMotifMatcher
#' @rdname MultiMethodMotifMatcher
#' @aliases MultiMethodMotifMatcher
#------------------------------------------------------------------------------------------------------------------------
.MultiMethodMotifMatcher <- setClass ("MultiMethodMotifMatcher",
                                  representation = representation(
                                     genome="character",
                                     motifMatrix="list",    # a one-element named list, a matrix with a matrix name
                                     regions="data.frame",
                                     sequenceMatchAlgorithm="character",
                                     matchThreshold="numeric",
                                     quiet="logical")
                                  )
#------------------------------------------------------------------------------------------------------------------------
setGeneric("match",  signature="obj", function(obj, session, input, output) standardGeneric("match"))
#------------------------------------------------------------------------------------------------------------------------
#' Create a MultiMethodMotifMatcher object
#'
#' @description
#' provides unified access to a set of motif/DNA sequence matching algorithms
#'
#' @rdname MultiMethodMotifMatcher
#'
#' @param genome  A character string, one of the supported genome builds: hg38, mm10
#' @param motifMatrix   A position frequency matrix
#' @param regions A data.frame with chrom, start, end columns
#' @param sequenceMatchAlgorithm A character string, either "Biostrings matchPWM" or "MOODS matchMotifs"
#' @param matchThreshold numeric, between 0 and 1 (usually 0.7 or higher)
#' @param quiet A logical indicating whether or not the Trena object should print output
#'
#' @return An object of the MultiMethodMotifMatcher class
#'
#' @export
#'
MultiMethodMotifMatcher <- function(genome, motifMatrix, tbl.regions, sequenceMatchAlgorithm, matchThreshold, quiet=TRUE)
{

   stopifnot(sequenceMatchAlgorithm %in% c("Biostrings matchPWM", "MOODS matchMotifs"))
   obj <- .MultiMethodMotifMatcher(genome=genome,
                                   motifMatrix=motifMatrix,
                                   regions=tbl.regions,
                                   sequenceMatchAlgorithm=sequenceMatchAlgorithm,
                                   matchThreshold=matchThreshold, quiet=quiet)

   obj

} # MultiMethodMotifMatcher
#------------------------------------------------------------------------------------------------------------------------
#' run the MultiMethodMotifMatcher
#'
#'
#' @rdname match
#'
#' @param obj A properly configured MultiMethodMotifMatching object
#'
#' @return A data.frame with scored hits
#'
#' @export
#'
setMethod("match", "MultiMethodMotifMatcher",

    function(obj){
       if(obj@sequenceMatchAlgorithm == "Biostrings matchPWM"){
          motifMatcher <- MotifMatcher(genomeName=obj@genome, pfms=obj@motifMatrix, quiet=obj@quiet)
          matchThreshold <- as.integer(100 * obj@matchThreshold)
          tbl.out <- findMatchesByChromosomalRegion(motifMatcher, obj@regions, pwmMatchMinimumAsPercentage=matchThreshold)
          if(nrow(tbl.out) == 0)
             return(data.frame)
          tbl.out <- tbl.out[, c("chrom", "motifStart", "motifEnd", "strand", "motifScore")]
          tbl.out$width <- with(tbl.out, 1 + motifEnd - motifStart)
          colnames(tbl.out) <- c("chrom", "start", "end", "strand", "score", "width")
          tbl.out <- tbl.out[, c("chrom", "start", "end", "width", "strand", "score")]
          return(tbl.out)
          } # Biostrings
       if(obj@sequenceMatchAlgorithm == "MOODS matchMotifs"){
         motif.tfbs <- convert_motifs(obj@motifMatrix[[1]], "TFBSTools-PWMatrix")
         gr.regions <- GRanges(obj@regions)
         tbl.out <- as.data.frame(matchMotifs(motif.tfbs, gr.regions, genome="hg38", out="positions"))
         if(nrow(tbl.out) == 0)
            return(data.frame())
         columns.to.remove <- grep("group", colnames(tbl.out))
         if(length(columns.to.remove) > 0)
            tbl.out <- tbl.out[, -columns.to.remove]
         colnames(tbl.out)[grep("seqnames", colnames(tbl.out))] <- "chrom"
         tbl.out$chrom <- as.character(tbl.out$chrom)
         tbl.out$strand <- as.character(tbl.out$strand)
         return(tbl.out)
         } # MOODS
       })

#------------------------------------------------------------------------------------------------------------------------
