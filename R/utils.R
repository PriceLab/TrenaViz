#----------------------------------------------------------------------------------------------------
#' standardize motif-matched regions from footprint databases and directly from fimo
#'
#'
#' @param tbl data.frame
#'
#' @return data.frame
#'
#' @export
#'
#' @aliases standardizeMotifMatchedTable
#' @rdname standardizeMotifMatchedTable
standardizeMotifMatchedTable <- function(tbl)
{
   preferred.column.names <- c("chrom", "start", "end", "name", "pval", "tf")

   if("fp_start" %in% colnames(tbl)){ # a data.frame from the footprint database
      dups <- which(duplicated(tbl$loc))
      if(length(dups) > 0)
         tbl <- tbl[-dups,]
      tbl.out <- tbl[ c("chrom", "fp_start", "fp_end", "motifName", "score3", "geneSymbol")]
      colnames(tbl.out) <- preferred.column.names
      tbl.out$start <- as.numeric(tbl.out$start)
      tbl.out$end <- as.numeric(tbl.out$end)
      } # from fpdb

   if("matched_sequence" %in% colnames(tbl)){  # straight from fimo
      tbl.out <- tbl[, c("chrom", "start", "end", "motif", "pValue", "tf")]
      colnames(tbl.out) <- preferred.column.names
      } # from fimo

   tbl.out

} # standardizMotifMatchedTable
#----------------------------------------------------------------------------------------------------
