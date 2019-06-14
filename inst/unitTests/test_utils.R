# test_utils.R
#------------------------------------------------------------------------------------------------------------------------
library(TrenaViz)
library(RUnit)
#------------------------------------------------------------------------------------------------------------------------
test_standardizeMotifMatchedTable <- function()
{
   printf("--- test_standardizeMotifMatchedTable")

   preferred.column.names <- c("chrom", "start", "end", "name", "pval", "tf")
   preferred.column.classes <- c("character", "numeric", "numeric", "character", "numeric", "character")

   tbl.fpdb <- get(load(system.file(package="TrenaViz", "extdata", "footprintDatabase-matches-table.RData")))
   tbl.fimo <- get(load(system.file(package="TrenaViz", "extdata", "fimo-matches-table.RData")))

   tbl.fpdb.fixed <- standardizeMotifMatchedTable(tbl.fpdb)
   tbl.fimo.fixed <- standardizeMotifMatchedTable(tbl.fimo)

   checkEquals(colnames(tbl.fpdb.fixed), preferred.column.names)
   checkEquals(colnames(tbl.fimo.fixed), preferred.column.names)

   checkEquals(as.character(lapply(tbl.fpdb.fixed, "class")), preferred.column.classes)
   checkEquals(as.character(lapply(tbl.fimo.fixed, "class")), preferred.column.classes)

} # test_standardizeMotifMatchedTable
#------------------------------------------------------------------------------------------------------------------------
