library(TrenaViz)
library(RUnit)
library(graph)

if(!exists("tv")){
   tv <- TrenaViz("TrenaProjectLymphocyte")
   tp <- TrenaProjectLymphocyte()
   load("gata2.model.RData")
   }
#------------------------------------------------------------------------------------------------------------------------
runTests <- function()
{
   test_.standardizeModelTable()
   test_.standardizeRegulatoryRegionsTable()

} # runTests
#------------------------------------------------------------------------------------------------------------------------
test_.standardizeModelTable <- function()
{
   printf("--- test_.standardizeModelTable")

   #-----------------------------------------------------------------------------------------
   # the incoming tbl.model presents these challenges:
   #
   #       gene betaLasso  lassoPValue pearsonCoeff  rfScore   betaRidge spearmanCoeff bindingSites
   #  6    E2F3         0 7.124847e-07    0.8683105 2.936714  0.04945335     0.8149973           NA
   #  45 HOXC13         0 3.987483e-02   -0.8640875 2.457541 -0.01531601    -0.7659080           NA
   #  97 ZNF263         0 6.236969e-01    0.9003067 2.134046  0.04104303     0.6360153           NA
   #  70  PRDM4         0 1.000000e+00    0.8984506 1.900193  0.03627523     0.7405583           NA
   #
   # and for which we want these results (first 4 rows only)
   #
   #         tf    pearson   spearman betaLasso randomForest
   #  6    E2F3  0.8683105  0.8149973         0     2.936714
   #  45 HOXC13 -0.8640875 -0.7659080         0     2.457541
   #  97 ZNF263  0.9003067  0.6360153         0     2.134046
   #  70  PRDM4  0.8984506  0.7405583         0     1.900193

   tbl.std <- TrenaViz:::.standardizeModelTable(tbl.model)
   checkEquals(dim(tbl.std), c(nrow(tbl.model), 5))
   checkEquals(colnames(tbl.std), c("tf", "pearson", "spearman", "betaLasso", "randomForest"))
   checkEquals(tbl.model$gene, tbl.std$tf)
   checkEquals(tbl.model$rfScore, tbl.std$randomForest)

} # test_.standardizeModelTable
#------------------------------------------------------------------------------------------------------------------------
test_.standardizeRegulatoryRegionsTable <- function()
{
   printf("--- test_.standardizeRegulatoryRegionsTable")

   tbl.regions <- get(load("../extdata/tbl.regRegions-GATA2.RData"))
   library(TrenaProjectErythropoiesis)
   tp <- TrenaProjectErythropoiesis()
   tss <- getTranscriptsTable(tp, "GATA2")$start
   targetGene <- "GATA2"
   tbl.std <- TrenaViz:::.standardizeRegulatoryRegionsTable(tbl.regions, targetGene, tss)
   checkEquals(nrow(tbl.regions), nrow(tbl.std))
   checkEquals(tbl.regions$geneSymbol, tbl.std$tf)
   checkEquals(tbl.regions$fp_start, tbl.std$start)
   checkTrue(all(tbl.std$targetGene == targetGene))

} # test_.standardizeRegulatoryRegionsTable
#------------------------------------------------------------------------------------------------------------------------
test_.geneRegulatoryModelToGraph <- function()
{
   printf("--- test_.geneRegulatoryModelToGraph")

   f <- system.file(package="TrenaViz", "extdata", "model.and.regRegions.irf4.top5.RData")
   print(load(f))
   tbl.model.std <- TrenaViz:::.standardizeModelTable(tbl.model)

   targetGene <- "IRF4"
   tss <- getTranscriptsTable(tp, targetGene)$start
   tbl.reg.std <- TrenaViz:::.standardizeRegulatoryRegionsTable(tbl.reg, targetGene, tss)

   #g <- .geneRegulatoryModelToGraph(targetGene, tbl.model.std, tbl.reg.std)
   g <- TrenaViz:::.geneRegulatoryModelToGraph(targetGene, tbl.model.std, tbl.reg.std)


} # test_.geneRegulatoryModelToGraph
#------------------------------------------------------------------------------------------------------------------------
# test_multiple.geneRegulatoryModelsToGraph <- function(display=FALSE)
# {
#    printf("--- test_multiple.geneRegulatoryModelToGraph")
#    load(system.file(package="TReNA", "extdata", "twoAQP4modelsForTesting.RData"))
#    tViz <- TrenaViz()
#
#    models <- list(wt=list(tbl.model=x.wt$tbl.model, tbl.regulatoryRegions=x.wt$tbl.regulatoryRegions),
#                   rs3875089=list(tbl.model=x.mut$tbl.model, tbl.regulatoryRegions=x.mut$tbl.regulatoryRegions))
#    g <- buildMultiModelGraph(tViz, "AQP4", models)
#    checkEquals(length(nodes(g)), 34)
#    checkEquals(length(edgeNames(g)), 46)
#
#    noa.names <- sort(names(nodeDataDefaults(g)))
#    checkEquals(length(noa.names), 33)
#    checkEquals(length(grep("rs3875089.", noa.names, fixed=TRUE)), 11)
#    checkEquals(length(grep("wt.", noa.names, fixed=TRUE)), 11)
#    g.lo <- addGeneModelLayout(tViz, g)
#
#    if(display){
#       addGraph(tViz, g.lo, names(models))
#       loadStyle(tViz, system.file(package="TrenaHelpers", "extdata", "style.js"))
#       Sys.sleep(3); fit(tViz)
#       browser();
#       xyz <- 99
#       }
#
#    return(TRUE)
#
#
# } # test_multiple.geneRegulatoryModelToGraph
#------------------------------------------------------------------------------------------------------------------------
