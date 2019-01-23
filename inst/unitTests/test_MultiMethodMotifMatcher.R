library(RUnit)
library(TrenaViz)
#------------------------------------------------------------------------------------------------------------------------
# cribbed from trena/inst/unitTests/test_MotifMatcher.R
#    jaspar.human.pfms <- as.list(query (query(MotifDb, "sapiens"), "jaspar2016"))
#    motifMatcher <- MotifMatcher(genomeName="hg38", pfms=jaspar.human.pfms, quiet=TRUE)
tbl.regions <- data.frame(chrom=rep("chr2",2),
                          start=c(88874736, 88879024),
                            end=c(88876958, 88880191),
                          stringsAsFactors=FALSE)
motif <- as.list(query(MotifDb, c("NFE2", "sapiens"), "hocomoco"))[1]
m4.biostrings <- MultiMethodMotifMatcher("hg38", motif, tbl.regions, "Biostrings matchPWM", .9)
m4.moods <- MultiMethodMotifMatcher("hg38", motif, tbl.regions, "MOODS matchMotifs", .9)
#------------------------------------------------------------------------------------------------------------------------
demo_trenaMotifMatcher <- function()
{
   library(trena)
   motifs <- query(MotifDb, "sapiens", "jaspar2018")
   motif <- query(MotifDb, c("NFE2", "sapiens"), "hocomoco")
   mm <- MotifMatcher(genomeName="hg38", as.list(motif), quiet=TRUE)
   tbl <-findMatchesByChromosomalRegion(mm, tbl.regions, pwmMatchMinimumAsPercentage=90)

} # demo_trenaMotifMatcher
#------------------------------------------------------------------------------------------------------------------------
demo_motifmatchr <- function()
{
   library(universalmotif)
   library(TFBSTools)
   library(motifmatchr)
   motif.tfbs <- convert_motifs(motif, "TFBSTools-PWMatrix")
   gr.regions <- GRanges(tbl.regions)
   tbl.hits <- as.data.frame(matchMotifs(motif.tfbs[[1]], gr.regions, genome="hg38", out="positions"))

} # demo_motifmatchr
#------------------------------------------------------------------------------------------------------------------------
runTests <- function()
{
   test_constructor()
   test_biostringsAlgorithm()
   test_moodsAlgorithm()

} # runTests
#------------------------------------------------------------------------------------------------------------------------
test_constructor <- function()
{
   checkEquals(is(m4.biostrings), "MultiMethodMotifMatcher")

} # test_constructor
#------------------------------------------------------------------------------------------------------------------------
test_biostringsAlgorithm <- function()
{
   printf("--- test_biostringsAlgorithm")

   tbl.hits.b <- match(m4.biostrings)
   checkEquals(nrow(tbl.hits.b), 1)
   checkEquals(tbl.hits.b$chrom, "chr2")
   checkEquals(tbl.hits.b$start, 88875627)
   checkEquals(tbl.hits.b$end, 88875640)
   checkEquals(tbl.hits.b$width, 14)
   checkEquals(tbl.hits.b$strand, "+")
   checkTrue(tbl.hits.b$score > 9.5)


} # test_biostringsAlgorithm
#------------------------------------------------------------------------------------------------------------------------
test_moodsAlgorithm <- function()
{
   print("--- test_moodsAlgorithm")

   tbl.hits.m <- match(m4.moods)
   checkEquals(nrow(tbl.hits.m), 1)
   checkEquals(tbl.hits.m$chrom, "chr2")
   checkEquals(tbl.hits.m$start, 88875627)
   checkEquals(tbl.hits.m$end, 88875640)
   checkEquals(tbl.hits.m$width, 14)
   checkEquals(tbl.hits.m$strand, "+")
   checkTrue(tbl.hits.m$score > 17)

} # test_moodsAlgorithm
#------------------------------------------------------------------------------------------------------------------------
