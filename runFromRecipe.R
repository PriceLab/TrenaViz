library(trenaSGM)
library(TrenaProjectErythropoiesis)
load("recipe.RData")
load("~/github/trenaSGM/inst/extdata/recipe-to-reproduce-gene-named-1-bug.RData")
genomeName <- "hg38"
targetGene <- "GATA2"
builder <- trenaSGM::FimoDatabaseModelBuilder(genomeName, targetGene, recipe)
x <- build(builder)
