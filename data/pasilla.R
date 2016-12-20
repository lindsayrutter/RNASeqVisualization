source("https://bioconductor.org/biocLite.R")
biocLite("pasilla")
library(pasilla)
library(DESeq)

datafile = system.file("extdata/pasilla_gene_counts.tsv", package="pasilla")
pasillaCountTable = read.table( datafile, header=TRUE, row.names=1 )

pasillaDesign = data.frame(row.names = colnames( pasillaCountTable ), condition = c( "untreated", "untreated", "untreated", "untreated", "treated", "treated", "treated" ), libType = c("single-end", "single-end", "paired-end", "paired-end", "single-end", "paired-end", "paired-end" ))

pairedSamples = pasillaDesign$libType == "paired-end"
#14599
countTable = pasillaCountTable[ , pairedSamples ]
condition = pasillaDesign$condition[ pairedSamples ]

cds = newCountDataSet(countTable, condition)



