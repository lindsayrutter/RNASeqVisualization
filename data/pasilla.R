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

# As a first processing step, we need to estimate the effective library size. This step is sometimes also called normalisation, even though there is no relation to normality or a normal distribution. The effective library size information is called the size factors vector, since the package only needs to know the relative library sizes. If the counts of non-differentially expressed genes in one sample are, on average, twice as high as in another (because the library was sequenced twice as deeply), the size factor for the first sample should be twice that of the other sample
cds = estimateSizeFactors(cds)
head(counts(cds))
sizeFactors(cds)

# If we divide each column of the count table by the size factor for this column, the count values are brought to a common scale, making them comparable. When called with normalized=TRUE, the counts accessor function performs this calculation.
head(counts(cds, normalized=TRUE))

# The dispersion can be understood as the square of the coefficient of biological variation. So, if a gene’s expression typically differs from replicate to replicate sample by 20 percent, this gene’s dispersion is (0.2)^2 = .04. It is difficult to do this calcluation, because it also involves optimization.
cds = estimateDispersions(cds)

# The function estimateDispersions performs three steps. First, it estimates a dispersion value for each gene, then it fits a curve through the estimates. Finally, it assigns to each gene a dispersion value, using a choice between the per-gene estimate and the fitted value. To allow the user to inspect the intermediate steps, a fitInfo object is stored, which contains the per-gene estimate, the fitted curve and the values that will subsequently be used for inference.
str(fitInfo(cds))

# It is useful to inspect the results of these steps visually. We can plot the per-gene estimates against the mean normalized counts per gene and overlay the fitted curve by using the function plotDispEsts
plotDispEsts(cds)

# Having estimated the dispersion for each gene, it is straight-forward to look for differentially expressed genes. To contrast two conditions, e.g., to see whether there is differential expression between conditions "untreated" and "treated", we simply call the function nbinomTest. It performs the tests as described in [1] and returns a data frame with the p values and other useful information.
# cds in nbinomTest() is CountDataSet with size factors and raw variance functions
res = nbinomTest(cds, "untreated", "treated")

head(res)
plotMA(res)
hist(res$pval, breaks=100, col="skyblue", border="slateblue", main="")
#3783
resSig = res[ res$padj < 0.05, ]

#######################################################################
# Make variance stabilizied transformed data
cdsBlind = estimateDispersions(cds, method = "blind")
vsd = varianceStabilizingTransformation(cdsBlind)
print(plotPCA(vsd, intgroup="condition"))
