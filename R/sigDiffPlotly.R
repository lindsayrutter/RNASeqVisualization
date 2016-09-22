library(nullabor)
library(rtracklayer)
library(Rsamtools)
library(grid)
library(GenomicAlignments)
library(ggplot2)
library(GGally)
library(edgeR)
library(stringr)
library(EDASeq)
library(dplyr)
library(matrixStats)
library(gridExtra)
library(reshape2)
library(scales)
library(tidyr)
library(gtools)

rm(list=ls())
load("data/All_wasp.rda")

permList <- list()
permInfo <- list()

listcond = rep(c("DR","DU"),each= 6)
permInfo[[1]] = listcond

for(i in 1:3){
  if (i>1){
    permInfo[[i]] = permute(listcond)
  }

  y = DGEList(counts=countTable[,c(1:12)], group=permInfo[[i]])

  keep <- rowSums(cpm(y)>1) >= 6
  y <- y[keep, keep.lib.sizes=FALSE]

  y <- calcNormFactors(y)
  y = estimateCommonDisp(y)
  y = estimateTagwiseDisp(y)

  de = exactTest(y, pair=c("DR","DU"))
  tt = topTags(de, n=nrow(y))

  nc = cpm(y, normalized.lib.sizes=TRUE)
  rn = rownames(tt$table)

  permList[[i]] = cbind(nc[rn,order(permInfo[[i]])], tt$table)
  write.csv(permList[[i]], file= paste("TopDEG_", i, ".csv", sep=""))
}

for (i in 1:100){
    gene = permList[[1]][i,1:12]
    rep = 6
    fact = 2
    dat = data.frame(x=rep(1:fact, each=rep),y=t(gene),z=rep(1:rep, times = fact))
    colnames(dat)=c("x","y","rep")
    dat$x=as.factor(dat$x)
    levels(dat$x)=c("DR","DU")
    genePlot = ggplot(dat, aes(x, y)) + geom_point(aes(colour = factor(x)), shape = 20, size=5) + scale_shape(solid = FALSE) + ylab("Read Count") + ggtitle(paste("Transcript:", rownames(gene), " FDR: ", permList[[1]][i,]$FDR,3)) + scale_y_continuous(limits=c(0, max(dat$y))) + theme(axis.title.x = element_blank(), legend.position="bottom", axis.text=element_text(size=12), axis.title=element_text(size=12), legend.title=element_text(size=12), legend.text=element_text(size=12)) + labs(colour = "Group", size=12) + geom_segment(aes(x = 1, y = mean(dat$y[1:6]), xend = 2, yend = mean(dat$y[7:12])))

    jpeg(file = paste(getwd(), "/Perm1/", "Gene_", i, ".jpg", sep=""), height = 700, width = 700)
    print(genePlot)
    dev.off()
}

