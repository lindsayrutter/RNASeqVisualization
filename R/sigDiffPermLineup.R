# This function

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
correctPlace <- list()

if (!dir.exists(paste(getwd(), "/Perm", i, sep=""))){
  dir.create(paste(getwd(), "/Perm", i, sep=""))
}

listcond = rep(c("DR", "DU"), each = 6)
permInfo[[1]] = listcond

for(i in 1:3){
  if (i>1){
    # Check that permutations include equal number of treatments in each group (check that permutations are thoroughly shuffled)
    while (length(permInfo) < i){
      myPerm = permute(listcond)
      if (all(table(myPerm[1:6]) == table(myPerm[7:12]))){
        if (!all(myPerm == permInfo[[i-1]])){
          permInfo[[i]] = myPerm
        }
      }
    }
  }

  if (!dir.exists(paste(getwd(), "/PermLineup", sep=""))){
    dir.create(paste(getwd(), "/PermLineup", sep=""))
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
  write.csv(permList[[i]], file= paste("TopDEG", i, ".csv", sep=""))
}

for (i in 1:100){
  fullDat <- data.frame()
  lineup <- permute(seq(1:3))
  correctPlace[i] <- which(lineup==1)
  for (j in 1:3){
    gene = permList[[j]][i,1:12]
    rep = 6
    fact = 2
    dat = data.frame(x=rep(1:fact, each=rep),y=t(gene),z=lineup[j])
    colnames(dat)=c("x","y","z")
    dat$x=as.factor(dat$x)
    levels(dat$x)=c("DR","DU")
    dat$meanDR = mean(filter(dat, x=="DR")$y)
    dat$meanDU = mean(filter(dat, x=="DU")$y)
    fullDat <- rbind(fullDat, dat)
  }
    allPlot = ggplot(fullDat, aes(x, y)) + geom_point(aes(colour = factor(x)), shape = 20, size=5, alpha = 0.5) + scale_shape(solid = FALSE) + ggtitle(paste("Transcript: ", i)) + ylab("Read Count") + scale_y_continuous(limits=c(0, max(fullDat$y))) + theme(axis.title.x = element_blank(), legend.position="bottom", axis.text=element_text(size=12), axis.title=element_text(size=12), legend.title=element_text(size=12), legend.text=element_text(size=12), plot.title=element_text(hjust=0.5)) + labs(colour = "Group", size=12) + geom_segment(aes(x = 1, y = meanDR, xend = 2, yend = meanDU), colour="gray25", size = 0.1) + facet_grid(. ~ z)

    jpeg(file = paste(getwd(), "/PermLineup/Gene", i, ".jpg", sep=""), height = 700, width = 700)
    print(allPlot)
    dev.off()
}

colnames(correctPlace) = 1:100
correctPlace = data.frame(t(correctPlace))
write.csv(correctPlace, file = paste(getwd(), "/PermLineup/Correct.csv", sep=""))
