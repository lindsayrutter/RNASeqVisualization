# This function outputs several items into a folder called PermLineup:
# 1) Triangle lineups of the top 100 DEGs
# 2) Correct locations in Correct.csv
# 3) TopDEG1.csv (real data), TopDEG2.csv (permuted), TopDEG3.csv (permuted)
# 4) Permutations.csv tells the permutation orders

# It is different from sigDiffPerm.R because it does not include FDR value in plot outputs.

library(ggplot2)
library(edgeR)
library(dplyr)
library(gtools)
library(tibble)
library(readr)
library(EDASeq)

# countTable should be a dataframe with first column (type "chr") named "ID" and rest of columns (type "num") named sample names. Have no extra columns. The sample names must be in the format groupName.repNumber (ex: DU.1).

getLineups <- function(countTable, nRep, nPerm, outDir){
  group1 = unlist(strsplit(colnames(countTable)[2], "[.]"))[1]
  group2 = unlist(strsplit(colnames(countTable)[2+nRep], "[.]"))[1]
  listCond = rep(c(group1, group2), each = nRep)

  allComb <- getPerms(2*nRep)
  allCombLab <- allComb
  for (i in 1: (2*nRep)){
    allCombLab[which(allCombLab == i)] = colnames(countTable)[i + 1]
  }

  permList <- list()
  correctPlace <- list()

  if (!dir.exists(paste0(getwd(), "/", outDir))){
    dir.create(paste0(getwd(), "/", outDir))
  }

  for(i in 1:nPerm){
    y = DGEList(counts=countTable[,c(allComb[i,]+1)], group=listCond)
    rownames(y[[1]]) <- countTable[,1]
    keep <- rowSums(cpm(y)>1) >= 2*nRep
    # y <- y[keep, keep.lib.sizes=FALSE] # Option 1
    #y <- calcNormFactors(y) # Option 1

    y <- y[keep,] # Option 2
    y <- cpm(y, TRUE, TRUE) # Option 2 (edgeR)
    y <- betweenLaneNormalization(y, which="full", round=FALSE) #Option 2 (EDASeq)

    cpm.L120.new <- cpm(L120, TRUE, TRUE)
    cpm.L120.norm <- betweenLaneNormalization(cpm.L120.new, which="full", round=FALSE)

    y = estimateCommonDisp(y) #(edgeR)
    y = estimateTagwiseDisp(y)
    de = exactTest(y, pair=c(group1,group2))
    tt = topTags(de, n=nrow(y))
    nc = cpm(y, normalized.lib.sizes=TRUE)
    permList[[i]] = arrange(merge(nc, tt[[1]], by=0, all=TRUE), FDR)
    colnames(permList[[i]])[1] <- "ID"
    write.csv(permList[[i]], file= paste(getwd(), "/", outDir, "/TopDEG", i, ".csv", sep=""))
  }

  for (i in 1:100){
    fullDat <- data.frame()
    lineup <- permute(seq(1:nPerm))
    correctPlace[i] <- which(lineup==1)
    for (j in 1:nPerm){
      gene = permList[[j]][i,2:(2*nRep+1)]
      dat = data.frame(x=rep(1:2, each=nRep),y=t(gene),z=which(lineup==j))
      colnames(dat)=c("x","y","z")
      dat$x=as.factor(dat$x)
      levels(dat$x)=c(group1,group2)
      dat$meanG1 = mean(filter(dat, x==group1)$y)
      dat$meanG2 = mean(filter(dat, x==group2)$y)
      fullDat <- rbind(fullDat, dat)
    }
    allPlot = ggplot(fullDat, aes(x, y)) + geom_point(aes(colour = factor(x)), shape = 20, size=5, alpha = 0.5) + scale_shape(solid = FALSE) + ggtitle(paste("Transcript: ", i)) + ylab("Read Count") + scale_y_continuous(limits=c(0, max(fullDat$y))) + theme(axis.title.x = element_blank(), legend.position="bottom", axis.text=element_text(size=12), axis.title=element_text(size=12), legend.title=element_text(size=12), legend.text=element_text(size=12), plot.title=element_text(hjust=0.5)) + labs(colour = "Group", size=12) + geom_segment(aes(x = 1, y = meanG1, xend = 2, yend = meanG2), colour="gray25", size = 0.1) + facet_wrap(~ z, ncol = 5)

    jpeg(file = paste0(getwd(), "/", outDir, "/", "Gene", i, ".jpg"), height = ceiling(nPerm/5)*175, width = 700)
    print(allPlot)
    dev.off()
  }

  correctPlace = data.frame(correctPlace)
  colnames(correctPlace) = 1:100
  correctPlace = t(correctPlace)
  colnames(correctPlace) = "DataPlot"
  correctPlace = data.frame(correctPlace)
  correctPlace <- rownames_to_column(correctPlace, "Gene")
  write.csv(correctPlace, row.names = FALSE, file = paste0(getwd(), "/", outDir, "/Correct.csv"))

  # permInfo --> allComb[1,]
  permInfo <- data.frame(matrix(unlist(allCombLab), nrow = nPerm))
  rownames(permInfo) <- c("Data", paste0(rep("Permute",(nPerm-1)),1:(nPerm-1)))
  colnames(permInfo) <- c(1:(2*nRep))
  write.table(permInfo, sep=",",  col.names=FALSE, file = paste0(getwd(), "/", outDir, "/Permutations.csv"))
}
