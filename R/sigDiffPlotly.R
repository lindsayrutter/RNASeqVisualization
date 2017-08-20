# This will interactively plot the 100 most significant DEGs in plotly.

#library(nullabor) #
library(rtracklayer)
library(Rsamtools)
library(grid)
library(GenomicAlignments)
library(ggplot2)
#library(GGally) #
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
library(plotly)

rm(list=ls())
load("../data/All_wasp.rda")

listcond = rep(c("DR","DU"),each= 6)
y = DGEList(counts=countTable[,c(1:12)], group=listcond)
keep <- rowSums(cpm(y)>1) >= 6
y <- y[keep, keep.lib.sizes=FALSE]
y <- calcNormFactors(y)
y = estimateCommonDisp(y)
y = estimateTagwiseDisp(y)
de = exactTest(y, pair=c("DR","DU"))
tt = topTags(de, n=nrow(y))
nc = cpm(y, normalized.lib.sizes=TRUE)
rn = rownames(tt$table)
geneList = cbind(nc[rn,order(listcond)], tt$table)

ax <- list(title = "", showticklabels = TRUE)
ay <- list(title = "Read Count")

for (i in 1:100){
  gene = geneList[i,1:12]
  rep = 6
  fact = 2
  dat = data.frame(x=rep(1:fact, each=rep),y=t(gene),z=rep(1:rep, times = fact))
  colnames(dat)=c("x","y","rep")
  dat$x=as.factor(dat$x)
  levels(dat$x)=c("DR","DU")
  g1 <- levels(dat$x)[1]
  g2 <- levels(dat$x)[2]
  g1m <- mean(filter(dat, x==g1)$y)
  g2m <- mean(filter(dat, x==g2)$y)

  dat <- dat[,1:2]

  dat %>% plot_ly(x = ~x, y = ~y, type = "scatter", marker = list(size = 10), color = ~x, colors = "Set2", hoverinfo = "text", text = paste0("Read count = ", format(round(dat$y, 2), nsmall = 2))) %>% layout(title = paste("Transcript =", rownames(gene), "<br> FDR =", formatC(geneList[i,]$FDR, format = "e", digits = 2)), xaxis = ax, yaxis = ay, legend = list(x = 0.35, y = -0.26)) %>% add_segments(x = g1, xend = g2, y = g1m, yend = g2m, showlegend = FALSE, line = list(color='#000000')) %>% add_trace(x = g1, y= g1m, showlegend = FALSE, hoverinfo = "text", text = paste0("Mean Read Count = ", round(g1m, digits = 2)), marker = list(color='#000000')) %>% add_trace(x = g2, y= g2m, showlegend = FALSE, hoverinfo = "text", text = paste0("Mean Read Count = ", round(g2m, digits = 2)), marker = list(color='#000000'))
}
