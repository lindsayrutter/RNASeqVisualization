library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(GGally)
library(edgeR)

coty <- read_delim("R/SISBID-2016-master/data/GSE61857_Cotyledon_normalized.txt.gz", delim="\t", col_types="cddddddddd", col_names=c("ID", "C_S1_R1", "C_S1_R2", "C_S1_R3", "C_S2_R1", "C_S2_R2", "C_S2_R3", "C_S3_R1", "C_S3_R2", "C_S3_R3"), skip=1)
coty <- as.data.frame(coty)

d <- DGEList(counts = coty[,2:7],
             group = c(rep("S1", 3), rep("S2", 3)),
             genes = coty[,1])
d <- calcNormFactors(d)
d <- estimateCommonDisp(d)
d <- estimateTagwiseDisp(d)
d <- estimateTrendedDisp(d)
de <- exactTest(d, pair=c("S1", "S2"), dispersion = "tagwise")
sig.tab <- de$table
sig.tab$genes <- coty$ID
sig.tab <- sig.tab %>% filter(PValue < 0.01)

sig.tab <- merge(sig.tab, coty[,1:7], by.x="genes", by.y="ID")

#sub <- coty %>% select(ID, C_S1_R1, C_S1_R2, C_S1_R3, C_S2_R1, C_S2_R2, C_S2_R3)
#sub <- sub[1:1000,]

sig.tab <- mutate(sig.tab, xmin = pmin(C_S1_R1, C_S1_R2, C_S1_R3), xmax = pmax(C_S1_R1, C_S1_R2, C_S1_R3), ymin = pmin(C_S2_R1, C_S2_R2, C_S2_R3), ymax = pmax(C_S2_R1, C_S2_R2, C_S2_R3))

ggplot(sig.tab) +
  geom_segment(aes(x=xmin, xend=xmax, y=ymin, yend=ymin)) +
  geom_segment(aes(x=xmin, xend=xmin, y=ymin, yend=ymax)) +
  geom_segment(aes(x=xmin, xend=xmax, y=ymax, yend=ymax)) +
  geom_segment(aes(x=xmax, xend=xmax, y=ymin, yend=ymax)) +
  theme(aspect.ratio = 1)




### MWE ###########
myDat <- data.frame(A1 = runif(10,0,1), A2 = runif(10,0,1), A3 = runif(10,0,1), B1 = runif(10,0,1), B2 = runif(10,0,1), B3 = runif(10,0,1))
myDat <- mutate(myDat, xmin = pmin(A1, A2, A3), xmax = pmax(A1,A2,A3), ymin = pmin(B1,B2,B3), ymax = pmax(B1,B2,B3))

ggplot(myDat) +
  geom_segment(aes(x=xmin, xend=xmax, y=ymin, yend=ymin)) +
  geom_segment(aes(x=xmin, xend=xmin, y=ymin, yend=ymax)) +
  geom_segment(aes(x=xmin, xend=xmax, y=ymax, yend=ymax)) +
  geom_segment(aes(x=xmax, xend=xmax, y=ymin, yend=ymax)) +
  theme(aspect.ratio = 1)
