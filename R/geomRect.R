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

sig.tab <- mutate(sig.tab, xmin = pmin(C_S1_R1, C_S1_R2, C_S1_R3), xmax = pmax(C_S1_R1, C_S1_R2, C_S1_R3), ymin = pmin(C_S2_R1, C_S2_R2, C_S2_R3), ymax = pmax(C_S2_R1, C_S2_R2, C_S2_R3))

ggplot(sig.tab, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) + geom_rect()
