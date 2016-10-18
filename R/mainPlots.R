library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(GGally)
library(edgeR)

coty <- read_delim("R/SISBID-2016-master/data/GSE61857_Cotyledon_normalized.txt.gz", delim="\t", col_types="cddddddddd", col_names=c("ID", "C_S1_R1", "C_S1_R2", "C_S1_R3", "C_S2_R1", "C_S2_R2", "C_S2_R3", "C_S3_R1", "C_S3_R2", "C_S3_R3"), skip=1)
coty <- as.data.frame(coty)
GGally::ggscatmat(head(coty,1000), columns=2:7, alpha=0.1) # use just the first 1000

sub <- coty %>% select(ID, C_S1_R1, C_S1_R2, C_S2_R1, C_S2_R2)
ggplot(sub, aes(x=C_S1_R1, xend=C_S1_R2, y=C_S2_R1, yend=C_S2_R2)) +
  geom_segment() + xlim(c(0, 17.5)) + ylim(c(0, 17.5)) +
  theme(aspect.ratio = 1)

ggparcoord(coty[sample(1:nrow(coty), 1000),],
           columns=2:10, scale="globalminmax",
           alphaLines=0.1)

ggplot(sub, aes(x=stage, y=expr)) + geom_point(alpha=0.4) +
  facet_wrap(~ID, ncol=3) +
  geom_line(data=sub.m, aes(x=stage, y=expr), colour="red")


