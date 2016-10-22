library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(GGally)
library(edgeR)

coty <- read_delim("R/SISBID-2016-master/data/GSE61857_Cotyledon_normalized.txt.gz", delim="\t", col_types="cddddddddd", col_names=c("ID", "C_S1_R1", "C_S1_R2", "C_S1_R3", "C_S2_R1", "C_S2_R2", "C_S2_R3", "C_S3_R1", "C_S3_R2", "C_S3_R3"), skip=1)
coty <- as.data.frame(coty)

# 2-rep porcupine plot
#sub <- coty %>% select(ID, C_S1_R1, C_S1_R2, C_S2_R1, C_S2_R2)
#ggplot(sub, aes(x=C_S1_R1, xend=C_S1_R2, y=C_S2_R1, yend=C_S2_R2)) +
#  geom_segment() + xlim(c(0, 17.5)) + ylim(c(0, 17.5)) +
#  theme(aspect.ratio = 1)
