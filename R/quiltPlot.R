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

sub <- coty %>% select(ID, C_S1_R1, C_S1_R2, C_S1_R3, C_S2_R1, C_S2_R2, C_S2_R3)
sub <- sub[1:10,]

sub <- mutate(sub, xmin = pmin(C_S1_R1, C_S1_R2, C_S1_R3), xmax = pmax(C_S1_R1, C_S1_R2, C_S1_R3), ymin = pmin(C_S2_R1, C_S2_R2, C_S2_R3), ymax = pmax(C_S2_R1, C_S2_R2, C_S2_R3))



ggplot(sub, aes(x=xmin, xend=xmax, y=ymin, yend=ymin)) +
 geom_segment() + xlim(c(0, 17.5)) + ylim(c(0, 17.5)) +
 theme(aspect.ratio = 1)

# aes(x=xmin, xend=xmin, y=ymin, yend=ymax),


### MWE ###########
myDat <- data.frame(A1 = runif(10,0,1), A2 = runif(10,0,1), A3 = runif(10,0,1), B1 = runif(10,0,1), B2 = runif(10,0,1), B3 = runif(10,0,1))
myDat <- mutate(myDat, xmin = pmin(A1, A2, A3), xmax = pmax(A1,A2,A3), ymin = pmin(B1,B2,B3), ymax = pmax(B1,B2,B3))

ggplot(myDat) +
  geom_segment(aes(x=xmin, xend=xmax, y=ymin, yend=ymin)) +
  geom_segment(aes(x=xmin, xend=xmin, y=ymin, yend=ymax)) +
  geom_segment(aes(x=xmin, xend=xmax, y=ymax, yend=ymax)) +
  geom_segment(aes(x=xmax, xend=xmax, y=ymin, yend=ymax)) +
  theme(aspect.ratio = 1)
