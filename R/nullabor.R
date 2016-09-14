library(nullabor)
# The "null_permute" function takes as input a variable name of the data. This variable is permuted to obtain the null dataset.
d <- lineup(null_permute("mpg"), mtcars)

attr(d, "pos")

qplot(mpg, wt, data = d) + facet_wrap(~ .sample)

calc_mean_dist(lineup(null_permute('mpg'), mtcars, pos = 10), var = c('mpg', 'wt'), met = 'reg_dist', pos = 10)

#################################################################

# qplot(mpg, wt, data = mtcars)

d <- lineup(null_permute("mpg"), mtcars, n=3)

# Display different samples
# qplot(mpg, wt, data = mtcars) %+%
#   lineup(null_permute("mpg"), mtcars) +
#   facet_wrap(~ .sample)

# qplot(mpg, wt, data = d) + facet_wrap(~ .sample)

attr(d, "pos")

# Or with ggplot2
ggplot(d, aes(mpg, wt)) +
  geom_point() +
  facet_wrap(~ .sample)

#################################################################

threept <- subset(lal, type == "3pt" &
                    !is.na(x) & !is.na(y))
threept <- threept[c(".id", "period", "time",
                     "team", "etype", "player", "points", "result",
                     "x", "y")]
threept <- mutate(threept,
                  x = x + runif(length(x), -0.5, 0.5),
                  y = y + runif(length(y), -0.5, 0.5),
                  r = sqrt((x - 25) ^ 2 + y ^ 2),
                  angle = atan2(y, x - 25) * 180 / pi)

threept <- subset(threept, r > 20 & r < 39)

qplot(x, y, data = threept) + coord_equal()

qplot(x, y, data = threept) %+%
  lineup(null_permute("y"), threept) +
  facet_wrap(~ .sample) +
  coord_equal()

#################################################################

samps <- lineup(null_lm(r ~ poly(angle, 2)),
                threept, n = 5)

ggplot(samps, aes(angle, r)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ .sample)

ggplot(samps, aes(angle, r)) +
  geom_boxplot(aes(group = round_any(angle, 15))) +
  facet_wrap(~ .sample)


#################################################################

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
load("All_wasp.rda")
listcond = rep(c("DR","DU"),each= 6)
y = DGEList(counts=countTable[,c(1:12)], group=listcond)

keep <- rowSums(cpm(y)>1) >= 6
y <- y[keep, keep.lib.sizes=FALSE]

y <- calcNormFactors(y)
y = estimateCommonDisp(y)
y = estimateTagwiseDisp(y)

de = exactTest(y, pair=c("DR","DU"))
tt = topTags(de, n=nrow(y))
head(tt$table)
length(which((tt$table)$FDR < 0.05))

nc = cpm(y, normalized.lib.sizes=TRUE)
rn = rownames(tt$table)
# Sorted in order of lowest FDR from DE comparison
head(nc[rn,order(listcond)],5)

# just for plotting purposes
deg = rn[tt$table$FDR < .05]
topInfo = cbind(nc[rn,order(listcond)], tt$table)
write.csv(topInfo, file="TopDEG.csv")

for (i in 1:100){
  gene = topInfo[i,1:12]
  rep = 6
  fact = 2
  dat = data.frame(x=rep(1:fact, each=rep),y=t(gene),z=rep(1:rep, times = fact))
  colnames(dat)=c("x","y","rep")
  dat$x=as.factor(dat$x)
  levels(dat$x)=c("DR","DU")
  genePlot = ggplot(dat, aes(x, y)) + geom_point(aes(colour = factor(x)), shape = 20, size=5) + scale_shape(solid = FALSE) + ylab("Read Count") + ggtitle(paste("Transcript:", rownames(gene), " FDR: ", topInfo[i,]$FDR)) + scale_y_continuous(limits=c(0, max(dat$y))) + theme(axis.title.x = element_blank(), legend.position="bottom", axis.text=element_text(size=12), axis.title=element_text(size=12), legend.title=element_text(size=12), legend.text=element_text(size=12)) + labs(colour = "Group", size=12) + geom_segment(aes(x = 1, y = mean(dat$y[1:6]), xend = 2, yend = mean(dat$y[7:12])))

  jpeg(file = paste(getwd(), "/Perm1/", "Gene_", i, ".jpg", sep=""), height = 700, width = 700)
  print(genePlot)
  dev.off()
}

