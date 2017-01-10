#############################################
# Making scatterplot matrix interactive
library(GGally)
library(ggplot2)
library(plotly)
dat = mtcars[,1:3]
#p <- ggpairs(dat)
#ggplotly(p)

p <- ggpairs(dat)
ggplotly(p)

my_fn <- function(data, mapping, ...){
  #p <- ggplot(data = data, mapping = mapping) + geom_hex(bins=3)
  #p <- ggplot(data = data, mapping = mapping) + hexbin(x=x, y=y, xbins = 40)
  p <- ggplot(data = data, mapping = mapping) + geom_hex(binwidth=3)
  p
}
p <- ggpairs(dat, lower = list(continuous = my_fn))
ggplotly(p)

####################################################################
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

rm(list=ls())
load("All_leaves040615.rda")
ct <- countTable
ct2 <- assays(ct)[[1]]
rownames(ct2) <- ct@rowRanges@elementMetadata@listData$ID
colnames(ct2) <- unlist(strsplit(colnames(ct2), "\\."))[seq(1, 17*3, 3)]
leaves.all <- ct2
load("All_roots.rda")
ct.roots <- countTable
ct2 <- assays(ct.roots)[[1]]
rownames(ct2) <- ct@rowRanges@elementMetadata@listData$ID
colnames(ct2) <- unlist(strsplit(colnames(ct2), "\\."))[seq(1, 18*3, 3)]
roots.all <- ct2
all <- cbind(leaves.all, roots.all[,c(1:15,17:18)])
y <- DGEList(counts=all)
# Now try to threshold count number and normalize on the six samples
L120 = y[,c("ML08R","ML14R","ML22R","ML11R","ML27R","ML33R")]
colnames(L120)=c("s8","s14","s22","s11","s27","s33")

# View(L120$counts)
# Make sure each gene has at least one count in at least half of the six samples
L120 <- L120[rowSums(L120$counts>1)>=ncol(L120)/2,]
L120 <- calcNormFactors(L120, method="none") # Doesn't seem to do anything as per identical() function
# CPM values are useful descriptive measures for the expression level of a gene or transcript.
# By default, the normalized library sizes are used in the computation for DGEList objects but simple column sums for matrices.
# Now values are positive and negative
cpm.L120.new <- cpm(L120, TRUE, TRUE)
# Between-lane normalization for sequencing depth and possibly other distributional differences between lanes.
cpm.L120.norm <- betweenLaneNormalization(cpm.L120.new, which="full", round=FALSE)
L120 = cpm.L120.norm

RowSD = function(x) {
  sqrt(rowSums((x - rowMeans(x))^2)/(dim(x)[2] - 1))
}

L120t = L120
L120 = as.data.frame(L120t)
# Mutate is to add columns to a data frame
L120 = mutate(L120, mean = (s8+s14+s22+s11+s27+s33)/6, stdev = RowSD(cbind(s8,s14,s22,s11,s27,s33)))
rownames(L120)=rownames(L120t)
#L120 has 39,120
# The first quartile threshold of mean counts across the 5 samples
q1T = as.numeric(summary(L120$mean)["1st Qu."])
L120q1 = subset(L120,mean>q1T)
q1Ts = as.numeric(summary(L120q1$stdev)["1st Qu."])
L120q1 = subset(L120q1,stdev>q1Ts)
#L120q1 has 22,004
#filt has 17,116
#filt = subset(L120,mean<=q1T|stdev<=q1Ts)
#ind = seq(1, nrow(L120), by=10)

model = loess(mean ~ stdev, data=L120q1)
#L120q1 has 9,809
L120q1 = L120q1[which(sign(model$residuals) == 1),]
L120q1 = L120q1[,1:6]
# scale each row, so now negative and positive values
L120q1s = t(apply(as.matrix(L120q1), 1, scale))
colnames(L120q1s)=c("[1]Fe-","[2]Fe-","[3]Fe-","[1]Fe+","[2]Fe+","[3]Fe+")
colnames(L120q1)=c("[1]Fe-","[2]Fe-","[3]Fe-","[1]Fe+","[2]Fe+","[3]Fe+")

dendo = L120q1s
rownames(dendo) = NULL
d = dist(as.matrix(dendo))
hc = hclust(d, method="ward.D")

cNum=3
data=L120q1
set.seed(1)
k = kmeans(L120q1s,cNum)
colList = c("#bababa","#520090", "#bababa")
data$cluster = k$cluster
dat1 = data[which(data$cluster ==1),]
dat2 = data[which(data$cluster ==2),]
dat3 = data[which(data$cluster ==3),]
data = rbind(dat2, dat3, dat1)
data$cluster = factor(data$cluster)
levels(data$cluster) = c(colList)



# If try to do ggplotly(p) here, just get rainbow circle. Way too much data!
# Problem with adding alpha = 0.01 (scatmat(data, columns=1:6, color="cluster", alpha = 0.01)) is that it makes the outliers very light
p <- scatmat(data, columns=1:6, color="cluster") + scale_alpha(trans = reverse) + scale_color_manual(values = levels(data$cluster), labels = c("Clusters 1-2","Cluster 3")) + theme(legend.position=c("bottom"), legend.text=element_text(size=12)) + ggtitle("L120 Replicates Fe+/-") +theme(legend.title=element_blank())

my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + geom_point(aes(alpha=-variableB) + geom_abline(intercept = 0, color = "red", size = 0.25)
  p
}

my_fn <- function(data, mapping, ...){
  #p <- ggplot(data = data, mapping = mapping) + geom_hex(...)
  p <- ggplot(data = data, mapping = mapping) + geom_hex(binwidth=1) + geom_abline(intercept = 0, color = "red", size = 0.25)
  p
}
colnames(data) <- c("m1","m2","m3","p1","p2","p3","cluster")
p <- ggpairs(data[,1:6], lower = list(continuous = my_fn))
ggplotly(p)


# Zoom in one subplot
datSub <- select(data, p2, p3, cluster)

# Regular
#p2 <- ggplot(data = datSub, aes(p2, p3)) + geom_hex(binwidth=0.5) + geom_abline(intercept = 0, color = "red", size = 1)

# Split each bin in half, and keep count color on same scale regardless of group
#p2 <- ggplot(data = datSub, aes(p2, p3, group=cluster)) + geom_hex(binwidth=0.5) + geom_abline(intercept = 0, color = "red", size = 1)

# Lose count information, separate bins into two separate colors
#p2 <- ggplot(data = datSub, aes(p2, p3, fill=cluster)) + geom_hex(binwidth=0.5) + geom_abline(intercept = 0, color = "red", size = 1)

# Pretty good, has both counts and groups are entirely separate. If change bandwith=1, then have no hover information
#p2 <- ggplot(data = datSub, aes(p2, p3, colour=cluster)) + geom_hex(binwidth=1) + geom_abline(intercept = 0, color = "red", size = 1)

# Lose count information
#p2 <- ggplot(data = datSub, aes(p2, p3, fill=cluster, colour=cluster)) + geom_hex(binwidth=1) + geom_abline(intercept = 0, color = "red", size = 1)

#p2 <- ggplot(data = datSub, aes(p2, p3, alpha=cluster)) + geom_hex(binwidth=0.5) + geom_abline(intercept = 0, color = "red", size = 1)

#p2 <- ggplot(data = datSub, aes(p2, p3, alpha=cluster)) + geom_hex(binwidth=0.5) + geom_abline(intercept = 0, color = "red", size = 1)

p3 <- ggplotly(p2)

#####################################################################

plt<- ggplot(hives,aes(x=Temp,y=Humidity)) + geom_hex(bins=20) 
plt + facet_grid( ~ Hive)
ggplotly() 















p <- plot_ly(diamonds[1:1000, ], x = ~x, y = ~cut, color = ~color) %>% add_bars()
p <- layout(p, shapes = list(type = "line", fillcolor = "red",
                             line = list(color = "red"),
                             opacity = 1,
                             x0 = 3, x1 = 3, xref = 'x',
                             y0 = -0.5, y1 = 4.5, yref = 'y'))

####################################################################
data = dat;
mapping = NULL;
columns = 1:ncol(data);
title = NULL;
upper = list(continuous = "cor", combo = "box_no_facet", discrete = "facetbar", na = "na");
lower = list(continuous = "points", combo = "facethist", discrete = "facetbar", na = "na");
diag = list(continuous = "densityDiag", discrete = "barDiag", na = "naDiag"); params = NULL;
xlab = NULL;
ylab = NULL;
axisLabels = c("show", "internal", "none");
columnLabels = colnames(data[columns]);
labeller = "label_value";
showStrips = NULL;
legend = NULL;
cardinality_threshold = 15;
legends = stop("deprecated")


