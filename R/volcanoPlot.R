library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(GGally)
library(edgeR)

coty <- read_delim("R/SISBID-2016-master/data/GSE61857_Cotyledon_normalized.txt.gz", delim="\t", col_types="cddddddddd", col_names=c("ID", "C_S1_R1", "C_S1_R2", "C_S1_R3", "C_S2_R1", "C_S2_R2", "C_S2_R3", "C_S3_R1", "C_S3_R2", "C_S3_R3"), skip=1)
coty <- as.data.frame(coty)
colnames(coty) <- c("ID","S1.1","S1.2","S1.3","S2.1","S2.2","S2.3","S3.1","S3.2","S3.3")

d <- DGEList(counts = coty[,2:10],
             group = c(rep("S1", 3), rep("S2", 3), rep("S3", 3)),
             genes = coty[,1])
d <- calcNormFactors(d)
d <- estimateCommonDisp(d)
d <- estimateTagwiseDisp(d)
d <- estimateTrendedDisp(d)

myLevels <- levels(d@.Data[[2]]$group)
myList <- list()

# Runs exact test on all pairs of groups and saves in list
for (i in 1:(length(myLevels)-1)){
  for (j in (i+1):(length(myLevels))){
    coty[[paste(i,j,"FC",sep="-")]] <- as.data.frame(exactTest(d, pair=c(myLevels[i], myLevels[j]), dispersion = "tagwise"))$table.logFC
    coty[[paste(i,j,"pval",sep="-")]] <- as.data.frame(exactTest(d, pair=c(myLevels[i], myLevels[j]), dispersion = "tagwise"))$table.PValue
  }
}

nCol = ncol(coty)
dat = coty
datFCP = dat[,(nCol-2*length(myLevels)+1):nCol]

# x-axis FC, y-axis pval
xMax = max(datFCP[,seq(1,ncol(datFCP),by=2)])
xMin = min(datFCP[,seq(1,ncol(datFCP),by=2)])
yMax = max(datFCP[,seq(2,ncol(datFCP),by=2)])
yMin = min(datFCP[,seq(2,ncol(datFCP),by=2)])

# my_fn <- function(data, mapping, ...){
#   x = data[,c(as.character(mapping$x))]
#   y = data[,c(as.character(mapping$y))]
#   p <- ggplot(data = dat, aes(x=x, y=y)) + coord_cartesian(xlim = c(xMin, xMax), ylim = c(yMin, yMax))
#   p
# }
#
# p <- ggpairs(dat[,-1], lower = list(continuous = my_fn))


      coord_cartesian(xlim = c(maxRange[1], maxRange[2]), ylim = c(maxRange[1], maxRange[2]))











qplot(data=deDF, table.logFC, -log(table.PValue))

ui <- shinyUI(fluidPage(
  sliderInput("threshold", "Fold Change:", min = 0, max = 30, value=15, step=0.1),
  plotlyOutput("myPlot")
))

server <- shinyServer(function(input, output) {

})

shinyApp(ui, server)
