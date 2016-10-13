# DRDUq1s.Rdata was made from L120.Rnw file in /Users/lindz/CLUSTERING_WASP/Clustering
# It is the data before making PCP (normalized, logged, standardized)

load("DRDUq1s.Rdata")



load("geneList.rda")
library(dplyr)

geneList$MeanDR = rowMeans(subset(geneList, select = c(DR.1, DR.2, DR.3, DR.4, DR.5, DR.6)), na.rm = TRUE)
geneList$MeanDU = rowMeans(subset(geneList, select = c(DU.1, DU.2, DU.3, DU.4, DU.5, DU.6)), na.rm = TRUE)
geneList$StdDR = rowSds(as.matrix(geneList), cols = 1:6)
geneList$StdDU = rowSds(as.matrix(geneList), cols = 7:12)
geneList$Std = rowMeans(subset(geneList, select = c(StdDR, StdDU)), na.rm = TRUE)
geneList$Std = abs(log(geneList$Std))

#####################################################################
library(plotly)

myDat <- geneList[1:100,]

plot_ly(myDat, x = ~MeanDR, y = ~MeanDU, type = 'scatter', mode = 'markers',
        marker = list(size = ~Std, opacity = 0.5)) %>%
  layout(title = 'Genes',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE)) %>%

#####################################################################
library(plotly)

data <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/school_earnings.csv")

plot_ly(data, x = ~Women, y = ~Men, type = 'scatter', mode = 'markers',
        marker = list(size = ~gap, opacity = 0.5)) %>%
  layout(title = 'Numbers of School Earning by Sex',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE))

#####################################################################
gene12 = select(geneList, DR.1, DR.2, DU.1, DU.2)
colnames(gene12) <- c("x","xend","y","yend")

set.seed(1)
x <- runif(5000,-1,1)
xend <- runif(5000,-1,1)
y <- runif(5000,-1,1)
yend <- runif(5000,-1,1)
data <- data.frame(x = x, xend = xend, y = y, yend = yend)

ggplot() + geom_segment(data=gene12, mapping=aes(x=x, y=y, xend=xend, yend=yend), alpha = 0.1) + xlim(0,1000) + ylim(0,1000) + geom_abline()

gene12 %>% add_segments(x = gene12$x, xend = gene12$xend, y = gene12$y, yend = gene12$yend, showlegend = FALSE)


x <- c(1:100)
random_y <- rnorm(100, mean = 0)
data <- data.frame(x, random_y)

testDRDU <- DRDUq1s[1:100,]
plot_ly(testDRDU, x = ~x, y = ~random_y, type = 'scatter', mode = 'lines')
