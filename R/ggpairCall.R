#############################################
# Making scatterplot matrix interactive
library(GGally)
library(ggplot2)
library(plotly)
dat = mtcars[,1:3]

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

load("leavesDat.Rda")

# If try to do ggplotly(p) here, just get rainbow circle. Way too much data!
# Problem with adding alpha = 0.01 (scatmat(data, columns=1:6, color="cluster", alpha = 0.01)) is that it makes the outliers very light
p <- scatmat(data, columns=1:6, color="cluster") + scale_color_manual(values = levels(data$cluster), labels = c("Clusters 1-2","Cluster 3")) + theme(legend.position=c("bottom"), legend.text=element_text(size=12)) + ggtitle("L120 Replicates Fe+/-") +theme(legend.title=element_blank())

my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + geom_point(aes(alpha=-variableB)) + geom_abline(intercept = 0, color = "red", size = 0.25)
  p
}

my_fn <- function(data, mapping, ...){
  #p <- ggplot(data = data, mapping = mapping) + geom_hex(...)
  p <- ggplot(data = data, mapping = mapping) + geom_hex(binwidth=1) + geom_abline(intercept = 0, color = "red", size = 0.25)
  p
}
p <- ggpairs(data[,2:7], lower = list(continuous = my_fn))
pp <- ggplotly(p)

# Remove interactivity in diagonal
for (x in c(1,4,5,7:9)) pp$x$data[[x]]$hoverinfo <- "none"
pp

# Can erase diagonal, but does not seem to speed up and looks weird
ggpairs(data[,2:7], lower = list(continuous = my_fn), diag = NULL)

# Get x=y on same scale across individual plots
p <- ggpairs(data[,2:7], lower = list(continuous = my_fn))
myMax = max(data[,2:7])
myMin = 0
pS <- p
for(i in 2:p$nrow) {
  for(j in 1:(i-1)) {
    pS[i,j] <- p[i,j] +
      scale_x_continuous(limits = c(myMin, myMax)) +
      scale_y_continuous(limits = c(myMin, myMax))
  }
}

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


