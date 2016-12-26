# Making scatterplot matrix interactive
library(GGally)
dat = mtcars[,1:3]
#p <- ggpairs(dat)
#ggplotly(p)

my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + geom_hex(...)
  p
}
p <- ggpairs(dat, lower = list(continuous = my_fn))
ggplotly(p)




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


