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


