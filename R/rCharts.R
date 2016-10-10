require(devtools)
install_github('rCharts', 'ramnathv')
library(rCharts)

#scatterplot
names(iris) = gsub("\\.", "", names(iris))
rPlot(SepalLength ~ SepalWidth | Species, data = iris, color = 'Species', type = 'point')
