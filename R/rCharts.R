require(devtools)
install_github('rCharts', 'ramnathv')
library(rCharts)
install_github('Polychart/polychart2')

# Example 1 - Facetted Scatterplot
names(iris) = gsub("\\.", "", names(iris))
rPlot(SepalLength ~ SepalWidth | Species, data = iris, color = 'Species', type = 'point')

# Example 2 - Facetted Barplot
hair_eye = as.data.frame(HairEyeColor)
rPlot(Freq ~ Hair | Eye, color = 'Eye', data = hair_eye, type = 'bar')

# Example 3 - Using Polychart charting library
rPlot(mpg ~ wt | am + vs, data = mtcars, type = "point", color = "gear")

data(economics, package = "ggplot2")
econ <- transform(economics, date = as.character(date))
m1 <- mPlot(x = "date", y = c("psavert", "uempmed"), type = "Line", data = econ)

