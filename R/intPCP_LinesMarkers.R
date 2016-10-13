# http://stackoverflow.com/questions/40014168/plotly-in-r-box-selection-for-plot-with-many-lines

library(plotly)
library(reshape2)
# some reproducible data
d <- cbind.data.frame(x=1:nrow(iris), iris[,-5])
d <- d[1:12,]

# transform to long format using reshape's melt() function
d_long <- melt(d, id.vars ="x" )

p <- plot_ly(d_long, x= ~x, y= ~value, type = 'scatter', mode = 'lines+markers', color = ~variable)  %>% layout(dragmode="box")

