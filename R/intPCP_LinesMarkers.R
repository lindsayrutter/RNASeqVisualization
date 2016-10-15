# http://stackoverflow.com/questions/40014168/plotly-in-r-box-selection-for-plot-with-many-lines

library(plotly)
library(reshape2)
# some reproducible data
#d <- cbind.data.frame(x=1:nrow(iris), iris[,-5])
#d <- d[1:12,]

trace_0 <- rnorm(12, mean = 0)
trace_1 <- rnorm(12, mean = 0)
trace_2 <- rnorm(12, mean = 0)
trace_3 <- rnorm(12, mean = 0)
trace_4 <- rnorm(12, mean = 0)
trace_5 <- rnorm(12, mean = 0)
trace_6 <- rnorm(12, mean = 0)
trace_7 <- rnorm(12, mean = 0)
trace_8 <- rnorm(12, mean = 0)
trace_9 <- rnorm(12, mean = 0)
trace_10 <- rnorm(12, mean = 0)
trace_11 <- rnorm(12, mean = 0)
trace_12 <- rnorm(12, mean = 0)
trace_13 <- rnorm(12, mean = 0)
trace_14 <- rnorm(12, mean = 0)
trace_15 <- rnorm(12, mean = 0)
trace_16 <- rnorm(12, mean = 0)
trace_17 <- rnorm(12, mean = 0)
trace_18 <- rnorm(12, mean = 0)
trace_19 <- rnorm(12, mean = 0)
trace_20 <- rnorm(12, mean = 0)

x <- c(1:12)

d <- data.frame(x, trace_0, trace_1, trace_2, trace_3, trace_4, trace_5, trace_6, trace_7, trace_8, trace_9, trace_10, trace_11, trace_12, trace_13, trace_14, trace_15, trace_16, trace_17, trace_18, trace_19, trace_20)

# transform to long format using reshape's melt() function
d_long <- melt(d, id.vars ="x" )

p <- plot_ly(d_long, x= ~x, y= ~value, type = 'scatter', mode = 'lines+markers', color = ~variable)  %>% layout(dragmode="box", showlegend = FALSE)

