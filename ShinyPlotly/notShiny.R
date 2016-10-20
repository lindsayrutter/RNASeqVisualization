
data(BreastCancer)
plot.df <- data.frame(BreastCancer[,"Cl.thickness"],
                      BreastCancer[,"Cell.size"],
                      Class = BreastCancer$Class)
colnames(plot.df) <- c("x", "y", "Class")

# output$Plot1
plot_ly(plot.df, x = ~x, y = ~y, mode = "markers", color = ~Class, source = "subset",
        marker = list(size = 30)) %>%
  layout(title = paste("Cl.thickness", "vs ", "Cell.size"),
         xaxis = list(title = "Cl.thickness"),
         yaxis = list(title = "Cell.size"),
         dragmode =  "select",
         plot_bgcolor = "6A446F")

plot.df <- plot.df %>%
  group_by(x, y, Class) %>%
  summarize(Count = n()) %>%
  filter(Class == "malignant")

plot.df <- data.frame(plot.df)
plot.df$x <- as.numeric(plot.df$x)
plot.df$y <- as.numeric(plot.df$y)
plot.df$Count <- as.numeric(plot.df$Count)

  plot_ly(plot.df, x = ~x, y = ~y, z = ~Count, type = "contour") %>%
  layout(title = "Contour map of number of malignant cases",
         xaxis = list(title = "Cl.thickness"),
         yaxis = list(title = "Cell.size"))










library(plotly)
library(stringr)
library(reshape2)

data.loess <- loess(qsec ~ wt * hp, data = mtcars)

# Create a sequence of incrementally increasing (by 0.3 units) values for both wt and hp
xgrid <-  seq(min(mtcars$wt), max(mtcars$wt), 0.3)
ygrid <-  seq(min(mtcars$hp), max(mtcars$hp), 0.3)
# Generate a dataframe with every possible combination of wt and hp
data.fit <-  expand.grid(wt = xgrid, hp = ygrid)
# Feed the dataframe into the loess model and receive a matrix output with estimates of
# acceleration for each combination of wt and hp
mtrx3d <-  predict(data.loess, newdata = data.fit)
# Abbreviated display of final matrix
mtrx3d[1:4, 1:4]

# Transform data to long form
mtrx.melt <- melt(mtrx3d, id.vars = c('wt', 'hp'), measure.vars = 'qsec')
names(mtrx.melt) <- c('wt', 'hp', 'qsec')
# Return data to numeric form
mtrx.melt$wt <- as.numeric(str_sub(mtrx.melt$wt, str_locate(mtrx.melt$wt, '=')[1,1] + 1))
mtrx.melt$hp <- as.numeric(str_sub(mtrx.melt$hp, str_locate(mtrx.melt$hp, '=')[1,1] + 1))

plot_ly(mtrx.melt, x = ~wt, y = ~hp, z = ~qsec, type = "contour") %>% layout(autosize = F, width = 600, height = 500)
