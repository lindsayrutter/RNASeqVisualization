
data(BreastCancer)
plot.df <- data.frame(BreastCancer[,"Cl.thickness"],
                      BreastCancer[,"Cell.size"],
                      Class = BreastCancer$Class)
colnames(plot.df) <- c("x", "y", "Class")

################################################################
# output$Plot1
# plot_ly(source = "subset")
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

################################################################
# output$Plot2
plot_ly(plot.df, x = ~x, y = ~y, z = ~Count, type = "contour") %>%
layout(title = "Contour map of number of malignant cases",
       xaxis = list(title = "Cl.thickness"),
       yaxis = list(title = "Cell.size"))

################################################################
# event.data <- event_data("plotly_selected", source = "subset")

# https://plot.ly/javascript/hover-events/
# curveNumber = index in data of the trace associated with the selected point
# pointNumber = index of the selected point

# Get number of malignant and benign cases from selection
#malig.class <- subset(plot.df, Class == "malignant")[subset(event.data, curveNumber == 0)$pointNumber + 1,]
#benign.class <- subset(plot.df, Class == "benign")[subset(event.data, curveNumber == 1)$pointNumber + 1,]


plot.subset <- rbind(malig.class, benign.class)

# Summarize
plot.summ <- plot.subset %>%
  group_by(x, y, Class) %>%
  summarize(Count = n())

