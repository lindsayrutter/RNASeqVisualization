#https://github.com/jcheng5/plotly/blob/joe/feature/crosstalk/inst/examples/crosstalk/01-intro.R
#Because all data must be loaded into the browser, CROSSTALK IS NOT APROPRIATE FOR LARGE DATASETS. (There's no hard limit, since HTML widgets require varying amounts of CPU cycles and memory for each data point.)

#devtools::install_github("ropensci/plotly@joe/feature/crosstalk")
devtools::install_github("rstudio/crosstalk", repos = "http://cran.rstudio.com")
library(plotly)
library(crosstalk)

nPatients <- 20
nVisits <- 10
set.seed(7)
df <- data.frame(
  ID = rnorm(n = nPatients * nVisits, mean = 50, sd = 10),
  patient = rep(seq(nPatients), each = nVisits),
  Sample = rep(seq(nVisits), nPatients))

# declare the patient variable as the "unit of interest"
sd <- SharedData$new(df, ~patient)

p <- plot_ly(df, x = ~Sample, y = ~ID, text = ~paste("Patient:", patient)) %>%
  group_by(patient) %>%
  add_lines(color = I("orange")) %>%
  add_markers(color = I("steelblue"), hoverinfo = "text") %>%
  hide_legend()




set.seed(5)
pcpDat <- data.frame(ID = paste0("ID",1:20), A=1.3*rnorm(20), B=1.3*rnorm(20), C=1.3*rnorm(20), D=1.3*rnorm(20), E=1.3*rnorm(20), F=1.3*rnorm(20))
pcpDat$ID <- as.character(pcpDat$ID)
colNms <- colnames(pcpDat[, c(2:(ncol(pcpDat)))])
nVar <- length(colNms)


boxDat <- pcpDat %>% gather(key, val, -c(ID))
BP <- ggplot(boxDat, aes(x = key, y = val)) + geom_boxplot()
ggBP <- ggplotly(BP)




# Since crosstalk's SharedData object was supplied to plot_ly() with a key of
# patient, it knows to highlight any lines/markers matching the selected patient(s).
# By default, the interaction type is "plotly_selected", which corresponds to
# click and drag mouse events. Plotly provides two types of drag modes: "lasso"
# and "select". For this type of plot, a rectangular selection seems more
# appropriate since one may want to see how patient(s) with a high/low response
# on a given visit respond during other visits.
layout(p, dragmode = "select")

# Other interaction types, beyond click and drag interactions, can also select
# value(s) of a SharedData key and are specified via the crosstalk() function.
# The first argument, `on`, sets the interaction type used to add values to the
# selection set. The second argument, `off`, sets the interaction required to
# clear the selection set and return to the original view. By default, a
# "plotly_relayout" event will clear the selection set. This event is triggered
# by clicking on the home icon in the mode bar, or double-clicking on the plot
# when in a zoom or pan dragmode. Some other sensible events for clearing the
# selection set are "plotly_deselect" and "plotly_doubleclick". Both events are
# triggered with a double click, but are dependant upon the current dragmode
# ("plotly_deselect" is triggered when in select/lasso dragmode and
# "plotly_doubleclick" when in zoom/pan dragmode).
p %>%
  crosstalk(on = "plotly_hover", off = "plotly_doubleclick") %>%
  layout(dragmode = "zoom")

# By default, all selections are transient, meaning prior selections are
# removed from the selection set before new selections are added. To prevent
# prior selections from being removed, simply set the persistent argument to
# `TRUE`.
crosstalk(p, on = "plotly_hover", persistent = TRUE)

p %>%
  crosstalk(on = "plotly_click", off = "plotly_deselect", persistent = TRUE) %>%
  layout(dragmode = "zoom")

# Sometimes its useful to compare two or more different selection sets.
# For example, how do patients with a high response on visit 1 compare to those
# with a low response? To make this sort of comparison, we can alter the color
# in multiple persistent selections. By setting the dynamic argument to `TRUE`
# a colourpicker htmlwidget (@colourpicker) will appear just above the plotly
# visualization. At any given time, the value of this widget controls the
# color of new selection(s).
crosstalk(p, on = "plotly_click", persistent = TRUE, dynamic = TRUE)

# By default, the colourpicker widget uses colors from the "Set1"
# colour brewer palette (@RColorBrewer), but any set of valid R colors can
# be supplied to the color argument.
colors <- RColorBrewer::brewer.pal(4, "Dark2")
crosstalk(p, on = "plotly_click", color = colors, dynamic = TRUE, persistent = TRUE)
