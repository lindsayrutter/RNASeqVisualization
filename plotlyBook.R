p <- ggplot(txhousing, aes(date, median)) +
  geom_line(aes(group = city), alpha = 0.2)

subplot(
  p, ggplotly(p, tooltip = "city"),
  ggplot(txhousing, aes(date, median)) + geom_bin2d(),
  ggplot(txhousing, aes(date, median)) + geom_hex(),
  nrows = 2, shareX = TRUE, shareY = TRUE,
  titleY = FALSE, titleX = FALSE
)

library(dplyr)
tx <- group_by(txhousing, city)
# initiate a plotly object with date on x and median on y
p <- plot_ly(tx, x = ~date, y = ~median)
# plotly_data() returns data associated with a plotly object
plotly_data(p)

# When a data frame is associated with a plotly object, it allows us to manipulate the data underlying that object in the same way we would directly manipulate the data. Currently, plot_ly() borrows semantics from and provides special plotly methods for generic functions in the dplyr and tidyr packages. Most importantly, plot_ly() recognizes and preserves groupings created with dplyr’s group_by() function.

# add a line highlighting houston
add_lines(
  # plots one line per city since p knows city is a grouping variable
  add_lines(p, alpha = 0.2, name = "Texan Cities", hoverinfo = "none"),
  name = "Houston", data = filter(txhousing, city == "Houston")
)

#################### Easier way to write past two blocks ################
allCities <- txhousing %>%
  group_by(city) %>%
  plot_ly(x = ~date, y = ~median) %>%
  add_lines(alpha = 0.2, name = "Texan Cities", hoverinfo = "none")

allCities %>%
  filter(city == "Houston") %>%
  add_lines(name = "Houston")

# Sometimes the directed acyclic graph property of a pipeline can be too restrictive for certain types of plots. In this example, after filtering the data down to Houston, there is no way to recover the original data inside the pipeline. The add_fun() function helps to work-around this restriction – it works by applying a function to the plotly object, but does not affect the data associated with the plotly object.
allCities %>%
  add_fun(function(plot) {
    plot %>% filter(city == "Houston") %>% add_lines(name = "Houston")
  }) %>%
  add_fun(function(plot) {
    plot %>% filter(city == "San Antonio") %>%
      add_lines(name = "San Antonio")
  })

##########################################

layer_city <- function(plot, name) {
  plot %>% filter(city == name) %>% add_lines(name = name)
}

# reusable function for plotting overall median & IQR
layer_iqr <- function(plot) {
  plot %>%
    group_by(date) %>%
    summarise(
      q1 = quantile(median, 0.25, na.rm = TRUE),
      m = median(median, na.rm = TRUE),
      q3 = quantile(median, 0.75, na.rm = TRUE)
    ) %>%
    add_lines(y = ~m, name = "median", color = I("black")) %>%
    add_ribbons(ymin = ~q1, ymax = ~q3, name = "IQR", color = I("black"))
}

allCities %>%
  add_fun(layer_iqr) %>%
  add_fun(layer_city, "Houston") %>%
  add_fun(layer_city, "San Antonio")


# A layering function does not have to be a data-plot-pipeline itself. Its only requirement on a layering function is that the first argument is a plot object and it returns a plot object. This provides an opportunity to say, fit a model to the plot data, extract the model components you desire, and map those components to visuals. Furthermore, since plotly’s add_*() functions don’t require a data.frame, you can supply those components directly to attributes (as long as they are well-defined), as done in Figure 1.5 via the forecast package

library(forecast)
layer_forecast <- function(plot) {
  d <- plotly_data(plot)
  series <- with(d,
                 ts(median, frequency = 12, start = c(2000, 1), end = c(2015, 7))
  )
  fore <- forecast(ets(series), h = 48, level = c(80, 95))
  plot %>%
    add_ribbons(x = time(fore$mean), ymin = fore$lower[, 2],
                ymax = fore$upper[, 2], color = I("gray95"),
                name = "95% confidence", inherit = FALSE) %>%
    add_ribbons(x = time(fore$mean), ymin = fore$lower[, 1],
                ymax = fore$upper[, 1], color = I("gray80"),
                name = "80% confidence", inherit = FALSE) %>%
    add_lines(x = time(fore$mean), y = fore$mean, color = I("blue"),
              name = "prediction")
}

txhousing %>%
  group_by(city) %>%
  plot_ly(x = ~date, y = ~median) %>%
  add_lines(alpha = 0.2, name = "Texan Cities", hoverinfo = "none") %>%
  add_fun(layer_iqr) %>%
  add_fun(layer_forecast)


######################### Now look at ggplotly() #####################
p <- ggplot(fortify(gold), aes(x, y)) + geom_line()
gg <- ggplotly(p)
layout(gg, dragmode = "pan")

rangeslider(gg)

plotly_json(p)

# Generally speaking, the style() function is designed modify attribute values of trace(s) within a plotly object, which is primarily useful for customizing defaults produced via ggplotly()

style(p, hoverinfo = "none", traces = 2:3)

# Since ggplotly() returns a plotly object, and plotly objects can have data attached to them, it attaches data from ggplot2 layer(s) (either before or after summary statistics have been applied). Furthermore, since each ggplot layer owns a data frame, it is useful to have some way to specify the particular layer of data of interest, which is done via the ***layerData*** argument in ggplotly(). Also, when a particular layer applies a summary statistic (e.g., geom_bin()), or applies a statistical model (e.g., geom_smooth()) to the data, it might be useful to access the output of that transformation, which is the point of the ***originalData*** argument in ggplotly().
p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() + geom_smooth()
p %>%
  ggplotly(layerData = 2, originalData = FALSE) %>%
  plotly_data()
# The data shown above is the data ggplot2 uses to actually draw the fitted values (as a line) and standard error bounds (as a ribbon)

# Below leverages this data to add additional information about the model fit; in particular, it adds a vertical lines and annotations at the x-values that are associated with the highest and lowest amount uncertainty in the fitted values
p %>%
  ggplotly(layerData = 2, originalData = F) %>%
  add_fun(function(p) {
    p %>% slice(which.max(se)) %>%
      add_segments(x = ~x, xend = ~x, y = ~ymin, yend = ~ymax) %>%
      add_annotations("Maximum uncertainty", ax = 60)
  }) %>%
  add_fun(function(p) {
    p %>% slice(which.min(se)) %>%
      add_segments(x = ~x, xend = ~x, y = ~ymin, yend = ~ymax) %>%
      add_annotations("Minimum uncertainty")
  })

# In addition to leveraging output from StatSmooth, it is sometimes useful to leverage output of other statistics, especially for annotation purposes. Figure 1.11 leverages the output of StatBin to add annotations to a stacked bar chart.
p <- ggplot(diamonds, aes(cut, fill = clarity)) +
  geom_bar(position = "fill")

ggplotly(p, originalData = FALSE) %>%
  mutate(ydiff = ymax - ymin) %>%
  add_text(
    x = ~x, y = ~1 - (ymin + ymax) / 2,
    text = ~ifelse(ydiff > 0.02, round(ydiff, 2), ""),
    showlegend = FALSE, hoverinfo = "none",
    color = I("black"), size = I(9)
  )

# Note that, in this example, the add_text() layer takes advantage of ggplotly()'s ability to inherit aesthetics from the global mapping
p <- ggplot(MASS::geyser, aes(x = waiting, y = duration)) +
  geom_density2d()
ggplotly(p, originalData = FALSE) %>%
  group_by(piece) %>%
  slice(which.min(y)) %>%
  add_text(
    text = ~level, size = I(9), color = I("black"), hoverinfo = "none"
  )

############## Chapter 2 Notes ##############

# Three subplots of scatterplots
subplot(
  plot_ly(mpg, x = ~cty, y = ~hwy, name = "default"),
  plot_ly(mpg, x = ~cty, y = ~hwy) %>%
    add_markers(alpha = 0.2, name = "alpha"),
  plot_ly(mpg, x = ~cty, y = ~hwy) %>%
    add_markers(symbol = I(1), name = "hollow")
)

p <- plot_ly(mpg, x = ~cty, y = ~hwy, alpha = 0.5)
subplot(
  add_markers(p, color = ~cyl, showlegend = FALSE) %>%
    colorbar(title = "Viridis"),
  add_markers(p, color = ~factor(cyl))
)


################ Linking views without Shiny ################
# As far as ggplotly() and plot_ly() are concerned, SharedData object(s) act just like a data frame, but with a special key attribute attached to graphical elements.

library(crosstalk)
sd <- SharedData$new(txhousing, ~year)

# Brushing scatterplot matrix - takes 1.5 seconds or so
d <- SharedData$new(iris)
p <- GGally::ggpairs(d, aes(color = Species), columns = 1:4)
layout(ggplotly(p), dragmode = "select")

# Temporary versus persistant highlighting
sd <- SharedData$new(txhousing, ~city)
p <- ggplot(sd, aes(date, median)) + geom_line()
gg <- ggplotly(p, tooltip = "city")
highlight(gg, on = "plotly_hover", dynamic = TRUE)
highlight(gg, on = "plotly_hover", dynamic = TRUE, persistent = TRUE)

############# Nested selections in ggplotly() #################
# Graphical element to be tied to multiple observations. For example, a line of fitted values from a linear model is inherently connected to the observations used to fit the model. In fact, any graphical summary (e.g. boxplot, histogram, density, etc.) can be linked back the original data used to derive them. Especially when comparing multiple summaries, it can be useful to highlight group(s) of summaries, as well as the raw data that created them. Figure 4.13 uses ggplotly()’s built-in support for linking graphical summaries with raw data to enable highlighting of linear models.16 Furthermore, notice how there are actually two levels of selection in Figure 4.13 – when hovering over a single point, just that point is selected, but when hovering over a fitted line, all the observations tied to that line are selected.
# # if you don't want to highlight individual points, you could specify
# `class` as the key variable here, instead of the default (rownames)
m <- SharedData$new(mpg)
p <- ggplot(m, aes(displ, hwy, colour = class)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "lm")
ggplotly(p) %>% highlight("plotly_hover")

# Simple highlighting link between two subplots
d <- SharedData$new(mtcars)
subplot(
     qplot(data = d, x = mpg, y = wt),
     qplot(data = d, x = mpg, y = vs)
  )

# Brushing scatterplot matrices takes several seconds
pm <- GGally::ggpairs(iris)
ggplotly(pm)

################### Link line plots ###################
library(dplyr)
top5 <- txhousing %>%
  group_by(city) %>%
  summarise(m = mean(sales, na.rm = TRUE)) %>%
  arrange(desc(m)) %>%
  top_n(5)

p <- semi_join(txhousing, top5, by = "city") %>%
  plot_ly(x = ~date, y = ~median)

subplot(
  add_lines(p, color = ~city),
  add_lines(p, linetype = ~city),
  shareX = TRUE, nrows = 2
)

# Fit ribbon
m <- lm(mpg ~ wt, data = mtcars)
broom::augment(m) %>%
  plot_ly(x = ~wt, showlegend = FALSE) %>%
  add_markers(y = ~mpg, color = I("black")) %>%
  add_ribbons(ymin = ~.fitted - 1.96 * .se.fit, 
              ymax = ~.fitted + 1.96 * .se.fit, color = I("gray80")) %>%
  add_lines(y = ~.fitted, color = I("steelblue"))

# Add polygons
map_data("world", "canada") %>%
  group_by(group) %>%
  plot_ly(x = ~long, y = ~lat, alpha = 0.2) %>%
  add_polygons(hoverinfo = "none", color = I("black")) %>%
  add_markers(text = ~paste(name, "<br />", pop), hoverinfo = "text", color = I("red"), data = maps::canada.cities) %>%
  layout(showlegend = FALSE)


dat <- map_data("world", "canada") %>% group_by(group)

# geo() is the only object type which supports different map projections
map3 <- plot_geo(dat, x = ~long, y = ~lat) %>% 
  add_markers(size = I(1)) %>%
  add_segments(x = -100, xend = -50, y = 50, 75) %>%
  layout(geo = list(projection = list(type = "mercator")))

# histogram2d()
p <- plot_ly(diamonds, x = ~log(carat), y = ~log(price))
subplot(
  add_histogram2d(p) %>%
    colorbar(title = "default") %>%
    layout(xaxis = list(title = "default")),
  add_histogram2d(p, zsmooth = "best") %>%
    colorbar(title = "zsmooth") %>%
    layout(xaxis = list(title = "zsmooth")),
  add_histogram2d(p, nbinsx = 60, nbinsy = 60) %>%
    colorbar(title = "nbins") %>%
    layout(xaxis = list(title = "nbins")),
  shareY = TRUE, titleX = TRUE
)

################## subplots ########################


