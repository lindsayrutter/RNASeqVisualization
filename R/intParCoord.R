devtools::install_github("cpsievert/pedestrians", repos = "http://cran.rstudio.com")
devtools::install_github("ropensci/plotly", repos = "http://cran.rstudio.com")
devtools::install_github("rstudio/crosstalk", repos = "http://cran.rstudio.com", force = TRUE)

library(leaflet)
library(plotly)
library(crosstalk)
library(htmltools)
library(tibble)
library(tidyr) #gather_

data(pedestrians, package = "pedestrians")
data(sensors, package = "pedestrians")
data(cog, package = "pedestrians")


data(diamonds, package="ggplot2")
diamonds.samp <- diamonds[sample(1:dim(diamonds)[1], 100), c(1, 5,6,8,9,10)]
p <- ggparcoord(data = diamonds.samp)

p <- diamonds.samp %>%
  plot_ly() %>%
  data.frame(., check.names = FALSE) %>%
  #mutate(rnames = rownames(.)) %>%
  gather_(., "variable", "value", setdiff(colnames(.), "rnames")) %>%
  ggplot(aes(variable, value, group = variable, text = variable)) +
  geom_line() + geom_point(size = 0.01) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(x = NULL, y = NULL)

p2 <- p %>%
  ggplotly(tooltip = "text") %>%
  layout(dragmode = "select") %>%
  crosstalk(dynamic = TRUE, persistent = TRUE)


########################################

library(plotly)
library(crosstalk)

d <- SharedData$new(txhousing, ~city)
p <- qplot(data = d, x = date, y = median, group = city, geom = "line")
ggplotly(p, tooltip = "city") %>%
  crosstalk(on = "plotly_hover", color = "red")



library(GGally)
iris$id <- seq_len(nrow(iris))
d <- SharedData$new(iris, ~id)
p <- ggpairs(d, aes(colour = Species), columns = 1:4)
ggplotly(p, c("x", "y", "colour"))

