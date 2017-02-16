library(shiny)
library(plotly)
library(data.table)

d <- ggplot(diamonds, aes(carat, price))
d <- d + geom_hex()
pd <- ggplotly(d)
plotly_data(pd)


plot_ly(diamonds, x = ~wt, y = ~mpg, color = ~factor(vs)) %>%
  add_markers(text = ~search, hoverinfo = "x+y") %>%
  onRender("
