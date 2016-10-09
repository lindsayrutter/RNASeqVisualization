library(plotly)

p <- plot_ly(economics,
             type = "scatter",
             x = ~date,
             y = ~uempmed,
             name = "unemployment",
             marker = list(color="rgb(16, 32, 77)"))



set.seed(100)
d <- diamonds[sample(nrow(diamonds), 1000), ]
p <- ggplot(data = d, aes(x = carat, y = price)) +
  geom_point(aes(text = paste("Clarity:", clarity))) +
  facet_wrap(~ cut)
ggplotly(p)
