
p <- plot_ly(economics,
             type = "scatter",
             x = ~date,
             y = ~uempmed,
             name = "unemployment",
             marker = list(color="rgb(16, 32, 77)"))

