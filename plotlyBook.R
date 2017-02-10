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



