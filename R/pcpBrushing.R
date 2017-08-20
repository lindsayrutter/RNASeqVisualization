#http://www.buildingwidgets.com/blog/2015/1/30/week-04-interactive-parallel-coordinates-1

devtools::install_github("timelyportfolio/parcoords", force=TRUE)
library(parcoords)

parcoords(
  mtcars, brush = "1d-axes", # 2d-strums are really neat
  reorderable = TRUE
)

data( diamonds, package = "ggplot2" )
parcoords(
  diamonds[sample(1:nrow(diamonds),1000),]
  , rownames = F # turn off rownames from the data.frame
  , brushMode = "2D-strums"
  , reorderable = T
  , queue = T
  , color = RColorBrewer::brewer.pal(4,"BuPu")[4]
)
