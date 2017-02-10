# Create a new widget

devtools::create("mywidget")               # create package using devtools
setwd("mywidget")                          # navigate to package dir
htmlwidgets::scaffoldWidget("mywidget")    # create widget scaffolding
devtools::install()                        # install the package so we can try it

# This creates a simple widget that takes a single text argument and displays that text within the widgets HTML element. You can try it like this:

library(mywidget)
mywidget("hello, world")
