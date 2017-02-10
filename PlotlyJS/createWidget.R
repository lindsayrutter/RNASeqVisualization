# Create a new widget

devtools::create("mywidget")               # create package using devtools
setwd("mywidget")                          # navigate to package dir
htmlwidgets::scaffoldWidget("mywidget")    # create widget scaffolding
devtools::install()                        # install the package so we can try it
