library(shiny)
library(plotly)
library(data.table)

server <- function(input, output, session) {

  set.seed(1)
  dat <- data.frame(ID = paste0("ID",1:100), A = runif(100), B = runif(100))
  dat$ID <- as.character(dat$ID)
  data <- dat

  p <- qplot(x=A, y=B, data=dat)

  output$plot <- renderPlotly({
    ggplotly(p)
  })

  d <- reactive(event_data("plotly_click"))

  output$click <- renderPrint({
    if (is.null(d())){
      "Click on a state to view event data"
    }
    else{
      str(d())
      d()$curveNumber
      d()$pointNumber
      d()$x
      data[d()$curveNumber,]
      data[d()$curveNumber,]$ID
      as.list(d())
    }
  })
}







####################### Even smaller MWE #######################
# library(shiny)
# library(plotly)
# library(data.table)
#
# server <- function(input, output, session) {
#
#   set.seed(1)
#   dat <- data.frame(ID = paste0("ID",1:20), A = runif(20), B = runif(20))
#   dat$ID <- as.character(dat$ID)
#   data <- dat
#
#   my_fn <- function(data, mapping, ...){
#     p <- ggplot(data = data, mapping = mapping) + geom_hex(binwidth=0.1) + geom_abline(intercept = 0, color = "red", size = 0.25)
#     p
#   }
#
#   p <- ggpairs(data[,2:3], lower = list(continuous = my_fn))
#   myMax = max(data[,2:3])
#   myMin = min(data[,2:3])
#   p2 <- p
#   for(i in 2:p$nrow) {
#     for(j in 1:(i-1)) {
#       p2[i,j] <- p[i,j] +
#         scale_x_continuous(limits = c(myMin, myMax)) +
#         scale_y_continuous(limits = c(myMin, myMax))
#     }
#   }
#
#   output$plot <- renderPlotly({
#     p2G <- ggplotly(p2)
#     delBoth = c()
#     for (i in 1:length(p2G$x$data)){
#       if (!is.null(p2G$x$data[[i]]$text[1])){
#         if (startsWith(p2G$x$data[[i]]$text[1], "density") ||
#             startsWith(p2G$x$data[[i]]$text[1], "intercept"))
#           delBoth = c(delBoth, i)
#
#       }
#     }
#     delHover = c()
#     for (i in 1:length(p2G$x$data)){
#       if (!is.null(p2G$x$data[[i]]$text[1])){
#         if (startsWith(p2G$x$data[[i]]$text[1], "Corr"))
#           delHover = c(delHover, i)
#
#       }
#     }
#     for (x in c(delBoth, delHover)) p2G$x$data[[x]]$hoverinfo <- "none"
#     for (x in delBoth) p2G$x$data[[x]]$text <- NULL
#     p2G
#   })
#
#   d <- reactive(event_data("plotly_click"))
#
#   output$click <- renderPrint({
#     if (is.null(d())){
#       "Click on a state to view event data"
#     }
#     else{
#       str(d())
#       d()$curveNumber
#       data[d()$curveNumber,]
#       data[d()$curveNumber,]$ID
#     }
#   })
# }
