library(shiny)
library(plotly)
library(data.table)

server <- function(input, output, session) {

  set.seed(1)
  load("leavesDat.Rda")
  data$ID <- as.character(data$ID)

  my_fn <- function(data, mapping, ...){
    p <- ggplot(data = data, mapping = mapping) + geom_hex(binwidth=1) + geom_abline(intercept = 0, color = "red", size = 0.25)
    p
  }

  p <- ggpairs(data[,2:7], lower = list(continuous = my_fn))
  myMax = max(data[,2:7])
  #myMin = min(data[,2:7])
  myMin = min(data[,2:7])
  pS <- p
  for(i in 2:p$nrow) {
    for(j in 1:(i-1)) {
      pS[i,j] <- p[i,j] +
        scale_x_continuous(limits = c(myMin, myMax)) +
        scale_y_continuous(limits = c(myMin, myMax))
    }
  }

  output$plot <- renderPlotly({
    ggplotly(pS)
  })

  # no work
  #d <- reactive(event_data("plotly_selected"))
  # all say 1 obs of 1 variable, curveNumber: int x
  d <- reactive(event_data("plotly_click"))
  # same as plotly_click
  #d <- reactive(event_data("plotly_hover"))
  #d <- reactive(event_data("plotly_relayout"))

  output$click <- renderPrint({
    if (is.null(d())){
      "Click on a state to view event data"
    }
    else{
      str(d())
      d()$curveNumber
      data[d()$curveNumber,]
      data[d()$curveNumber,]$ID
      # d()$pointNumber
      # dat[(d()$pointNumber+1),]
      # dat[(d()$pointNumber+1),]$ID
      # str(dat[(d()$pointNumber+1),]$ID)
    }
  })

  # # Convert DF from scatterplot to PCP
  # datt <- data.frame(t(dat))
  # names(datt) <- as.matrix(datt[1, ])
  # datt <- datt[-1, ]
  # datt[] <- lapply(datt, function(x) type.convert(as.character(x)))
  # setDT(datt, keep.rownames = TRUE)[]
  # colnames(datt)[1] <- "x"
  # dat_long <- melt(datt, id.vars ="x" )
  #
  # output$plot2 <- renderPlotly({
  #   plot_ly(dat_long[dat_long$variable %in% dat[(d()$pointNumber+1),]$ID,], x= ~x, y= ~value, type = 'scatter', mode = 'lines+markers', color = ~variable)  %>% layout(dragmode="box", showlegend = FALSE)
  # })

}







