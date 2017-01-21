# This script shows that deletion of points in the ggpairs causes the problem with squashing subplots together

library(shiny)
library(plotly)
library(data.table)
library(GGally)

ui <- fluidPage(
  plotlyOutput("plot2", height = "300px"),
  plotlyOutput("plot", height = "300px"),
  verbatimTextOutput("click")
)

server <- function(input, output, session) {

  # If change to set.seed(2), then it starts working
  set.seed(5)
  dat <- data.frame(ID = paste0("ID",1:10), A = runif(10), B = runif(10), C = runif(10), D = runif(10), E = runif(10))
  dat$ID <- as.character(dat$ID)
  data <- dat

  ciVal = 0.5
  myMax = max(data[,2:6])
  myMin = min(data[,2:6])

  # Works okay!
  my_fn <- function(data, mapping, ...){
    x <- data[,c(as.character(mapping$x))]
    y <- data[,c(as.character(mapping$y))]
    df <- data.frame(x,y)
    p <- ggplot(data = df, aes(x=x,y=y)) + geom_point(size=0.5) + geom_abline(intercept = 0, color = "red", size = 0.25) + geom_abline(intercept = ciVal, color ="blue", size = 0.25) + geom_abline(intercept = -1*ciVal, color ="blue", size = 0.25) + scale_x_continuous(limits = c(myMin, myMax)) + scale_y_continuous(limits = c(myMin, myMax))
    p
  }

  # Doesn't work
  my_fn2 <- function(data, mapping, ...){
    x <- data[,c(as.character(mapping$x))]
    y <- data[,c(as.character(mapping$y))]
    keep <- sign(resid(lm(y-x-ciVal ~ 0)))==1 | sign(resid(lm(y-x+ciVal ~ 0)))==-1
    df <- data.frame(x = x[keep], y = y[keep])
    p2 <- ggplot(data = df, aes(x=x,y=y)) + geom_point(size=0.5) + geom_abline(intercept = 0, color = "red", size = 0.25) + geom_abline(intercept = ciVal, color ="blue", size = 0.25) + geom_abline(intercept = -1*ciVal, color ="blue", size = 0.25) + scale_x_continuous(limits = c(myMin, myMax)) + scale_y_continuous(limits = c(myMin, myMax))
    p2
  }

  p <- ggpairs(data[,2:6], lower = list(continuous = my_fn))
  p2 <- ggpairs(data[,2:6], lower = list(continuous = my_fn2))

  output$plot <- renderPlotly({
    ggplotly(p)
  })

  output$plot2 <- renderPlotly({
    ggplotly(p2)
  })

  d <- reactive(event_data("plotly_click"))

  output$click <- renderPrint({
    if (is.null(d())){
      "Click on a state to view event data"
    }
    else{
      str(d())
      d()$curveNumber
      data[d()$curveNumber,]
      data[d()$curveNumber,]$ID
    }
  })
}

shinyApp(ui, server)
