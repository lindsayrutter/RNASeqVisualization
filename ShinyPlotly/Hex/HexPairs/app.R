library(shiny)
library(plotly)
library(data.table)
library(GGally)

ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("click")
)

server <- function(input, output, session) {

  set.seed(1)
  dat <- data.frame(ID = paste0("ID",1:100), A = runif(100), B = runif(100), C = runif(100), D = runif(100), E = runif(100))
  dat$ID <- as.character(dat$ID)
  data <- dat

  my_fn <- function(data, mapping, ...){
    p <- ggplot(data = data, mapping = mapping) + geom_hex(binwidth=0.1) + geom_abline(intercept = 0, color = "red", size = 0.25)
    p
  }

  p <- ggpairs(data[,2:6], lower = list(continuous = my_fn))
  myMax = max(data[,2:6])
  myMin = min(data[,2:6])
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
