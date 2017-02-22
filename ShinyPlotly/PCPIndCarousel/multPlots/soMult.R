library(shiny)
library(plotly)
library(data.table)
library(dplyr)

ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("click"),
  plotlyOutput("plot2")
)

server <- function(input, output, session) {

  set.seed(1)
  dat <- data.frame(ID = paste0("ID",1:10), x = runif(10), y = runif(10))
  dat$ID <- as.character(dat$ID)

  output$plot <- renderPlotly({
    plot_ly(data = dat, x= ~x, y= ~y)
  })

  d <- reactive(event_data("plotly_selected"))

  output$click <- renderPrint({
    if (is.null(d())){
      "Click on a state to view event data"
    }
    else{
      d()$y
    }
  })

  output$plot2 <- renderPlotly({
    yVar <- d()$y
    plot_ly(x = c(4, 4), y = c(0, 10), mode = "lines") %>% add_trace(x = c(3, 5), y = c(yVar, yVar))
  })
}

shinyApp(ui, server)
