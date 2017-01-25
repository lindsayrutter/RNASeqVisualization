# Shows that box select in one subplot of points will highlight in other subplots

library(shiny)
library(plotly)
library(data.table)
library(ggplot2)
library(GGally)

ui <- fluidPage(
  plotlyOutput("plot", height = "500px")#,
 # verbatimTextOutput("click")
)

server <- function(input, output, session) {
  dat = mtcars[,1:3]
  p <- ggpairs(dat)

  output$plot <- renderPlotly({
    ggplotly(p)
  })
}

shinyApp(ui, server)
