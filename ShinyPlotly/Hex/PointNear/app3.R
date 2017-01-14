library(GGally)
library(dplyr)

ui <- basicPage(
  plotOutput("plot1", click = "plot_click"),
  verbatimTextOutput("info")
)

server <- function(input, output) {
  data <- select(mtcars,wt,mpg)

  output$plot1 <- renderPlot({
    ggpairs(data)
  })

  output$info <- renderPrint({
    nearPoints(mtcars, input$plot_click, xvar = "wt", yvar = "mpg")
  })
}

shinyApp(ui, server)
