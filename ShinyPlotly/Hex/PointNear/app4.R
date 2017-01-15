ui <- basicPage(
  plotOutput("plot1", click = "plot_click"),
  verbatimTextOutput("info")
)

server <- function(input, output) {

  p <- ggplot(mtcars, aes(x=wt, y=mpg)) + geom_hex()
  pB <- ggplot_build(p)

  output$plot1 <- renderPlot({
    pB
  })

  output$info <- renderPrint({
    nearPoints(mtcars, input$plot_click, xvar = "wt", yvar = "mpg")
  })
}

shinyApp(ui, server)
