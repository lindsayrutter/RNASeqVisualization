ui <- basicPage(
  plotlyOutput("plot1")#,
  #verbatimTextOutput("info")
)

server <- function(input, output) {

  p <- qplot(mtcars$wt, mtcars$mpg)
  gp <- ggplotly(p)

  output$plot1 <- renderPlotly({
  gp %>% onRender("
    function(el, x, data) {

el.on('plotly_selected', function(e) {
  var xMin = e.range.x[0]
  var xMax = e.range.x[1]
  var yMin = e.range.y[0]
  var yMax = e.range.y[1]
  console.log(xMin)
  console.log(xMax)
  console.log(yMin)
  console.log(yMax)
})
    }")})
}


shinyApp(ui, server)

# paste0('brush: ', xy_range_str(input$plot_brush))
