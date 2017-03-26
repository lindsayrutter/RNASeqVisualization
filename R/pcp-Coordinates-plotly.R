library(ggplot2)

ui <- basicPage(
  plotlyOutput("plot1")#,
  #verbatimTextOutput("info")
)

server <- function(input, output) {
  # For some reason, will only allow box select if use geom_blank() with qplot() instead of ggplot()
  #p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_blank()
  #gp <- ggplotly(p)
  p <- qplot(mtcars$wt, mtcars$mpg) + geom_blank()
  gp <- ggplotly(p)

  set.seed(1)
  xArr = c(2, 3, 4, 5)
  yArr = sample(seq(10,33,1), size=40, replace=TRUE)

  output$plot1 <- renderPlotly({
  gp %>% onRender("
    function(el, x, data) {
 //here1

  el.on('plotly_selected', function(e) {
    var xMin = e.range.x[0]
    var xMax = e.range.x[1]
    var yMin = e.range.y[0]
    var yMax = e.range.y[1]
    console.log(xMin)
    console.log(xMax)
    console.log(yMin)
    console.log(yMax)

  // here2
  //Traces.push(traceHiLine);
  //Plotly.addTraces(el.id, Traces);
  })
    }", data = list(xArr=xArr, yArr=yArr))})
}


shinyApp(ui, server)

# paste0('brush: ', xy_range_str(input$plot_brush))
