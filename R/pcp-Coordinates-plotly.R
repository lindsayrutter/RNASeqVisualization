library(ggplot2)

ui <- basicPage(
  plotlyOutput("plot1")#,
  #verbatimTextOutput("info")
)

server <- function(input, output) {
  # For some reason, will only allow box select if use geom_blank() with qplot() instead of ggplot()
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point(alpha=0)
  gp <- ggplotly(p)

  set.seed(1)
  xArr = c(2, 3, 4, 5)
  yArr = sample(seq(10,33,1), size=40, replace=TRUE)

  output$plot1 <- renderPlotly({
  gp %>% onRender("
    function(el, x, data) {
    Traces=[]
    for (a=0; a<10; a++){
      var tracePCP = {
        x: data.xArr,
        y: data.yArr.slice(a*4, (a+1)*4),
        mode: 'lines',
        line: {
          color: 'orange',
          width: 1
        },
        opacity: 0.9,
      }
      Traces.push(tracePCP);
    }
    Plotly.addTraces(el.id, Traces);

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
    }", data = list(xArr=xArr, yArr=yArr))})
}

shinyApp(ui, server)

