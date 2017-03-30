library(plotly)
library(GGally)
library(htmlwidgets)

ui <- shinyUI(fluidPage(
  plotlyOutput("myPlot")
))

server <- shinyServer(function(input, output) {
  p <- ggplot(mtcars, aes(x = wt, y = mpg))  + xlim(10,40) +ylim(0,10)
  ggPS <- ggplotly(p)

  output$myPlot <- renderPlotly(ggPS %>%
onRender("
   function(el, x, data) {

   var xArr = [];
   var yArr = [];
   for (a=0; a<data.wt.length; a++){
   xArr.push(data.wt[a])
   yArr.push(data.mpg[a])
   }

   Traces=[]
   var tracePoints = {
   x: yArr,
   y: xArr,
   hoverinfo: 'none',
   mode: 'markers',
   marker: {
   color: 'black',
   size: 4
   }
   };
   Traces.push(tracePoints);
   Plotly.addTraces(el.id, Traces);

   el.on('plotly_selected', function(e) {
   var numSel = e.points.length
   var xSel = [];
   var ySel = [];
   for (a=0; a<numSel; a++){
   xSel.push(e.points[a].x)
   ySel.push(e.points[a].y)
   }

   var trace = {
   x: xSel,
   y: ySel,
   mode: 'markers',
   marker: {
   color: 'red',
   size: 4
   },
   hoverinfo: 'none'
   };
   Traces.push(trace);
   Plotly.addTraces(el.id, Traces);

   })

   }
   ", data = list(dat= mtcars, wt=mtcars$wt, mpg=mtcars$mpg)))})

shinyApp(ui, server)
