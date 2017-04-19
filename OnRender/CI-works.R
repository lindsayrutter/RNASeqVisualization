library(plotly)
library(GGally)
library(htmlwidgets)
library(shiny)

ui <- shinyUI(fluidPage(
  sliderInput("thresh", "Threshold:", min = 0, max = 3, value=1, step=1),
  plotlyOutput("myPlot"),
  verbatimTextOutput("selectedValues")
))

server <- shinyServer(function(input, output) {
  thresh <- reactive(input$thresh)

  set.seed(1)
  dat <- data.frame(Row = paste0("Row",sample(c(1:20),20)), A=4*rnorm(20), B=4*rnorm(20))
  dat$Row <- as.character(dat$Row)

  minVal = min(dat[,-1])
  maxVal = max(dat[,-1])

  gg <- ggplot(data = dat, aes(x=A, y=B)) + coord_cartesian(xlim = c(minVal, maxVal), ylim = c(minVal, maxVal))

  ggY <- ggplotly(gg)

  output$myPlot <- renderPlotly(ggY %>% onRender("
   function(el, x, data) {
   var Points = [];
   var Traces = [];
   var selRows = [];

   data.dat.forEach(function(row){
   if(Math.abs(row['B']) > data.thresh) selRows.push(row);});

   var xArr = [];
   var yArr = [];
   var keepIndex = [];
   for (a=0; a<selRows.length; a++){
   xArr.push(selRows[a]['A'])
   yArr.push(selRows[a]['B'])
   keepIndex.push(selRows[a]['Row'])
   }
   // Points contains row indices that are plotted (outside threshold)
   Points.push(keepIndex);
console.log(['Points', Points])

   // Add points above the threshold in black
   var tracePoints = {
   x: xArr,
   y: yArr,
   hoverinfo: 'none',
   mode: 'markers',
   marker: {
   color: 'black',
   size: 4
   }
   };

   // Add upper horizontal line of gray box
   var hiLine = {
   x: [-15,15],
   y: [data.thresh,data.thresh],
   mode: 'lines',
   line: {
   color: 'gray',
   width: 1
   },
   opacity: 0.25,
   hoverinfo: 'none'
   };

   // Add lower horizontal line of gray box
   var lowLine = {
   x: [-15,15],
   y: [-1*data.thresh,-1*data.thresh],
   mode: 'lines',
   fill: 'tonexty',
   line: {
   color: 'gray',
   width: 1
   },
   opacity: 0.25,
   hoverinfo: 'none'
   };

   Traces.push(tracePoints);
   Traces.push(hiLine);
   Traces.push(lowLine);
   Plotly.addTraces(el.id, Traces);
   console.log(['Traces', Traces])

   var idRows = []
   for (a=0; a<data.dat.length; a++){
   idRows.push(data.dat[a]['Row'])
   }
console.log(['idRows', idRows])
   var nseltrace = 0;
   el.on('plotly_selected', function(e) {
   console.log(e.points)
   numSel = e.points.length

   var pointNumbers = [];
   var selData = [];
   for (a=0; a<numSel; a++){
   if (e.points[a].curveNumber==1){  // remove duplicates of same point being selected twice (because already red)
   pointNumbers.push(e.points[a].pointNumber)
   selData.push(data.dat[idRows.indexOf(Points[0][pointNumbers[a]])])
   }
   }
   Shiny.onInputChange('selData', selData);
   var Traces = [];
   var xArr = [];
   var yArr = [];
   for (a=0; a<selData.length; a++){
   xArr.push(selData[a]['A'])
   yArr.push(selData[a]['B'])
   }

   // Add user-selected points in red
   var traceRed = {
   x: xArr,
   y: yArr,
   mode: 'markers',
   marker: {
   color: 'red',
   size: 4
   },
   hoverinfo: 'none'
   };

   console.log(['TracesB4Delete', Traces])
   if (nseltrace>0){
   Plotly.deleteTraces(el.id,-1)
   }
   console.log(['TracesAfterDelete', Traces])
   Traces.push(traceRed);
   nseltrace = nseltrace+1

   Plotly.addTraces(el.id, Traces);
   console.log(['TracesAfterAdd', Traces])
   })

   }", data = list(dat=dat, thresh=thresh())))

  selData <- reactive({
    req(input$selData)
    rawc <- input$selData
    df <- data.frame(t(matrix(rawc,nrow=3)))
    names(df) <- names(rawc)[1:3]
    df
  })
  output$selectedValues <- renderPrint({selData()})

})

shinyApp(ui, server)
