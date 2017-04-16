library(plotly)
library(GGally)
library(htmlwidgets)

ui <- shinyUI(fluidPage(
  sliderInput("threshold", "Threshold:", min = 0, max = 4, value=0.7, step=0.1),
  plotlyOutput("myPlot")
))

server <- shinyServer(function(input, output) {
  set.seed(1)
  dat <- data.frame(ID = paste0("ID",sample(c(1:20),20)), A=rnorm(20), B=rnorm(20))
  dat$ID <- as.character(dat$ID)

  minVal = min(dat[,-1])
  maxVal = max(dat[,-1])
  # Designate end points of lines to be drawn
  minLine = minVal - 5*(maxVal-minVal)
  maxLine = maxVal + 5*(maxVal-minVal)
  cv = 1

  my_fn <- function(data, mapping, ...){
    x = data[,c(as.character(mapping$x))]
    y = data[,c(as.character(mapping$y))]
    p <- ggplot(data = dat, aes(x=x, y=y)) + coord_cartesian(xlim = c(minVal, maxVal), ylim = c(minVal, maxVal))
    p
  }

  p <- ggpairs(dat[,-1], lower = list(continuous = my_fn))
  ggPS <- ggplotly(p)

  myLength <- length(ggPS[["x"]][["data"]])
  for (i in 1:myLength){
    item =ggPS[["x"]][["data"]][[i]]$text[1]
    if (!is.null(item))
      if (!startsWith(item, "co")){
        ggPS[["x"]][["data"]][[i]]$hoverinfo <- "none"
      }
  }

  output$myPlot <- renderPlotly(ggPS %>%
onRender("
       function(el, x, data) {
       console.log('start')

       len = Math.sqrt(document.getElementsByClassName('cartesianlayer')[0].childNodes.length);
       AxisNames = [];
       for (i = 1; i < (len+1); i++) {
         AxisNames.push(document.getElementsByClassName('infolayer')[0].childNodes[i].textContent);
       }

       var SubPoints = [];
       var Traces = [];
       var i=0;
       var k=1;
       while ((i*len+k)<=Math.pow((len-1),2)) {
         while ((i+k)<len){
           var selRows = [];
           data.dat.forEach(function(row){
             if(Math.abs(row[AxisNames[i]]-row[AxisNames[(len-k)]]) > Math.sqrt(2)*data.val){
             selRows.push(row);
           }})
           var xArr = [];
           var yArr = [];
           var keepIndex = [];
           for (a=0; a<selRows.length; a++){
             xArr.push(selRows[a][AxisNames[i]])
             yArr.push(selRows[a][AxisNames[(len-k)]])
             keepIndex.push(selRows[a]['ID'])
           }
           SubPoints.push(keepIndex);
//console.log(['SubPoints', SubPoints])

           // Plot all points outside CI
           var tracePoints = {
             x: xArr,
             y: yArr,
             hoverinfo: 'none',
             mode: 'markers',
             marker: {
               color: 'black',
               size: 4
             },
             xaxis: 'x' + (i+1),
             yaxis: 'y' + (i*len+k)
           };

           // Plot upper line of confidence interval
           var traceHiLine = {
             x: [data.minLine, data.maxLine - Math.sqrt(2)*data.val],
             y: [data.minLine + Math.sqrt(2)*data.val, data.maxLine],
             mode: 'lines',
             line: {
               color: 'gray',
               width: 1
             },
             opacity: 0.25,
             xaxis: 'x' + (i+1),
             yaxis: 'y' + (i*len+k)
           }

           // Plot lower line of confidence interval (and fill part between upper and lower confidence interval lines with gray)
             var traceLoLine = {
             x: [data.minLine + Math.sqrt(2)*data.val, data.maxLine],
             y: [data.minLine, data.maxLine - Math.sqrt(2)*data.val],
             mode: 'lines',
             fill: 'tonexty',
             line: {
               color: 'gray',
               width: 1
             },
             opacity: 0.25,
             xaxis: 'x' + (i+1),
             yaxis: 'y' + (i*len+k)
           }
           Traces.push(tracePoints);
           Traces.push(traceHiLine);
           Traces.push(traceLoLine);
           k++;
         }
         i++;
         k=1;
       }
       Plotly.addTraces(el.id, Traces);

       var idRows = [] // contains ID for each row of dataframe (can put this outside so it is not rerun each time CV changes) ************************
       for (a=0; a<data.dat.length; a++){
         idRows.push(data.dat[a]['ID'])
       }

       el.on('plotly_selected', function(e) {
console.log(el);

       numSel = e.points.length
//console.log(numSel)
       cN = e.points[0].curveNumber;

       var pointNumbers = [];
       for (a=0; a<numSel; a++){
       pointNumbers.push(e.points[a].pointNumber)
       }

       // Determine which subplot was selected
       subPlot = (cN - Math.pow(len,2))/3+1

//console.log(pointNumbers.length)
       var selData = []
       for (a=0; a<pointNumbers.length; a++){
//console.log(data.dat[idRows.indexOf(SubPoints[subPlot-1][pointNumbers[a]])])
       selData.push(data.dat[idRows.indexOf(SubPoints[subPlot-1][pointNumbers[a]])])
       }
//console.log(selData)


       var Traces = [];
       var i=0;
       var k=1;
       while ((i*len+k)<=Math.pow((len-1),2)) {
       var xArr = [];
       for (a=0; a<selData.length; a++){
       xArr.push(selData[a][AxisNames[i]])
       }
       while ((i+k)<len){
       var yArr = [];
       for (a=0; a<selData.length; a++){
       yArr.push(selData[a][AxisNames[(len-k)]])
       }
       var trace = {
       x: xArr,
       y: yArr,
       mode: 'markers',
       marker: {
       color: 'red',
       size: 4
       },
       xaxis: 'x' + (i+1),
       yaxis: 'y' + (i*len+k),
       hoverinfo: 'none'
       };
       Traces.push(trace);
       k++;
       }
       i++;
       k=1;
       }
       Plotly.addTraces(el.id, Traces);
       })

       }
       ", data = list(dat=dat, val = input$threshold, minLine=minLine, maxLine=maxLine)))})

shinyApp(ui, server)
