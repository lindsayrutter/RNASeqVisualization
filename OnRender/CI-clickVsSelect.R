library(plotly)
library(GGally)
library(htmlwidgets)

ui <- shinyUI(fluidPage(
  sliderInput("threshold", "Threshold:", min = 0, max = 4, value=1, step=0.1),
  plotlyOutput("myPlot")
))

server <- shinyServer(function(input, output) {
  set.seed(1)
  dat <- data.frame(ID = paste0("ID",sample(c(1:10),10)), A=rnorm(10), B=rnorm(10), C=rnorm(10), D=rnorm(10), E=rnorm(10))
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

  # ggPS <- ggplotly(pS)
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

                  function range(start, stop, step){
                    var a=[start], b=start;
                    while(b<stop){b+=step;a.push(b)}
                    return a;
                  };

             len = Math.sqrt(document.getElementsByClassName('cartesianlayer')[0].childNodes.length);
             AxisNames = [];
             for (i = 1; i < (len+1); i++) {
                AxisNames.push(document.getElementsByClassName('infolayer')[0].childNodes[i].textContent);
             }
             noPoint = x.data.length;
             var SubPoints = [];
             var Traces = [];
             var i=0;
             var k=1;
             while ((i*len+k)<=Math.pow((len-1),2)) {
               while ((i+k)<len){
                 var selRows = [];
                 data.dat.forEach(function(row){
                 selRows.push(row)})
                 var xArr = [];
                 for (a=0; a<selRows.length; a++){
                  xArr.push(selRows[a][AxisNames[i]])
                 }
                 var yArr = [];
                 for (a=0; a<selRows.length; a++){
                  yArr.push(selRows[a][AxisNames[(len-k)]])
                 }
                 var keepIndex = [];
                 for (a=0; a<selRows.length; a++){
                  keepIndex.push(selRows[a]['ID'])
                 }
                 SubPoints.push(keepIndex);
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
                 Traces.push(tracePoints);
                 k++;
               }
               i++;
               k=1;
             }
             Plotly.addTraces(el.id, Traces);

            var idRows = []
            for (a=0; a<data.dat.length; a++){
             idRows.push(data.dat[a]['ID'])
            }


                  noPoint = x.data.length;


           el.on('plotly_selected', function(e) {

//if (x.data.length > noPoint){
  //Plotly.deleteTraces(el.id, range(noPoint, (noPoint+(len*(len-1)/2-1)), 1));
  //Plotly.deleteTraces(el.id);
//}

             numSel = e.points.length
             console.log(numSel)
             cN = e.points[0].curveNumber;

             var pointNumbers = [];
             for (a=0; a<numSel; a++){
              pointNumbers.push(e.points[a].pointNumber)
             }

             // Determine which subplot was selected
             subPlot = (cN - Math.pow(len,2))+1
            console.log(subPlot)

            var selData = []
            for (a=0; a<numSel; a++){
              selData.push(data.dat[idRows.indexOf(SubPoints[subPlot-1][pointNumbers[a]])])
            }

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
