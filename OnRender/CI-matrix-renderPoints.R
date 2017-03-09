library(plotly)
library(GGally)
library(htmlwidgets)

ui <- shinyUI(fluidPage(
  sliderInput("treshold", "Threshold:", min = 0, max = 4, value=1),
  plotlyOutput("myPlot")
))

server <- shinyServer(function(input, output) {
  set.seed(1)
  dat <- data.frame(ID = paste0("ID",sample(c(1:20),20)), A=rnorm(20), B=rnorm(20), C=rnorm(20), D=rnorm(20), E=rnorm(20))
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

  # pS <- p
  # for(i in 2:p$nrow) {
  #   for(j in 1:(i-1)) {
  #     pS[i,j] <- p[i,j] +
  #       coord_cartesian(xlim = c(minVal, maxVal), ylim = c(minVal, maxVal))
  #   }
  # }

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

  output$myPlot <- renderPlotly(ggplotly(ggPS) %>%
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
             if(Math.abs(row[AxisNames[i]]-row[AxisNames[(len-k)]]) > Math.sqrt(2)*data.val){
               selRows.push(row);
             }})
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
             console.log(SubPoints)




           el.on('plotly_selected', function(e) {
             //console.log(e)
             numSel = e.points.length
             //console.log(numSel)
             //console.log(e.points[0].pointNumber)

             //var selX = [];
             //for (a=0; a<numSel; a++){
             //selX.push(e.points[a].x)
             //}
             //var selY = [];
             //for (a=0; a<numSel; a++){
             //selY.push(e.points[a].y)
             //}

             //console.log(selX);
             //console.log(selY);

             //var selPoints = {
             //x: selX,
             //y: selY,
             //hoverinfo: 'none',
             //mode: 'markers',
             //marker: {
             //color: 'red',
             //size: 4
             //}
             //}

             //Plotly.addTraces(el.id, selPoints);
             })


             }
             ", data = list(dat=dat, val = input$treshold, minLine=minLine, maxLine=maxLine))
)})

shinyApp(ui, server)
