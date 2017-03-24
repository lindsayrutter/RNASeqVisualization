library(plotly)
library(GGally)
library(htmlwidgets)

set.seed(1)
dat <- data.frame(ID = paste0("ID",sample(c(1:20),20)), A=rnorm(20), B=rnorm(20))
dat$ID <- as.character(dat$ID)

minVal = min(dat[,-1])
maxVal = max(dat[,-1])

p <- ggplot(data = dat, aes(x=A, y=B)) + coord_cartesian(xlim = c(minVal, maxVal), ylim = c(minVal, maxVal))

ggPS <- ggplotly(p)

myLength <- length(ggPS[["x"]][["data"]])
for (i in 1:myLength){
  item =ggPS[["x"]][["data"]][[i]]$text[1]
  if (!is.null(item))
    if (!startsWith(item, "co")){
      ggPS[["x"]][["data"]][[i]]$hoverinfo <- "none"
    }
}

ggPS %>% onRender("
       function(el, x, data) {

       var Points = [];
       var Traces = [];
       var selRows = [];
       data.dat.forEach(function(row){selRows.push(row);})
       var xArr = [];
       var yArr = [];
       var keepIndex = [];
       for (a=0; a<selRows.length; a++){
         xArr.push(selRows[a]['A'])
         yArr.push(selRows[a]['B'])
         keepIndex.push(selRows[a]['ID'])
       }

       Points.push(keepIndex);

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
       Traces.push(tracePoints);
       Plotly.addTraces(el.id, Traces);

       var idRows = []
       for (a=0; a<data.dat.length; a++){
       idRows.push(data.dat[a]['ID'])
       }

       el.on('plotly_selected', function(e) {

       numSel = e.points.length
       cN = e.points[0].curveNumber;

       var pointNumbers = [];
       var selData = [];
       for (a=0; a<numSel; a++){
       pointNumbers.push(e.points[a].pointNumber)
       selData.push(data.dat[idRows.indexOf(Points[0][pointNumbers[a]])])
       }

       var Traces = [];
       var xArr = [];
       var yArr = [];
       for (a=0; a<selData.length; a++){
         xArr.push(selData[a]['A'])
         yArr.push(selData[a]['B'])
       }
       var trace = {
         x: xArr,
         y: yArr,
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
       ", data = list(dat=dat))

