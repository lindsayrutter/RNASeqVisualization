library(plotly)
library(htmlwidgets)

set.seed(1)
#dat <- mtcars
#dat$mpg <- dat$mpg * 10
dat = data.frame(mpg = rnorm(50,-30,20), disp=rnorm(50,-30,20))
#dat = data.frame(mpg = rnorm(30,0,5), disp=rnorm(30,10,5))
minVal = min(dat)
maxVal = max(dat)
# Designate end points of lines to be drawn
minLine = minVal - 5*(maxVal-minVal)
maxLine = maxVal + 5*(maxVal-minVal)
cv = 25

p <- ggplot(data = dat, aes(x=disp,y=mpg)) + coord_cartesian(xlim = c(minVal, maxVal), ylim = c(minVal, maxVal))

ggplotly(p) %>%
  onRender("
           function(el, x, data) {

             AxisNames = [];
             for (i = 1; i < 3; i++) {
                AxisNames.push(document.getElementsByClassName('infolayer')[0].childNodes[i].textContent);
             }

           var selRows = [];
           data.dat.forEach(function(row){
           if(Math.abs(row[AxisNames[0]]-row[AxisNames[1]]) > Math.sqrt(2)*data.cv) selRows.push(row);
           });
           console.log(selRows);

           var xArr = [];
           for (a=0; a<selRows.length; a++){
             xArr.push(selRows[a][AxisNames[0]])
           }
           var yArr = [];
           for (a=0; a<selRows.length; a++){
             yArr.push(selRows[a][AxisNames[1]])
           }

           var tracePoints = {
             x: xArr,
             y: yArr,
             mode: 'markers',
             marker: {
              color: 'black',
              size: 4
             }
           }

           var traceHiLine = {
             x: [data.minLine, data.maxLine - Math.sqrt(2)*data.cv],
             y: [data.minLine + Math.sqrt(2)*data.cv, data.maxLine],
             mode: 'lines',
             line: {
               color: 'gray',
               width: 1
             },
             opacity: 0.25
           }

           var traceLoLine = {
             x: [data.minLine + Math.sqrt(2)*data.cv, data.maxLine],
             y: [data.minLine, data.maxLine - Math.sqrt(2)*data.cv],
             mode: 'lines',
             fill: 'tonexty',
             line: {
               color: 'gray',
               width: 1
             },
             opacity: 0.25
           }

           Plotly.addTraces(el.id, traceHiLine);
           Plotly.addTraces(el.id, traceLoLine);
           Plotly.addTraces(el.id, tracePoints);

           el.on('plotly_selected', function(e) {
             console.log(e)
           })

           }
           ", data = list(dat=dat, cv=cv, minLine=minLine, maxLine=maxLine))
