library(plotly)
library(htmlwidgets)

#dat <- mtcars
#dat$mpg <- dat$mpg * 10
#dat = data.frame(mpg = rnorm(20,0,5), disp=rnorm(20,0,5))
dat = data.frame(mpg = rnorm(20,30,5), disp=rnorm(20,30,5))
minVal = min(dat)
maxVal = max(dat)
# Designate end points of lines to be drawn
minLine = minVal - 5*(maxVal-minVal)
maxLine = maxVal + 5*(maxVal-minVal)
cv = 4

p <- ggplot(data = dat, aes(x=disp,y=mpg)) + geom_point(size=0.5) + coord_cartesian(xlim = c(minVal, maxVal), ylim = c(minVal, maxVal))

ggplotly(p) %>%
  onRender("
           function(el, x, data) {

             AxisNames = [];
             //for (i = 1; i < (len+1); i++) {
             for (i = 1; i < 3; i++) {
                AxisNames.push(document.getElementsByClassName('infolayer')[0].childNodes[i].textContent);
             }
             console.log(AxisNames);

           el.on('plotly_click', function(e) {

           myMin = -1000000000;
           myMax = 1000000000;

           var selRows = [];
           data.dat.forEach(function(row){
           console.log(Math.abs(row['disp']-row['mpg']) )
           if(Math.abs(row['disp']-row['mpg']) > Math.sqrt(2)*data.cv) selRows.push(row);
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
              color: 'orange',
              size: 10
             }
           }

           var Traces = [];
           Traces.push(tracePoints);

           var traceHiLine = {
             x: [data.minLine, data.maxLine - Math.sqrt(2)*data.cv],
             y: [data.minLine + Math.sqrt(2)*data.cv, data.maxLine],
             mode: 'lines',
             line: {
               color: 'blue',
               width: 2
             },
             opacity: 0.5
           }

           var traceLoLine = {
             x: [data.minLine + Math.sqrt(2)*data.cv, data.maxLine],
             y: [data.minLine, data.maxLine - Math.sqrt(2)*data.cv],
             mode: 'lines',
             fill: 'tonexty',
             line: {
               color: 'red',
               width: 2
             },
             opacity: 0.5
           }

           Plotly.addTraces(el.id, traceHiLine);
           Plotly.addTraces(el.id, traceLoLine);
           Plotly.addTraces(el.id, Traces);
           })
           }
           ", data = list(dat=dat, cv=cv, minLine=minLine, maxLine=maxLine))
