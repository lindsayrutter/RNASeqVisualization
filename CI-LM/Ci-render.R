library(plotly)
library(htmlwidgets)
library(shiny)

ui <- shinyUI(fluidPage(
  sliderInput("ci", "Confidence interval:", min = 0, max = 1, value=0.95, step=0.05),
  plotlyOutput("myPlot")
))

server <- shinyServer(function(input, output) {

x<-c(2,1,5,5,20,20,23,10,30,25)
y<-c(1.9,3.1,3.3,4.8,5.3,6.1,6.4,7.6,9.8,12.4)
# For convenience, the data may be formatted into a dataframe
dat <- as.data.frame(cbind(x,y))
# Fit a linear model for the data and summarize the output from function lm()
datLm <- lm(y~x,data=dat)

b0 <- coef(datLm)[1]
b1 <- coef(datLm)[2]
sse <- summary(datLm)[[6]]

minVal = min(c(x,y))
maxVal = max(c(x,y))

p <- ggplot(data = dat, aes(x=x, y=y)) + coord_cartesian(xlim = c(minVal, maxVal), ylim = c(minVal, maxVal))

ggPS <- ggplotly(p)

output$myPlot <- renderPlotly(ggPS %>%
    onRender("
             function(el, x, data) {
                 var x = [];
                 var y = [];
                 var xTotal = 0;
                 var ssx = 0;
                 n = data.dat.length;
                 for (a=0; a<n; a++){
                   xa = data.dat[a].x
                   x.push(xa)
                   y.push(data.dat[a].y)
                   xTotal+=xa
                 }
                 var xm = xTotal/n
                 for (a=0; a<n; a++){
                   ssx+=Math.pow((data.dat[a].x - xm),2)
                 }

                 // JS needs to calculate this later
                 var st = 2.306004

                 var minX = Math.min.apply(null,x)
                 var maxX = Math.max.apply(null,x)
                 var inc = (maxX-minX)/100
                 var xv = [];
                 var yv = [];
                 var se = [];
                 var ci = [];
                 var uyv = [];
                 var lyv = [];
                 var a = minX
                 while (a < maxX){
                   xv.push(a);
                   yva = data.b0+data.b1*a;
                   sea = data.sse * Math.sqrt(1/n+Math.pow((a-xm),2)/ssx);
                   yv.push(yva);
                   se.push(sea);
                   ci.push(st*sea);
                   uyv.push(yva+st*sea);
                   lyv.push(yva-st*sea);
                   a+=inc;
                 }

                 var lwr = [];
                 var upr = [];
                 var ypred = [];
                 var ssea = [];
                 var outCI = [];
                 var xPoints = [];
                 var yPoints = [];
                 for (a=0; a<n; a++){
                   xa = data.dat[a].x
                   ssea.push(data.sse * Math.sqrt(1/n+Math.pow((xa-xm),2)/ssx))
                   ypred.push(data.b0+data.b1*xa)
                   lwr.push(ypred[a] - ssea[a]*st)
                   upr.push(ypred[a] + ssea[a]*st)
                   if (!(y[a]>lwr[a] & y[a]<upr[a])){
                      xPoints.push(xa)
                      yPoints.push(data.dat[a].y)
                   }
                 }
                 console.log(xPoints);
                 console.log(yPoints);

                var tracePoints = {
                  x: xPoints,
                  y: yPoints,
                  mode: 'markers',
                  marker: {
                    color: 'orange',
                    size: 7
                  },
                  //xaxis: 'x' + (i+1),
                  //yaxis: 'y' + (i*len+k),
                  hoverinfo: 'none'
                };

             Plotly.addTraces(el.id, tracePoints);
             }
             ", data = list(dat=dat, lm=datLm, b0=b0, b1=b1, sse=sse, ci=input$ci)))})

shinyApp(ui, server)




