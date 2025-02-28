library(plotly)
library(htmlwidgets)
library(shiny)

ui <- shinyUI(fluidPage(
  sliderInput("ci", "Confidence interval:", min = 0, max = 1, value=0.95, step=0.05),
  plotlyOutput("myPlot")
))

server <- shinyServer(function(input, output) {

set.seed(1)
x<-rnorm(100,0,1)
y<-rnorm(100,0,1)

#load("../leavesDat.Rda")
#x <- data$M.1
#y <- data$M.2
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
                 console.log(ssx)

                 // JS needs to calculate this later
                 var st = 2.306004
                 // console.log(t(1,1))

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

                var tracePoints = {
                  x: xPoints,
                  y: yPoints,
                  mode: 'markers',
                  marker: {
                    color: 'black',
                    size: 4
                  },
                  hoverinfo: 'none'
                };
                var hiLine = {
                  x: xv,
                  y: uyv,
                  mode: 'lines',
                  line: {
                    color: 'gray',
                    width: 1
                  },
                  opacity: 0.25
                };
                var lowLine = {
                  x: xv,
                  y: lyv,
                  mode: 'lines',
                  fill: 'tonexty',
                  line: {
                    color: 'gray',
                    width: 1
                  },
                  opacity: 0.25
                };
                //var fitLine = {
                //  x: [xv[0], xv[xv.length-1]],
                //  y: [data.b0+data.b1*xv[0], data.b0+data.b1*xv[xv.length-1]],
                //  mode: 'lines',
                //  line: {
                //    color: 'black',
                //    width: 1
                //  },
                //  hoverinfo: 'none'
                //};

             Plotly.addTraces(el.id, tracePoints);
             Plotly.addTraces(el.id, hiLine);
             Plotly.addTraces(el.id, lowLine);
             //Plotly.addTraces(el.id, fitLine);
             }
             ", data = list(dat=dat, b0=b0, b1=b1, sse=sse, ci=input$ci)))})

shinyApp(ui, server)




