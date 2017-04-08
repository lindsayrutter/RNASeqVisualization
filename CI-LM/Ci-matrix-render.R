library(plotly)
library(htmlwidgets)
library(shiny)
library(GGally)

ui <- shinyUI(fluidPage(
  sliderInput("ci", "Confidence interval:", min = 0, max = 1, value=0.95, step=0.01),
  plotlyOutput("myPlot")
))

server <- shinyServer(function(input, output) {

ci <- reactive(input$ci)

#set.seed(2)
#dat <- data.frame(ID = paste0("ID",1:100), A.1=rnorm(100), A.2=rnorm(100), A.3=rnorm(100), B.1=rnorm(100), B.2=rnorm(100))

load("../leavesDat.Rda")
dat <- data.frame(ID = paste0("ID",1:nrow(data)), M.1=data[,2], M.2=data[,3], M.3=data[,4], P.1=data[,5], P.2=data[,6], P.3=data[,7])
dat <- dat[1:100,]

dat$ID <- as.character(dat$ID)
nCol = ncol(dat)

#st = qt(1-(1-0.999)/2,(nrow(dat)-2))
#st = 150

b0 = c()
b1 = c()
sse = c()
for (i in 2:(nCol-1)){
  j = nCol
  while (j >i){
    datXY <- as.data.frame(cbind(x = dat[,i], y = dat[,j]))
    datLm <- lm(y~x,data=datXY)
    b0 <- c(b0, coef(datLm)[1])
    b1 <- c(b1, coef(datLm)[2])
    sse <- c(sse, summary(datLm)[[6]])
    j = j-1
  }
}
b0<-as.vector(b0)
b1<-as.vector(b1)
sse<-as.vector(sse)

minVal = 0
maxVal = max(dat[,-1])

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
console.log(data.st)

         len = Math.sqrt(document.getElementsByClassName('cartesianlayer')[0].childNodes.length);
         AxisNames = [];
         for (i = 1; i < (len+1); i++) {
           AxisNames.push(document.getElementsByClassName('infolayer')[0].childNodes[i].textContent);
         }

       var Traces = [];
       var i=0;
       var j=0;
       var k=1;
       while ((i*len+k)<=Math.pow((len-1),2)) {
       while ((i+k)<len){
         var x = [];
         var y = [];
         var xTotal = 0;
         var ssx = 0;
         n = data.dat.length; // 100
         for (a=0; a<n; a++){
           xa = data.dat[a][AxisNames[i]]
           x.push(xa)
           y.push(data.dat[a][AxisNames[(len-k)]])
           xTotal+=xa
         }

         var xm = xTotal/n
         for (a=0; a<n; a++){
           ssx+=Math.pow((data.dat[a][AxisNames[i]] - xm),2)
         }

         var minX = Math.min.apply(null,x)
         var maxX = Math.max.apply(null,x)

         console.log(x)
         //console.log(minX)
         //console.log(maxX)

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
           yva = data.b0[j]+data.b1[j]*a;
           sea = data.sse[j] * Math.sqrt(1/n+Math.pow((a-xm),2)/ssx);
           yv.push(yva);
           se.push(sea);
           ci.push(data.st*sea);
           uyv.push(yva+data.st*sea);
           lyv.push(yva-data.st*sea);
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
           xa = data.dat[a][AxisNames[i]]
           ssea.push(data.sse[j] * Math.sqrt(1/n+Math.pow((xa-xm),2)/ssx))
           ypred.push(data.b0[j]+data.b1[j]*xa)
           lwr.push(ypred[a] - ssea[a]*data.st)
           upr.push(ypred[a] + ssea[a]*data.st)
           if (!(y[a]>lwr[a] & y[a]<upr[a])){
              xPoints.push(xa)
              yPoints.push(data.dat[a][AxisNames[(len-k)]])
           }
         }

        var tracePoints = {
          x: xPoints,
          y: yPoints,
          mode: 'markers',
          marker: {
            color: 'black',
            size: 2
          },
          xaxis: 'x' + (i+1),
          yaxis: 'y' + (i*len+k),
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
          xaxis: 'x' + (i+1),
          yaxis: 'y' + (i*len+k),
          opacity: 0.25,
          hoverinfo: 'none'
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
          xaxis: 'x' + (i+1),
          yaxis: 'y' + (i*len+k),
          opacity: 0.25,
          hoverinfo: 'none'
        };
        Traces.push(tracePoints);
        Traces.push(hiLine);
        Traces.push(lowLine);
        j++;
        k++;
       }
       i++;
       k=1;
       }
     Plotly.addTraces(el.id, Traces);
     }
     ", data = list(dat=dat, b0=b0, b1=b1, sse=sse, st=ci())))})
#st=st, ci=input$ci

shinyApp(ui, server)




