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
datLm <- lm(x~y,data=dat)

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
                 console.log(xm)
                 console.log(ssx)



             }
             ", data = list(dat=dat, lm = datLm, ci = input$ci)))})

shinyApp(ui, server)
