library(plotly)
library(htmlwidgets)

ui <- shinyUI(fluidPage(
  sliderInput("treshold", "Threshold:", min = 1, max = 400, value=50),
  plotlyOutput("myPlot")
))

server <- shinyServer(function(input, output) {
  set.seed(1)
  dat <- mtcars
  dat$mpg <- dat$mpg * 10
  minVal = min(dat)
  maxVal = max(dat)

  p <- ggplot(data = dat, aes(x=disp,y=mpg)) + geom_point() + coord_cartesian(xlim = c(minVal, maxVal), ylim = c(minVal, maxVal))

  output$myPlot <- renderPlotly(ggplotly(p) %>%
    onRender("
             function(el, x, data) {

             var selRows = [];
             console.log(data.dat)
             data.dat.forEach(function(row){
              if(Math.abs(row['disp']-row['mpg']) > data.val) selRows.push(row);
             //if(Math.abs(row['disp']-row['mpg']) > input$threshold) selRows.push(row);
             });
             console.log(selRows);

             var xArr = [];
             for (a=0; a<selRows.length; a++){
             xArr.push(selRows[a]['disp'])
             }
             var yArr = [];
             for (a=0; a<selRows.length; a++){
             yArr.push(selRows[a]['mpg'])
             }

             var tracePoints = {
             x: xArr,
             y: yArr,
             hoverinfo: 'none',
             mode: 'markers',
             marker: {
             color: 'orange',
             size: 5
             }
             }

             Plotly.addTraces(el.id, tracePoints);
             }
             ", data = list(dat=dat, val=input$treshold)))})

shinyApp(ui, server)
