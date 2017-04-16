library(plotly)
library(GGally)
library(htmlwidgets)

# thresh instead of ci
ui <- shinyUI(fluidPage(
  sliderInput("thresh", "Threshold:", min = 0, max = 3, value=1, step=1),
  plotlyOutput("myPlot"),
  textOutput("selectedValues")
))

server <- shinyServer(function(input, output) {
  thresh <- reactive(input$thresh)

  set.seed(1)
  dat <- data.frame(ID = paste0("ID",sample(c(1:20),20)), A=4*rnorm(20), B=4*rnorm(20))
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

  output$myPlot <- renderPlotly(ggPS %>% onRender("
    function(el, x, data) {

    var Points = [];
    var Traces = [];
    var selRows = [];

data.dat.forEach(function(row){
if(Math.abs(row['B']) > data.thresh) selRows.push(row);});


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

    var hiLine = {
      x: [-15,15],
      y: [data.thresh,data.thresh],
      mode: 'lines',
      line: {
        color: 'gray',
        width: 1
      },
      opacity: 0.25,
      hoverinfo: 'none'
    };

    var lowLine = {
      x: [-15,15],
      y: [-1*data.thresh,-1*data.thresh],
      mode: 'lines',
      fill: 'tonexty',
      line: {
        color: 'gray',
        width: 1
      },
      opacity: 0.25,
      hoverinfo: 'none'
    };

    Traces.push(tracePoints);
    Traces.push(hiLine);
    Traces.push(lowLine);
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
      Shiny.onInputChange('selData', selData);
      var Traces = [];
      var xArr = [];
      var yArr = [];
      for (a=0; a<selData.length; a++){
        xArr.push(selData[a]['A'])
        yArr.push(selData[a]['B'])
      }
      var traceRed = {
        x: xArr,
        y: yArr,
        mode: 'markers',
        marker: {
          color: 'red',
          size: 4
        },
        hoverinfo: 'none'
      };
      Traces.push(traceRed);

      Plotly.addTraces(el.id, Traces);
    })

    }", data = list(dat=dat, thresh=thresh())))

  selData <- reactive(input$selData)
  output$selectedValues <- renderPrint({selData()})

})

shinyApp(ui, server)
