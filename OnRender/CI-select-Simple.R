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
  dat <- data.frame(Row = paste0("Row",sample(c(1:20),20)), A=4*rnorm(20), B=4*rnorm(20))
  dat$Row <- as.character(dat$Row)

  minVal = min(dat[,-1])
  maxVal = max(dat[,-1])

  gg <- ggplot(data = dat, aes(x=A, y=B)) + coord_cartesian(xlim = c(minVal, maxVal), ylim = c(minVal, maxVal))

  ggY <- ggplotly(gg)

  output$myPlot <- renderPlotly(ggY %>% onRender("
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
      keepIndex.push(selRows[a]['Row'])
    }
    Points.push(keepIndex);

    // Add points above the threshold in black
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

    // Add upper horizontal line of gray box
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

    // Add lower horizontal line of gray box
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
      idRows.push(data.dat[a]['Row'])
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

      // Add user-selected points in red
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
