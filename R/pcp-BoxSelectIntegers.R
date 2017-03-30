library(ggplot2)
library(shiny)
library(plotly)
library(htmlwidgets)

ui <- basicPage(
  plotlyOutput("plot1"),
  actionButton("delete", "Delete selected")
)

server <- function(input, output) {

  delete <- reactive(input$delete)

  p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point(alpha=0) + xlim(0,5) +ylim(-3,3)
  gp <- ggplotly(p)

  set.seed(3)
  pcpDat <- data.frame(ID = paste0("ID",1:10), A=rnorm(10), B=rnorm(10), C=rnorm(10), D=rnorm(10), E=rnorm(10), F=rnorm(10))
  pcpDat$ID <- as.character(pcpDat$ID)
  colNms <- colnames(pcpDat[, c(2:(ncol(pcpDat)))])
  nVar <- length(colNms)

  output$plot1 <- renderPlotly({
    gp %>% onRender("
  function(el, x, data) {

  var pcpDat = data.pcpDat
  function range(start, stop, step){
  var a=[start], b=start;
  while(b<stop){b+=step;a.push(b)}
  return a;
  };
  var Traces = [];
  var dLength = pcpDat.length
  var vLength = data.nVar
  var cNames = data.colNms
  for (a=0; a<dLength; a++){
  xArr = [];
  yArr = [];
  for (b=0; b<vLength; b++){
  xArr.push(b)
  yArr.push(pcpDat[a][cNames[b]]);
  }
  var pcpLine = {
  x: xArr,
  y: yArr,
  mode: 'lines',
  line: {
  color: 'orange',
  width: 1
  },
  opacity: 0.9,
  }
  Traces.push(pcpLine);
  }
  Plotly.addTraces(el.id, Traces);

  el.on('plotly_selected', function(e) {
    var dLength = pcpDat.length
    var selectedPCP = []
    var xMin = e.range.x[0]
    var xMax = e.range.x[1]
    var xMinC = Math.abs(Math.ceil(xMin))
    var xMaxF = Math.floor(xMax)
    var yMin = e.range.y[0]
    var yMax = e.range.y[1]

    var integers = []

    if (!((xMax<0) || (xMin>(vLength-1)))){
      for (a=xMinC; a<(xMaxF+1); a++){
        integers.push(a)
      }
    }
    console.log(integers)
    var iLength = integers.length

    var selectedPCP = []

    for (a=0; a<dLength; a++){
      var dat = pcpDat[a]
      var isOut = 0;
      for (b=0; b<iLength; b++){
        var yVal = dat[cNames[integers[b]]]
        if (!(yMin < yVal && yVal < yMax)){
          isOut = 1;
        }
      }
      if (isOut==0){
        selectedPCP.push(a)
      }
    }


  console.log(selectedPCP)

    var updateSPCP = []
    var selectedPCPL = selectedPCP.length
    for (a=0; a<selectedPCPL; a++){
      updateSPCP[a]=selectedPCP[a]+1
    }

    var update = {
      line: {
        color: 'green',
        width: 1
      }
    }
    if (selectedPCPL !=0){
      Plotly.restyle(el.id, update, updateSPCP);
    }

  })
  }", data = list(pcpDat = pcpDat, nVar = nVar, colNms = colNms, deleteS=delete()))})

  }

shinyApp(ui, server)
