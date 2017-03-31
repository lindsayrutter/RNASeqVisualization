library(ggplot2)
library(shiny)
library(plotly)
library(htmlwidgets)

ui <- basicPage(
  plotlyOutput("plot1") #,
  #actionButton("delete", "Delete selected")
)

server <- function(input, output) {

  #delete <- reactive(input$delete)

  p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point(alpha=0) + xlim(0,5) +ylim(-3,3)
  gp <- ggplotly(p)

  set.seed(3)
  pcpDat <- data.frame(ID = paste0("ID",1:10), A=rnorm(10,-1), B=rnorm(10,-1), C=rnorm(10,-1), D=rnorm(10,1), E=rnorm(10,1), F=rnorm(10,1))
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
    var xMinF = Math.floor(xMin)
    var xMinC = Math.ceil(xMin)
    var xMaxF = Math.floor(xMax)
    var xMaxC = Math.ceil(xMax)
    var yMin = e.range.y[0]
    var yMax = e.range.y[1]

    if (xMin<0){
      xMin = 0
      xMinF = 0
      xMinC = 1
    }
    if (xMax>(vLength-1)){
      xMax = vLength-1
      xMaxF = vLength -2
      xMaxC = vLength-1
    }
    if (!((xMax<0) || (xMin>(vLength-1)))){
      for (a=0; a<dLength; a++){
        var dat = pcpDat[a]
        console.log(pcpDat[a])
        var yAtXminF = dat[cNames[xMinF]]
        var yAtXminC = dat[cNames[xMinF+1]]
        var yAtXmaxF = dat[cNames[xMaxF+1]]
        var yAtXmaxC = dat[cNames[xMaxF]]
        leftInt = xMinF
        while (leftInt < xMaxC){
          var rightInt = leftInt+1
          var yAtXmin = (xMin-xMinF)*(yAtXminC-yAtXminF)/(xMinC-xMinF) + yAtXminF
          var yAtXmax = (xMax-xMaxF)*(yAtXmaxF-yAtXmaxC)/(xMaxC-xMaxF) + yAtXmaxC
          if (leftInt == xMinF && yMin < yAtXmin && yAtXmin < yMax){
            selectedPCP.push(a)
            leftInt = xMaxC-1
          }
          else if (xMinF == xMaxF){
            if (Math.sign(yMin-yAtXmin)!=Math.sign(yMin-yAtXmax)){
              selectedPCP.push(a)
              leftInt = xMaxC-1
            }
            else if (Math.sign(yMax-yAtXmin)!=Math.sign(yMax-yAtXmax)){
              selectedPCP.push(a)
              leftInt = xMaxC-1
            }
          }
          else if (leftInt == xMinF && xMinF != xMaxF){
            var yLeftInt = dat[cNames[leftInt]]
            var yRightInt = dat[cNames[rightInt]]
            if ((Math.sign(yMin-yLeftInt)!=Math.sign(yMin-yRightInt) ||
            Math.sign(yMax-yLeftInt)!=Math.sign(yMax-yRightInt)) &&
            (Math.sign(yMin-yAtXmin) == Math.sign(yMin-yLeftInt) &&
            Math.sign(yMax-yAtXmin) == Math.sign(yMax-yLeftInt))){
              selectedPCP.push(a)
              leftInt = xMaxC-1
            }
          }
          else if (leftInt == xMaxF && xMinF != xMaxF){
            var yLeftInt = dat[cNames[leftInt]]
            var yRightInt = dat[cNames[rightInt]]
            if ((Math.sign(yMin-yLeftInt)!=Math.sign(yMin-yRightInt) ||
            Math.sign(yMax-yLeftInt)!=Math.sign(yMax-yRightInt)) &&
            (Math.sign(yMin-yAtXmax) == Math.sign(yMin-yRightInt) &&
            Math.sign(yMax-yAtXmax) == Math.sign(yMax-yRightInt))){
              selectedPCP.push(a)
              leftInt = xMaxC-1
            }
          }
          else if (leftInt >= xMin && rightInt <= xMax){
            var yLeftInt = dat[cNames[leftInt]]
            var yRightInt = dat[cNames[rightInt]]
            if (Math.sign(yMin-yLeftInt)!=Math.sign(yMin-yRightInt) ||
            Math.sign(yMax-yLeftInt)!=Math.sign(yMax-yRightInt)){
              selectedPCP.push(a)
              leftInt = xMaxC-1
            }
          }
          leftInt++;
        }
      }
    }

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
Plotly.restyle(el.id, update, updateSPCP);


//observeEvent(input$delete, {
//  Plotly.deleteTraces(el.id, updateSPCP);
//  var newDat = []
//  var selectedPCPL = selectedPCP.length
//  for (a=0; a<dLength; a++){
//  var equal = 0;
//  for (b=0; b<selectedPCPL; b++){
//  if (a==selectedPCP[b]){
//  equal=1
//  }
//  }
//  if (equal==0){
//  newDat.push(pcpDat[a])
//  }
//  }
//  pcpDat = newDat
//})


//    Plotly.deleteTraces(el.id, updateSPCP);
//    var newDat = []
//    var selectedPCPL = selectedPCP.length
//    for (a=0; a<dLength; a++){
//      var equal = 0;
//      for (b=0; b<selectedPCPL; b++){
//        if (a==selectedPCP[b]){
//          equal=1
//        }
//      }
//      if (equal==0){
//        newDat.push(pcpDat[a])
//      }
//    }
//    pcpDat = newDat


  })
  }", data = list(pcpDat = pcpDat, nVar = nVar, colNms = colNms ))})
#deleteS=delete()
  }

shinyApp(ui, server)
