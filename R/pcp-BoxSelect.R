library(ggplot2)
library(shiny)
library(plotly)
library(htmlwidgets)

ui <- basicPage(
  plotlyOutput("plot1", height = 700)
)

server <- function(input, output) {
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point(alpha=0) + xlim(0,5) +ylim(-3,3)
  gp <- ggplotly(p)
  
  set.seed(3)
  pcpDat <- data.frame(ID = paste0("ID",1:20), A=1.3*rnorm(20), B=1.3*rnorm(20), C=1.3*rnorm(20), D=1.3*rnorm(20), E=1.3*rnorm(20), F=1.3*rnorm(20))
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
                    color: 'pink',
                    width: 1
                    },
                    }
                    Traces.push(pcpLine);
                    }
                    Plotly.addTraces(el.id, Traces);
                    el.on('plotly_selected', function(e) {
                    console.log(pcpDat)
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
                    console.log(selectedPCP)
                    //Plotly.deleteTraces(el.id, updateSPCP);

var update = {
line: {
color: 'red',
width: 1
},
opacity: 1,
}
Plotly.restyle(el.id, update, updateSPCP);

                    })
                    }", data = list(pcpDat = pcpDat, nVar = nVar, colNms = colNms))})

  }

shinyApp(ui, server)
