library(ggplot2)
library(shiny)
library(plotly)
library(htmlwidgets)

ui <- basicPage(
  plotlyOutput("plot1"),
  verbatimTextOutput("info")
)

server <- function(input, output) {
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point(alpha=0) + xlim(0,5) +ylim(-3,3)
  gp <- ggplotly(p)

  set.seed(3)
  pcpDat <- data.frame(ID = paste0("ID",1:7), A=rnorm(7), B=rnorm(7), C=rnorm(7), D=rnorm(7), E=rnorm(7), F=rnorm(7))
  pcpDat$ID <- as.character(pcpDat$ID)
  colNms <- colnames(pcpDat[, c(2:(ncol(pcpDat)))])
  nVar <- length(colNms)

  output$plot1 <- renderPlotly({
  gp %>% onRender("
    function(el, x, data) {

      var Traces = [];
      var dLength = data.pcpDat.length //10
      var vLength = data.nVar //4
      var cNames = data.colNms //'A','B','C','D'

      for (a=0; a<dLength; a++){
        xArr = [];
        yArr = [];
        for (b=0; b<vLength; b++){
          xArr.push(b)
          yArr.push(data.pcpDat[a][cNames[b]]);
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

    console.log(el)

    el.on('plotly_selected', function(e) {
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
      var dat = data.pcpDat[a]
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
            //console.log(['leftEdge',a])
            selectedPCP.push(a+1)
            leftInt = xMaxC-1
        }
        else if (xMinF == xMaxF){
          if (Math.sign(yMin-yAtXmin)!=Math.sign(yMin-yAtXmax)){
            //console.log(['horizontalEdgeSmallBox1',a])
            selectedPCP.push(a+1)
            leftInt = xMaxC-1
          }
          else if (Math.sign(yMax-yAtXmin)!=Math.sign(yMax-yAtXmax)){
            //console.log(['horizontalEdgeSmallBox2',a])
            selectedPCP.push(a+1)
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
                //console.log(['horizontalEdgeXminBox',a])
                selectedPCP.push(a+1)
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
                //console.log(['horizontalEdgeXmaxBox',a])
                selectedPCP.push(a+1)
                leftInt = xMaxC-1
          }
        }
        else if (leftInt >= xMin && rightInt <= xMax){
            var yLeftInt = dat[cNames[leftInt]]
            var yRightInt = dat[cNames[rightInt]]
            if (Math.sign(yMin-yLeftInt)!=Math.sign(yMin-yRightInt) ||
                Math.sign(yMax-yLeftInt)!=Math.sign(yMax-yRightInt)){
              //console.log(['triangleWholeLength',a])
                selectedPCP.push(a+1)
                leftInt = xMaxC-1
            }
        }
        leftInt++;
      }
    }
  }
console.log(selectedPCP)
//for (a=0; a<selectedPCP.length; a++){
//  console.log(data.pcpDat[selectedPCP[a]]['ID'])
//}
Shiny.onInputChange('selectedPCP', selectedPCP);

//Plotly.deleteTraces(el.id, selectedPCP);
//Plotly.deleteTraces(el.id, range(noPoint, (noPoint+(len*(len-1)/2-1)), 1));



//      for (a=0; a<dLength; a++){
//        xArr = [];
//        yArr = [];
//        for (b=0; b<vLength; b++){
//          xArr.push(b)
//          yArr.push(data.pcpDat[a][cNames[b]]);
//        }
//        var pcpLine = {
//          x: xArr,
//          y: yArr,
//          mode: 'lines',
//          line: {
//            color: 'orange',
//            width: 1
//          },
//          opacity: 0.9,
//        }
//        Traces.push(pcpLine);
//      }
//      Plotly.addTraces(el.id, Traces);



//console.log(selectedPCP) // first data frame row has selectedPCP value of 0 and trace index of 1

Plotly.deleteTraces(el.id, selectedPCP);

})
}", data = list(pcpDat = pcpDat, nVar = nVar, colNms = colNms))})

  selectedPCP <- reactive(input$selectedPCP)
  selectID <- reactive(pcpDat[selectedPCP(),]$ID)

  output$info <- renderText({
    paste0(selectID())
  })

  }

shinyApp(ui, server)

