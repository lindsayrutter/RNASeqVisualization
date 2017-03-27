library(ggplot2)
library(shiny)
library(plotly)
library(htmlwidgets)

ui <- basicPage(
  plotlyOutput("plot1")#,
  #verbatimTextOutput("info")
)

server <- function(input, output) {
  # For some reason, will only allow box select if use geom_blank() with qplot() instead of ggplot()
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point(alpha=0) + xlim(0,3) +ylim(-3,3)
  gp <- ggplotly(p)

  set.seed(1)
  pcpDat <- data.frame(ID = paste0("ID",1:2), A=rnorm(2), B=rnorm(2), C=rnorm(2), D=rnorm(2))
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

  el.on('plotly_selected', function(e) {
    var xMin = e.range.x[0]
    var xMinF = Math.floor(xMin)
    var xMinC = Math.ceil(xMin)
    var xMax = e.range.x[1]
    var xMaxF = Math.floor(xMax)
    var xMaxC = Math.ceil(xMax)
    var yMin = e.range.y[0]
    var yMax = e.range.y[1]

if (!((xMax<0) || (xMin>(vLength-1)))){
for (a=0; a<dLength; a++){
  var dat = data.pcpDat[a]
  var yMinDC1 = dat[cNames[xMinF+1]] // yMax for case1
  var yMinDF1 = dat[cNames[xMinF]] // yMin for case1

  var yMinDC2 = dat[cNames[xMaxF+1]] // yMax for case2
  var yMinDF2 = dat[cNames[xMaxF]] // yMin for case2

  //console.log(yMinDC1, yMinDF1, xMinC, xMinF, xMax) //case1
  //console.log(yMinDC2, yMinDF2, xMaxC, xMaxF, xMax) //case2
  var case1 = (xMin-xMinF)*(yMinDC1-yMinDF1)/(xMinC-xMinF) + yMinDF1
  var case2 = (xMax-xMaxF)*(yMinDC2-yMinDF2)/(xMaxC-xMaxF) + yMinDF2

  //console.log(yMin, case1, yMax)
  if (yMin < case1 && case1 < yMax){
    console.log('yes case1')
  }
  else if (yMin < case2 && case2 < yMax){
    console.log('yes case2')
  }
  else if (Math.sign(yMin-case1)!=Math.sign(yMin-case2)){
    //if (!isNaN(case1)&&!isNaN(case2)){
      console.log('yes case3')
    //}
  }
  else if (Math.sign(yMax-case1)!=Math.sign(yMax-case2)){
    //if (!isNaN(case1)&&!isNaN(case2)){
      console.log('yes case4')
    //}
  }
  // Check triangle case
  else if (yMinDF1 < yMin && yMinDC2 < yMin && yMinDC1 > yMin && yMinDC1 < yMax){
    console.log('yes case5')
  }
  else if (yMinDF1 > yMax && yMinDC2 > yMax && yMinDC1 < yMax && yMinDC1 > yMin){
    console.log('yes case6')
  }

  //console.log(case1) // works perfectly
  //console.log(case2) // works perfectly
}

}


  })
    }", data = list(pcpDat = pcpDat, nVar = nVar, colNms = colNms))})
}

shinyApp(ui, server)

