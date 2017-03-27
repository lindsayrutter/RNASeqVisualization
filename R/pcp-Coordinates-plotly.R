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
  pcpDat <- data.frame(ID = paste0("ID",1:10), A=rnorm(10), B=rnorm(10), C=rnorm(10), D=rnorm(10))
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
    var xMax = e.range.x[1]
    var xMaxC = Math.ceil(xMax)
    var yMin = e.range.y[0]
    var yMax = e.range.y[1]

if (!((xMax<0) || (xMin>(vLength-1)))){
for (a=0; a<dLength; a++){
  console.log(data.pcpDat[a]['A'])
}
}


  })
    }", data = list(pcpDat = pcpDat, nVar = nVar, colNms = colNms))})
}

shinyApp(ui, server)

