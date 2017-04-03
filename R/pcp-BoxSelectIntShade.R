library(ggplot2)
library(shiny)
library(plotly)
library(htmlwidgets)

ui <- basicPage(
  plotlyOutput("plot1")
)

server <- function(input, output) {

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
                    var yMin = e.range.y[0]
                    var yMax = e.range.y[1]

                    var sel1 = {
                    x: [xMin, xMax],
                    y: [yMin, yMin],
                    mode: 'lines',
                    fill: 'tonexty',
                    line: {
                    color: 'black',
                    width: 0.5,
                    dash: 'dot'
                    },
                    hoverinfo: 'none',
                    }
                    var sel2 = {
                    x: [xMax, xMax],
                    y: [yMin, yMax],
                    mode: 'lines',
                    fill: 'tonexty',
                    line: {
                    color: 'black',
                    width: 0.5,
                    dash: 'dot'
                    },
                    hoverinfo: 'none'
                    }
                    var sel3 = {
                    x: [xMin, xMax],
                    y: [yMax, yMax],
                    mode: 'lines',
                    fill: 'tonexty',
                    line: {
                    color: 'black',
                    dash: 'dot',
                    width: 0.5
                    },
                    hoverinfo: 'none'
                    }
                    var sel4 = {
                    x: [xMin, xMin],
                    y: [yMin, yMax],
                    mode: 'lines',
                    fill: 'tonexty',
                    line: {
                    color: 'black',
                    dash: 'dot',
                    width: 0.5
                    },
                    hoverinfo: 'none'
                    }
                    Plotly.addTraces(el.id, [sel1, sel2, sel3, sel4]);
                    })
                    }", data = list(pcpDat = pcpDat, nVar = nVar, colNms = colNms))})
}
shinyApp(ui, server)
