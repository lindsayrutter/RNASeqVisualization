library(ggplot2)
library(plotly)
library(htmlwidgets)

ui <- shinyUI(fluidPage(
  sliderInput("threshP", "P-value:", min = 0, max = 1, value=1, step=0.05),
  sliderInput("threshFC", "Fold change:", min = 0, max = 10, value=0, step=0.5),
  plotlyOutput("plot1"),
  verbatimTextOutput("selectedValues")
))

server <- shinyServer(function(input, output) {
  set.seed(1)
  threshP <- reactive(input$threshP)
  threshFC <- reactive(input$threshFC)

  dat <- data.frame(ID = paste0("ID",1:14), FC=runif(14,0,10), pval=runif(14,0,1))
  dat$ID <- as.character(dat$ID)
  print(dat)

  # x-axis FC, y-axis pval
  xMax = max(dat$FC)
  xMin = min(dat$FC)
  yMax = max(dat$pval)
  yMin = min(dat$pval)

  df <- data.frame()
  p <- ggplot(df) + geom_point() + xlim(xMin, xMax) + ylim(yMin, yMax)
  gp <- ggplotly(p)

  output$plot1 <- renderPlotly({
    gp %>% onRender("
    function(el, x, data) {

console.log(el)
console.log(el.id)
console.log(x)

    var dat = data.dat
    var selFC = [];
    var selP = [];
    var sselID = [];
    var NselFC = [];
    var NselP = [];
    var NsselID = [];
    dat.forEach(function(row){
    rowFC = row['FC']
    rowP = row['pval']
    rowID = row['ID']
    if (rowP <= data.thP && data.thFC <= rowFC){
    selFC.push(rowFC);
    selP.push(rowP);
    sselID.push(rowID);
    }
    else{
    NselFC.push(rowFC);
    NselP.push(rowP);
    NsselID.push(rowID)
    }
    });

    var Traces = [];
    var tracePoints = {
    x: selFC,
    y: selP,
    text: sselID,
    mode: 'markers',
    marker: {
    color: 'red',
    size: 6
    },
    };

    var NtracePoints = {
    x: NselFC,
    y: NselP,
    text: NsselID,
    mode: 'markers',
    marker: {
    color: 'black',
    size: 6
    },
    opacity: 0,
    hoverinfo: 'none'
    };

    Traces.push(tracePoints);
    Traces.push(NtracePoints);
    Plotly.addTraces(el.id, Traces);

    el.on('plotly_selected', function(e) {

    numSel = e.points.length
    Points = e.points
    selID = []
    for (a=0; a<numSel; a++){
    PN = Points[a].pointNumber
    selRow = sselID[PN]
    selID.push(selRow)
    }
    Shiny.onInputChange('selID', selID);
    })
    }", data = list(dat = dat, thP=threshP(), thFC=threshFC()))})

  selID <- reactive(input$selID)
  selDat <- reactive(dat[which(dat$ID %in% selID()), ])
  output$selectedValues <- renderPrint({selDat()})
  })

shinyApp(ui, server)
