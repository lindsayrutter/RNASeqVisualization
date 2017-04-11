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
  dat <- data.frame(ID = paste0("ID",1:1000), FC=runif(1000,0,10), pval=runif(1000,0,1))
  dat$ID <- as.character(dat$ID)

  threshP <- reactive(input$threshP)
  threshFC <- reactive(input$threshFC)

  datInput <- reactive(subset(dat , pval > threshP() & threshFC() < FC))

  p <- qplot(datInput()$FC, datInput()$pval)
  gp <- ggplotly(p, tooltip=c())

  df <- data.frame()
  p <- ggplot(df) + geom_point()
  gp <- ggplotly(p)

  output$plot1 <- renderPlotly({
    gp %>% onRender("
    function(el, x, datInput) {

console.log(el)
console.log(el.id)
console.log(x)

    var selFC = [];
    var selP = [];
    var sselID = [];
    dat.forEach(function(row){
    rowFC = row['FC']
    rowP = row['pval']
    rowID = row['ID']
    selFC.push(rowFC);
    selP.push(rowP);
    sselID.push(rowID);
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
    Traces.push(tracePoints);
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
    }", data = datInput))

  selID <- reactive(input$selID)
  selDat <- reactive(dat[which(dat$ID %in% selID()), ])
  output$selectedValues <- renderPrint({selDat()})
  })

shinyApp(ui, server)
