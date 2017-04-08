library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(GGally)
library(edgeR)
library(plotly)
library(htmlwidgets)

ui <- shinyUI(fluidPage(
  sliderInput("threshP", "P-value:", min = 0, max = 1, value=0.05, step=0.05),
  plotlyOutput("plot1"),
  #textOutput("selectedValues"),
  plotlyOutput("boxPlot")
))

server <- shinyServer(function(input, output) {

  threshP <- reactive(input$threshP)

  coty <- read_delim(paste0(getwd(),"/SISBID-2016-master/data/GSE61857_Cotyledon_normalized.txt.gz"), delim="\t", col_types="cddddddddd", col_names=c("ID", "C_S1_R1", "C_S1_R2", "C_S1_R3", "C_S2_R1", "C_S2_R2", "C_S2_R3", "C_S3_R1", "C_S3_R2", "C_S3_R3"), skip=1)

  coty <- coty[1:100,]
  coty <- as.data.frame(coty)
  colnames(coty) <- c("ID","S1.1","S1.2","S1.3","S2.1","S2.2","S2.3","S3.1","S3.2","S3.3")

  d <- DGEList(counts = coty[,2:10],
               group = c(rep("S1", 3), rep("S2", 3), rep("S3", 3)),
               genes = coty[,1])
  d <- calcNormFactors(d)
  d <- estimateCommonDisp(d)
  d <- estimateTagwiseDisp(d)
  d <- estimateTrendedDisp(d)

  myLevels <- levels(d@.Data[[2]]$group)
  myList <- list()

  # Runs exact test on all pairs of groups and saves in list
  for (i in 1:(length(myLevels)-1)){
    for (j in (i+1):(length(myLevels))){
      coty[[paste(i,j,"FC",sep="-")]] <- as.data.frame(exactTest(d, pair=c(myLevels[i], myLevels[j]), dispersion = "tagwise"))$table.logFC
      coty[[paste(i,j,"pval",sep="-")]] <- -1*log10(as.data.frame(exactTest(d, pair=c(myLevels[i], myLevels[j]), dispersion = "tagwise"))$table.PValue)
    }
  }
  nCol = ncol(coty)
  dat = coty
  datFCP = dat[,(nCol-2*length(myLevels)+1):nCol]
  # x-axis FC, y-axis pval
  xMax = max(datFCP[,seq(1,ncol(datFCP),by=2)])
  xMin = min(datFCP[,seq(1,ncol(datFCP),by=2)])
  yMax = max(datFCP[,seq(2,ncol(datFCP),by=2)])
  yMin = min(datFCP[,seq(2,ncol(datFCP),by=2)])

  df <- data.frame()
  p <- ggplot(df) + geom_point() + xlim(xMin, xMax) + ylim(yMin, yMax)
  gp <- ggplotly(p)

  output$plot1 <- renderPlotly({
    gp %>% onRender("
      function(el, x, data) {
console.log(data.thP)

        var dat = data.dat
        var myX = 1
        var myY = 2

        var selFC = [];
        var selP = [];
        //console.log(['threshP', data.thP])
        //console.log(-1 * Math.log10(data.thP))
        dat.forEach(function(row){
          rowFC = row[myX+'-'+myY+'-FC']
          rowP = row[myX+'-'+myY+'-pval']
          //console.log([rowP, ])
          //if (rowP > -1 * Math.log10(data.thP)){
            selFC.push(rowFC);
            selP.push(rowP);
          //}
        });
        console.log(selP)

        var Traces = [];
        var tracePoints = {
          x: selFC,
          y: selP,
          mode: 'markers',
          marker: {
            color: 'black',
            size: 2
          },
          //xaxis: 'x' + (i+1),
          //yaxis: 'y' + (i*len+k),
          //hoverinfo: 'none'
        };
        Traces.push(tracePoints);
        Plotly.addTraces(el.id, Traces);

el.on('plotly_selected', function(e) {
    numSel = e.points.length
    selRow = []
    for (a=0; a<numSel; a++){
      d = e.points[a].pointNumber+1
      selRow.push(d)
    }
    console.log(selRow)
    Shiny.onInputChange('selRow', selRow);
})
      }", data = list(dat = dat, thP=threshP()))})

  selRow <- reactive(input$selRow)

  pcpDat <- reactive(dat[selRow(), 1:(ncol(dat)-2*length(myLevels))])
  output$selectedValues <- renderPrint({str(pcpDat())})

  colNms <- colnames(coty[, 2:(ncol(dat)-2*length(myLevels))])
  nVar <- length(2:(ncol(dat)-2*length(myLevels)))

  boxDat <- coty[, 1:(ncol(dat)-2*length(myLevels))] %>% gather(key, val, -c(ID))
  BP <- ggplot(boxDat, aes(x = key, y = val)) + geom_boxplot()
  ggBP <- ggplotly(BP)

  output$boxPlot <- renderPlotly({
    ggBP %>% onRender("
      function(el, x, data) {

      var Traces = [];

      var dLength = data.pcpDat.length
      var vLength = data.nVar
      var cNames = data.colNms

      for (a=0; a<dLength; a++){
      xArr = [];
      yArr = [];
      for (b=0; b<vLength; b++){
      xArr.push(b+1)
      yArr.push(data.pcpDat[a][cNames[b]]);
      }

      var traceHiLine = {
      x: xArr,
      y: yArr,
      mode: 'lines',
      line: {
      color: 'orange',
      width: 1
      },
      opacity: 0.9,
      }
      Traces.push(traceHiLine);
      }
      Plotly.addTraces(el.id, Traces);

      }", data = list(pcpDat = pcpDat(), nVar = nVar, colNms = colNms))})

})

shinyApp(ui, server)
