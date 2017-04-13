library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(GGally)
library(edgeR)
library(plotly)
library(htmlwidgets)

ui <- shinyUI(fluidPage(
  uiOutput("slider"),
  sliderInput("threshP", "P-value:", min = 0, max = 1, value=0.1, step=0.05),
  plotlyOutput("plot1"),
  #textOutput("selectedValues"),
  verbatimTextOutput("selectedValues"),
  plotlyOutput("boxPlot")
))

server <- shinyServer(function(input, output) {

  coty <- read_delim(paste0(getwd(),"/SISBID-2016-master/data/GSE61857_Cotyledon_normalized.txt.gz"), delim="\t", col_types="cddddddddd", col_names=c("ID", "C_S1_R1", "C_S1_R2", "C_S1_R3", "C_S2_R1", "C_S2_R2", "C_S2_R3", "C_S3_R1", "C_S3_R2", "C_S3_R3"), skip=1)
  #coty <- coty[1:1000,]
  coty <- as.data.frame(coty)
  colnames(coty) <- c("ID","S1.1","S1.2","S1.3","S2.1","S2.2","S2.3","S3.1","S3.2","S3.3")

  #coty <- data.frame(ID = paste0("ID",1:1000), S1.1=abs(rnorm(1000)), S1.2=abs(rnorm(1000)), S1.3=abs(rnorm(1000)), S2.1=abs(rnorm(1000)), S2.2=abs(rnorm(1000)), S2.3=abs(rnorm(1000)), S3.1=abs(rnorm(1000)), S3.2=abs(rnorm(1000)), S3.3=abs(rnorm(1000)))

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
  fcMax = ceiling(max(exp(xMax), 1/exp(xMin)))

  #make dynamic slider
  output$slider <- renderUI({
    sliderInput("threshFC", "Fold change:", min=0, max=fcMax, value=ceiling((fcMax)/3), step=0.5)
  })

  threshP <- reactive(input$threshP)
  threshFC <- reactive(input$threshFC)

  df <- data.frame()
  p <- ggplot(df) + geom_point() + xlim(xMin, xMax) + ylim(yMin, yMax)
  gp <- ggplotly(p)

  output$plot1 <- renderPlotly({
    gp %>% onRender("
      function(el, x, data) {

        var dat = data.dat
        var myX = 1
        var myY = 2

        var selFC = [];
        var selP = [];
        var sselID = [];
        dat.forEach(function(row){
          rowFC = row[myX+'-'+myY+'-FC']
          rowP = row[myX+'-'+myY+'-pval']
          rowID = row['ID']
          if (rowP >= -1 * Math.log10(data.thP) && data.thFC <= Math.exp(Math.abs(rowFC))){
            selFC.push(rowFC);
            selP.push(rowP);
            sselID.push(rowID);
          }
        });

        var Traces = [];
        var tracePoints = {
          x: selFC,
          y: selP,
          text: sselID,
          mode: 'markers',
          marker: {
            color: 'black',
            size: 2
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
      }", data = list(dat = dat, thP=threshP(), thFC=threshFC()))})

  selID <- reactive(input$selID)

  pcpDat <- reactive(dat[which(dat$ID %in% selID()), 1:(ncol(dat)-2*length(myLevels))])

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
