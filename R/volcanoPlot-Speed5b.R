library(shiny)
library(plotly)
library(readr)
library(edgeR)
library(shinyBS)
library(tidyr)
library(htmlwidgets)

ui <- shinyUI(fluidPage(
  sidebarLayout(
  sidebarPanel(
    uiOutput("selInput"),
    uiOutput("slider"),
    sliderInput("threshP", "P-value:", min = 0, max = 1, value=0.1, step=0.05),
    uiOutput("uiExample")
    #actionButton("goButton", "Go!")
  ),
  mainPanel(
    plotlyOutput("plot1", height = 350),
    #verbatimTextOutput("click"),
    #verbatimTextOutput("selectedValues"),
    plotlyOutput("boxPlot", height = 350)
  )))
)

#set.seed(1)
#dat <- data.frame(Case = paste0("case",1:100), val1=runif(100,0,1), val2=runif(100,0,1))

dat <- read_delim(paste0(getwd(),"/SISBID-2016-master/data/GSE61857_Cotyledon_normalized.txt.gz"), delim="\t", col_types="cddddddddd", col_names=c("ID", "C_S1_R1", "C_S1_R2", "C_S1_R3", "C_S2_R1", "C_S2_R2", "C_S2_R3", "C_S3_R1", "C_S3_R2", "C_S3_R3"), skip=1)
#dat <- dat[1:1000,]
dat <- as.data.frame(dat)
colnames(dat) <- c("ID","S1.1","S1.2","S1.3","S2.1","S2.2","S2.3","S3.1","S3.2","S3.3")

d <- DGEList(counts = dat[,2:10],
             group = c(rep("S1", 3), rep("S2", 3), rep("S3", 3)),
             genes = dat[,1])
d <- calcNormFactors(d)
d <- estimateCommonDisp(d)
d <- estimateTagwiseDisp(d)
d <- estimateTrendedDisp(d)

myLevels <- levels(d@.Data[[2]]$group)
myPairs <- list()

# Runs exact test on all pairs of groups and saves in list
k=1
for (i in 1:(length(myLevels)-1)){
  for (j in (i+1):(length(myLevels))){
    dat[[paste(i,j,"FC",sep="-")]] <- as.data.frame(exactTest(d, pair=c(myLevels[i], myLevels[j]), dispersion = "tagwise"))$table.logFC
    dat[[paste(i,j,"pval",sep="-")]] <- -1*log10(as.data.frame(exactTest(d, pair=c(myLevels[i], myLevels[j]), dispersion = "tagwise"))$table.PValue)
    myPairs[[k]] <- paste(myLevels[i], " and ", myLevels[j])
    k=k+1
  }
}
nCol = ncol(dat)
datFCP = dat[,(nCol-2*length(myLevels)+1):nCol]
# x-axis FC, y-axis pval
xMax = max(datFCP[,seq(1,ncol(datFCP),by=2)])
xMin = min(datFCP[,seq(1,ncol(datFCP),by=2)])
yMax = max(datFCP[,seq(2,ncol(datFCP),by=2)])
yMin = min(datFCP[,seq(2,ncol(datFCP),by=2)])
fcMax = ceiling(max(exp(xMax), 1/exp(xMin)))


server <- shinyServer(function(input, output) {
  #make dynamic slider
  output$slider <- renderUI({
    sliderInput("threshFC", "Fold change:", min=0, max=fcMax, value=ceiling((fcMax)/3), step=0.5)
  })

  output$selInput <- renderUI({
    selectInput("selPair", "Pairs:", myPairs)
  })

  pairNum <- reactive(as.numeric(which(myPairs==input$selPair)))
  col1 <- reactive(colnames(dat)[nCol-2*length(myLevels)+2*pairNum()])
  col2 <- reactive(colnames(dat)[nCol-2*length(myLevels)+2*pairNum()-1])

  # datInput only validated once the go button is clicked
  datInput <- eventReactive(input$goButton, {
    dat[ which(dat[isolate(col1())] > -1* log10(input$threshP) & exp(abs(dat[isolate(col2())])) > input$threshFC), ]
  })

  output$uiExample <- renderUI({
    tags$span(
      tipify(actionButton("goButton", "Go!"), "Choose low p-value and high fold change", "This button is pointless!")
    )
  })

  output$plot1 <- renderPlotly({
    # will wait to render until datInput is validated
    plot_dat <- datInput()
    p <- qplot(plot_dat[[isolate(col2())]], plot_dat[[isolate(col1())]], xlim = c(xMin, xMax), ylim=c(yMin, yMax)) + xlab("log2(Fold change)") + ylab("-log10(p-value)")
    ggplotly(p)
  })

  d <- reactive(event_data("plotly_selected"))
  output$click <- renderPrint({
    if (is.null(d())){
      "Click on a state to view event data"
    }
    else{
      datInput()[d()$pointNumber+1,] #Working now
    }
  })

  pcpDat <- reactive(datInput()[d()$pointNumber+1,1:(ncol(dat)-2*length(myLevels))])

  #pcpDat <- eventReactive(input$goButton, {data.frame()})

  colNms <- colnames(dat[, 2:(ncol(dat)-2*length(myLevels))])
  nVar <- length(2:(ncol(dat)-2*length(myLevels)))
  boxDat <- dat[, 1:(ncol(dat)-2*length(myLevels))] %>% gather(key, val, -c(ID))
  #output$selectedValues <- renderPrint({str(boxDat)})
  colnames(boxDat)[2:3] <- c("Sample","Counts")
  BP <- ggplot(boxDat, aes(x = Sample, y = Counts)) + geom_boxplot()
  ggBP <- ggplotly(BP)

  output$boxPlot <- renderPlotly({
    ggBP %>% onRender("
    function(el, x, data) {

console.log(data.pcpDat)

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

    var tracePCPLine = {
    x: xArr,
    y: yArr,
    mode: 'lines',
    line: {
    color: 'orange',
    width: 1
    },
    opacity: 0.9,
    }
    Traces.push(tracePCPLine);
    }
    Plotly.addTraces(el.id, Traces);

    }", data = list(pcpDat = pcpDat(), nVar = nVar, colNms = colNms))})

})

shinyApp(ui, server)
