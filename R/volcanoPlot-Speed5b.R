library(shiny)
library(plotly)
library(readr)
library(edgeR)

ui <- shinyUI(pageWithSidebar(
  headerPanel("Click the button"),
  sidebarPanel(
    uiOutput("slider"),
    sliderInput("threshP", "P-value:", min = 0, max = 1, value=0.1, step=0.05),
    actionButton("goButton", "Go!")
  ),
  mainPanel(
    plotlyOutput("plot1"),
    verbatimTextOutput("click")
  )
))

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
myList <- list()

# Runs exact test on all pairs of groups and saves in list
for (i in 1:(length(myLevels)-1)){
  for (j in (i+1):(length(myLevels))){
    dat[[paste(i,j,"FC",sep="-")]] <- as.data.frame(exactTest(d, pair=c(myLevels[i], myLevels[j]), dispersion = "tagwise"))$table.logFC
    dat[[paste(i,j,"pval",sep="-")]] <- -1*log10(as.data.frame(exactTest(d, pair=c(myLevels[i], myLevels[j]), dispersion = "tagwise"))$table.PValue)
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

  # datInput only validated once the go button is clicked
  datInput <- eventReactive(input$goButton, {
    subset(dat, `1-2-pval` > -1* log10(input$threshP) & exp(abs(`1-2-FC`)) > input$threshFC)
  })

  output$plot1 <- renderPlotly({
    # will wait to render until datInput is validated
    plot_dat <- datInput()

    p <- qplot(plot_dat$`1-2-FC`, plot_dat$`1-2-pval`, xlim = c(xMin, xMax), ylim=c(yMin, yMax))
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
})

shinyApp(ui, server)
