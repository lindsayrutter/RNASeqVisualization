# Saving individual h@cID

library(shiny)
library(plotly)
library(data.table)
library(GGally)
library(hexbin)

ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("click"),
  verbatimTextOutput("test"),
  verbatimTextOutput("test2"),
  verbatimTextOutput("test3"),
  verbatimTextOutput("test4"),
  verbatimTextOutput("test5")
)

server <- function(input, output, session) {

  # Curve number to ID
  cnToID <- function(h){
    df <- data.frame(table(h)) #h@cID
    colnames(df) <- c("hexID","count")
    cnID <- df[order(df$count,as.character(df$hexID)),]
    cnID$curveNumber <- seq(1, nrow(cnID))
    return(cnID)
  }

  set.seed(1)
  bindata <- data.frame(ID = paste0("ID",1:100), A=rnorm(100), B=rnorm(100), C=rnorm(100), D=rnorm(100), E=rnorm(100))
  bindata$ID <- as.character(bindata$ID)

  maxVal = max(abs(bindata[,2:6]))
  maxRange = c(-1*maxVal, maxVal)

  my_fn <- function(data, mapping, ...){
    x = data[,c(as.character(mapping$x))]
    y = data[,c(as.character(mapping$y))]
    h <- hexbin(x=x, y=y, xbins=5, shape=1, IDs=TRUE, xbnds=maxRange, ybnds=maxRange)
    hexdf <- data.frame (hcell2xy (h),  hexID = h@cell, counts = h@count)
    attr(hexdf, "cID") <- h@cID
    p <- ggplot(hexdf, aes(x=x, y=y, fill = counts, hexID=hexID)) + geom_hex(stat="identity")
    p
  }

  p <- ggpairs(bindata[,2:6], lower = list(continuous = my_fn))
  pS <- p
  for(i in 2:p$nrow) {
    for(j in 1:(i-1)) {
      pS[i,j] <- p[i,j] +
        scale_x_continuous(limits = maxRange) +
        scale_y_continuous(limits = maxRange)
    }
  }

  #attr(pS[2,1]$data, "cID")


  cnToPlot = data.frame()
  cN=1
  i=2
  n=ncol(bindata)-1
  while (i<=n){
    ki=i
    kj=i-1
    while (ki<=n){
      #print(attr(pS[ki,kj]$data, "cID"))
      myLength <- length(table(attr(pS[ki,kj]$data, "cID")))
      cnToPlot = rbind(cnToPlot, cbind(ki = rep(ki, myLength), kj = rep(kj, myLength), curveNumber = cN:(cN+myLength-1)))
      ki=ki+1
      cN=cN+myLength+1
    }
    cN=cN+i
    i=i+1
  }

  output$plot <- renderPlotly({
    ggplotly(pS)
  })

  output$click <- renderPrint({
    if (is.null(d())){
      "Click on a state to view event data"
    }
    else{
      #str(d())
      d()$curveNumber
    }
  })

  d <- reactive(event_data("plotly_click"))
  curveN <- reactive(d()$curveNumber)
  cnP <- reactive(cnToPlot[which(cnToPlot$curveNumber==curveN()),])
  cnPKI <- reactive(cnP()$ki)
  cnH <- reactive(cnToID(attr(pS[cnP()$ki,cnP()$kj]$data, "cID")))
  cnHex <- reactive(cbind(cnH()[,c(1,2)], curveNumber = cnToPlot[intersect(which(cnToPlot$ki==cnP()$ki), which(cnToPlot$kj==cnP()$kj)),]$curveNumber))
  #testVal <- reactive(cnToPlot[intersect(which(cnToPlot$ki==cnP()$ki), which(cnToPlot$kj==cnP()$kj)),]$curveNumber)
  hexVal <- reactive(as.numeric(as.character(cnHex()[which(cnHex()$curveNumber==curveN()),]$hexID)))

  output$test <- renderPrint({
    print("curveN")
    curveN()
  })

  output$test2 <- renderPrint({
    print("cnP")
    cnP()
  })

  output$test3 <- renderPrint({
    print("cnPKI")
    cnPKI()
  })

  output$test4 <- renderPrint({
    print("cnHex")
    cnHex()
  })

  output$test5 <- renderPrint({
    print("hexVal")
    hexVal()
  })

}

shinyApp(ui, server)
