# ScatMat with hexagons, and obtain correct data frame if click on a given hexagon. Now try to add points to that given hexagon

library(shiny)
library(plotly)
library(data.table)
library(GGally)
library(hexbin)

ui <- fluidPage(
  plotlyOutput("plot"),
  plotlyOutput("plot2")
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
        coord_cartesian(xlim = c(maxRange[1], maxRange[2]), ylim = c(maxRange[1], maxRange[2]))
    }
  }

  cnToPlot = data.frame()
  cN=1
  i=2
  n=ncol(bindata)-1
  while (i<=n){
    ki=i
    kj=i-1
    while (ki<=n){
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

  output$plot2 <- renderPlotly({
    d <- event_data("plotly_click")
    curveN <- d$curveNumber
    cnP <- cnToPlot[which(cnToPlot$curveNumber==curveN),]
    cnH <- cnToID(attr(pS[cnP$ki,cnP$kj]$data, "cID"))
    cnHex <- cbind(cnH[,c(1,2)], curveNumber = cnToPlot[intersect(which(cnToPlot$ki==cnP$ki), which(cnToPlot$kj==cnP$kj)),]$curveNumber)
    hexVal <- as.numeric(as.character(cnHex[which(cnHex$curveNumber==curveN),]$hexID))
    obsns <- which(attr(pS[cnP$ki,cnP$kj]$data, "cID")==hexVal)
    dat <- bindata[obsns,]

    i=2
    n=ncol(bindata)-1
    while (i<=n){
      ki=i
      kj=i-1
      while (ki<=n){
        pi = (ki-1)*5+kj
        pS$plots[pi][[1]] <- pS$plots[pi][[1]] + geom_point(data = dat, aes_string(x=colnames(dat[kj+1]), y=colnames(dat[ki+1])), inherit.aes = FALSE, colour = "white", size = 0.5)
        ki=ki+1
      }
      i=i+1
    }
    #pS

    ggplotly(pS)
  })
}

shinyApp(ui, server)




