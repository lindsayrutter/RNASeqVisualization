# ScatMat with hexagons, and obtain correct data frame if click on a given hexagon. Now try to add points to that given hexagon

library(shiny)
library(plotly)
library(data.table)
library(GGally)
library(hexbin)

ui <- fluidPage(
  plotlyOutput("plot"),
  #verbatimTextOutput("click"),
  #verbatimTextOutput("test"),
  #verbatimTextOutput("test2"),
  #verbatimTextOutput("test3"),
  #verbatimTextOutput("test4"),
  #verbatimTextOutput("test5"),
  #verbatimTextOutput("test6"),
  verbatimTextOutput("test7")

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
  cnH <- reactive(cnToID(attr(pS[cnP()$ki,cnP()$kj]$data, "cID")))
  cnHex <- reactive(cbind(cnH()[,c(1,2)], curveNumber = cnToPlot[intersect(which(cnToPlot$ki==cnP()$ki), which(cnToPlot$kj==cnP()$kj)),]$curveNumber))
  hexVal <- reactive(as.numeric(as.character(cnHex()[which(cnHex()$curveNumber==curveN()),]$hexID)))
  obsns <- reactive(which(attr(pS[cnP()$ki,cnP()$kj]$data, "cID")==hexVal()))
  dat <- reactive(bindata[obsns(),])

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

  output$test6 <- renderPrint({
    print("obsns")
    obsns()
  })

  output$test7 <- renderPrint({
    print("dat")
    dat()
  })

  ppS <- pS

  # my_fn <- function(data, mapping, ...){
  #   x1 = data[,c(as.character(mapping$x))]
  #   y1 = data[,c(as.character(mapping$y))]
  #   h <- hexbin(x=x1, y=y1, xbins=5, shape=1, IDs=TRUE, xbnds=maxRange, ybnds=maxRange)
  #   hexdf <- data.frame (hcell2xy (h),  hexID = h@cell, counts = h@count)
  #   attr(hexdf, "cID") <- h@cID
  #   x1 <- dat$A
  #   y1 <- dat$B
  #   p <- ggplot(hexdf, aes(x=x, y=y, fill = counts, hexID=hexID)) + geom_hex(stat="identity")
  #   p
  # }
  #
  # p <- ggpairs(bindata[,2:6], lower = list(continuous = my_fn))
  #







  # Approach to adding points to individual subplots directly is not working
  # for (i in 1:length(ppS$plots)){
  #   if (length(ppS$plots[i][[1]]$mapping)==2){
  #     print(i)
  #   }
  #
  # }
  # i=2
  # n=ncol(bindata)-1
  # while (i<=n){
  #   ki=i
  #   kj=i-1
  #   while (ki<=n){
  #     cat(sprintf("\"%f\" \"%f\"\n", ki, kj))
  #     curPlot <- getPlot(ppS,ki,kj)
  #     curPlot <- curPlot + geom_point(data = dat, aes(x=colnames(dat)[kj+1], y=colnames(dat)[ki+1]))
  #     curPlot + geom_point(data = dat, aes(x=A, y=B)) + coord_equal()
  #     curPlot + geom_point() + geom_point(dat, aes(A, B)) + coord_equal()
  #     curPlot + geom_point() + geom_point(dat[,c(kj+1,ki+1)]) + coord_equal()
  #     gg0 <- putPlot(gg0,curPlot,2,2)
  #     ki=ki+1
  #   }
  #   i=i+1
  # }


  output$plot2 <- renderPlotly({
    if (!is.null(d())){
      pp <- pS + geom_point(data = dat, aes(x=x, y=y)) + coord_equal()
      p2 <- ggplotly(pp)
      for (i in 1:nrow(hexdf)){
        p2$x$data[[i]]$text <- gsub("<.*$", "", p2$x$data[[i]]$text)
      }
    }
    p2
  })


}

shinyApp(ui, server)
