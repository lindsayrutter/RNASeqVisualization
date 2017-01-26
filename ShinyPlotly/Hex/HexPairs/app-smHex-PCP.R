# Saving individual h@cID

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
  #verbatimTextOutput("test7"),
  #verbatimTextOutput("test8"),
  #verbatimTextOutput("test9"),
  #verbatimTextOutput("test10"),
  plotlyOutput("plot3")
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
  oneRow=FALSE
  set.seed(1)
  bindata <- data.frame(ID = paste0("ID",1:200000), A=rnorm(200000), B=rnorm(200000), C=rnorm(200000), D=rnorm(200000), E=rnorm(200000))
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





  output$plot3 <- renderPlotly({
    d <- event_data("plotly_click") #,source="subset"
    if (is.null(d)) "Click and drag events (i.e., select/lasso) appear here (double-click to clear)" else d

    curveN <- d$curveNumber
    cnP <- cnToPlot[which(cnToPlot$curveNumber==curveN),]
    cnH <- cnToID(attr(pS[cnP$ki,cnP$kj]$data, "cID"))
    cnHex <- cbind(cnH[,c(1,2)], curveNumber = cnToPlot[intersect(which(cnToPlot$ki==cnP$ki), which(cnToPlot$kj==cnP$kj)),]$curveNumber)
    hexVal <- as.numeric(as.character(cnHex[which(cnHex$curveNumber==curveN),]$hexID))
    obsns <- which(attr(pS[cnP$ki,cnP$kj]$data, "cID")==hexVal)
    temp <- bindata[obsns,]

    if(nrow(temp)==1){
      oneRow = TRUE
      temp <- rbind(temp,temp)
      temp$ID[2]="justTest"
    }

    dattb <- data.frame(t(temp))
    #data.frame(t(temp[,-c(ncol(temp), ncol(temp)-1)]))
    names(dattb) <- as.matrix(dattb[1, ])
    dattb <- dattb[-1, ]
    dattb[] <- lapply(dattb, function(x) type.convert(as.character(x)))
    setDT(dattb, keep.rownames = TRUE)[]
    dat_long <- melt(dattb, id.vars ="rn" )

    if (oneRow){
      dat_long <- dat_long[1:(nrow(dat_long)/2),]
      oneRow=FALSE
    }

    dat_long

    plot_ly(dat_long, x= ~rn, y= ~value, type = 'scatter', mode = 'lines+markers', color = ~variable)  %>% layout(dragmode="box", showlegend = FALSE)
  })



# #  output$plot2 <- renderPlotly({
#     plot_ly(dat_long, x= ~rn, y= ~value, type = 'scatter', mode = 'lines+markers', color = ~variable)  %>% layout(dragmode="box", showlegend = FALSE)
#   })

}

shinyApp(ui, server)
