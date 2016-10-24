library(shiny)
library(plotly)
library(data.table)
library(dplyr)
library(tidyr)

library(rtracklayer)
library(Rsamtools)
library(grid)
library(GenomicAlignments)
library(ggplot2)
#library(GGally) #
library(edgeR)
library(stringr)
library(EDASeq)
library(matrixStats)
library(gridExtra)
library(reshape2)
library(scales)
library(gtools)

server <- function(input, output, session) {

  set.seed(1)
  dat <- data.frame(ID = paste0("ID",1:10), A.1 = runif(10), A.2 = runif(10), A.3 = runif(10), B.1 = runif(10), B.2 = runif(10), B.3 = runif(10))
  dat$ID <- as.character(dat$ID)

  # Convert DF from scatterplot to PCP
  datt <- data.frame(t(dat))
  names(datt) <- as.matrix(datt[1, ])
  datt <- datt[-1, ]
  datt[] <- lapply(datt, function(x) type.convert(as.character(x)))
  setDT(datt, keep.rownames = TRUE)[]
  colnames(datt)[1] <- "x"
  dat_long <- melt(datt, id.vars ="x" )
  dat_long <- separate(dat_long, x, c("group", "rep"), remove=FALSE)
  dat_long$group <- factor(dat_long$group)

  output$plot <- renderPlotly({
    plot_ly(dat_long, x= ~x, y= ~value, type = 'scatter', mode = 'lines+markers', color = ~variable)  %>% layout(dragmode="box", showlegend = FALSE)
  })

  d <- reactive(event_data("plotly_selected"))

  output$click <- renderPrint({
    if (is.null(d())){
      "Click on a state to view event data"
    }
    else{
      # str(d())
      # d()$curveNumber
      # dat[d()$curveNumber+1,]
      # dat[d()$curveNumber+1,]$ID
      myDat <- dat_long[variable == dat[d()$curveNumber+1,]$ID]
      myDat <- as.data.frame(myDat)
      #myDat
      str(myDat)
    }
  })

  output$plot2 <- renderPlotly({
    plot_ly(as.data.frame(dat_long[variable == dat[d()$curveNumber+1,]$ID]), x = ~group, y = ~value, type = "scatter", marker = list(size = 10), color = ~group)
    #%>% layout(xaxis = ax, yaxis = ay, legend = list(x = 0.35, y = -0.26))
  })
}
