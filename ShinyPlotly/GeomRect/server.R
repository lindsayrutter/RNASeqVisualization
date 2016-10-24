library(shiny)
library(plotly)
library(data.table)
library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(GGally)
library(edgeR)

server <- function(input, output, session) {

  # coty <- read_delim("../../R/SISBID-2016-master/data/GSE61857_Cotyledon_normalized.txt.gz", delim="\t", col_types="cddddddddd", col_names=c("ID", "C_S1_R1", "C_S1_R2", "C_S1_R3", "C_S2_R1", "C_S2_R2", "C_S2_R3", "C_S3_R1", "C_S3_R2", "C_S3_R3"), skip=1)
  # coty <- as.data.frame(coty)
  #
  # d <- DGEList(counts = coty[,2:7],
  #              group = c(rep("S1", 3), rep("S2", 3)),
  #              genes = coty[,1])
  # d <- calcNormFactors(d)
  # d <- estimateCommonDisp(d)
  # d <- estimateTagwiseDisp(d)
  # d <- estimateTrendedDisp(d)
  # de <- exactTest(d, pair=c("S1", "S2"), dispersion = "tagwise")
  # sig.tab <- de$table
  # sig.tab$genes <- coty$ID
  # sig.tab <- sig.tab %>% filter(PValue < 0.01)
  #
  # sig.tab <- merge(sig.tab, coty[,1:7], by.x="genes", by.y="ID")
  #
  # sig.tab <- mutate(sig.tab, xmin = pmin(C_S1_R1, C_S1_R2, C_S1_R3), xmax = pmax(C_S1_R1, C_S1_R2, C_S1_R3), ymin = pmin(C_S2_R1, C_S2_R2, C_S2_R3), ymax = pmax(C_S2_R1, C_S2_R2, C_S2_R3))

  load("sigTab.rda")
  output$plot <- renderPlotly({
    p <- ggplot(sig.tab, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) + geom_rect(fill = 'purple', color = 'black', size = 0.3, alpha = 0.1)
    ggplotly(p)
  })

  # hover, click --> Return only cruveNumber: int 0
  # relayout --> Return four corners of literal selection
  # selected --> ?
  d <- reactive(event_data("plotly_relayout"))

  output$click <- renderPrint({
    if (is.null(d())){
      "Click on a state to view event data"
    }
    else{
      str(d())
      # d()$pointNumber
      # dat[(d()$pointNumber+1),]
      # dat[(d()$pointNumber+1),]$ID
      # str(dat[(d()$pointNumber+1),]$ID)
    }
  })

  # Convert DF from scatterplot to PCP
  # datt <- data.frame(t(dat))
  # names(datt) <- as.matrix(datt[1, ])
  # datt <- datt[-1, ]
  # datt[] <- lapply(datt, function(x) type.convert(as.character(x)))
  # setDT(datt, keep.rownames = TRUE)[]
  # colnames(datt)[1] <- "x"
  # dat_long <- melt(datt, id.vars ="x" )

  # output$plot2 <- renderPlotly({
  #   plot_ly(dat_long[dat_long$variable %in% dat[(d()$pointNumber+1),]$ID,], x= ~x, y= ~value, type = 'scatter', mode = 'lines+markers', color = ~variable)  %>% layout(dragmode="box", showlegend = FALSE)
  # })

}
