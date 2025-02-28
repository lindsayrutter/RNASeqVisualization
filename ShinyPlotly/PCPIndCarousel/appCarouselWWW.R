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
library(edgeR)
library(stringr)
library(EDASeq)
library(matrixStats)
library(gridExtra)
library(reshape2)
library(scales)
library(gtools)
library(bsplus)

ui <- fluidPage(
  fluidRow(
    column(8, plotlyOutput("plot")),
    verbatimTextOutput("click"),
    column(4, plotlyOutput("plot2")),
    bs_carousel(id = "tabPrev", use_indicators = TRUE) %>%
      bs_append(
        content = bs_carousel_image(src = "knitr/birdSnow.jpg"),
        caption = bs_carousel_caption("Snow bird")
      ) %>%
      bs_append(
        content = bs_carousel_image(src = plotlyOutput("plot2")),
        caption = bs_carousel_caption("Green lake house")
      ) %>%
      bs_append(
        content = bs_carousel_image(src = "knitr/igloo.jpg"),
        caption = bs_carousel_caption("Glowing igloo")
      ) %>%
      bs_append(
        content = bs_carousel_image(src = "knitr/purpleLake.jpg"),
        caption = bs_carousel_caption("Purple lake")
      )
  )
)



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
      indDat <- as.data.frame(dat_long[variable %in% dat[d()$curveNumber+1,]$ID])
      str(indDat)
      # levels(indDat$group)[1]
      #levels(indDat$group)[2]
    }
  })

  output$plot2 <- renderPlotly({
    ax <- list(title = "", showticklabels = TRUE)
    ay <- list(title = "Read Count")
    indDat <- as.data.frame(dat_long[variable == dat[d()$curveNumber+1,]$ID])
    g1 <- levels(indDat$group)[1]
    g2 <- levels(indDat$group)[2]
    g1m <- mean(filter(indDat, group==g1)$value)
    g2m <- mean(filter(indDat, group==g2)$value)

    indDat %>% plot_ly(x = ~group, y = ~value, type = "scatter", marker = list(size = 10), color = ~group, colors = "Set2", hoverinfo = "text", text = paste0("Read count = ", format(round(indDat$value, 2), nsmall = 2))) %>% layout(xaxis = ax, yaxis = ay, legend = list(x = 0.35, y = -0.26)) %>% add_segments(x = g1, xend = g2, y = g1m, yend = g2m, showlegend = FALSE, line = list(color='#000000')) %>% add_trace(x = g1, y= g1m, showlegend = FALSE, hoverinfo = "text", text = paste0("Mean Read Count = ", round(g1m, digits = 2)), marker = list(color='#000000')) %>% add_trace(x = g2, y= g2m, showlegend = FALSE, hoverinfo = "text", text = paste0("Mean Read Count = ", round(g2m, digits = 2)), marker = list(color='#000000'))

  })
}

shinyApp(ui, server)
