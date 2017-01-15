# Taking into account most recent suggestions - but only with one plot (no facets)

library(shiny)
library(plotly)
library(data.table)
library(GGally)
library (reshape2)

ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("click")
)

server <- function(input, output, session) {

  set.seed(1) #Create data
  bindata <- data.frame(x=rnorm(100), y=rnorm(100))
  # fac_probs <- dnorm(seq(-3, 3, length.out=26))
  # fac_probs <- fac_probs/sum(fac_probs)
  # bindata$factor <- sample(letters, 100, replace=TRUE, prob=fac_probs)
  # bindata$factor <- as.factor (bindata$factor)

  h <- hexbin (bindata, xbins = 5, IDs = TRUE, xbnds = range (bindata$x), ybnds = range (bindata$y))

  counts <- hexTapply (h, bindata$factor, table)
  counts <- t (simplify2array (counts))
  counts <- melt (counts)
  # Each of the 20 facets has 26 IDs (with values between 3 and 40) (20*26=520)
  # Counts between 0 and 5
  colnames (counts)  <- c ("ID", "factor", "counts")

  # As we have the cell IDs, we can merge this data.frame with the proper coordinates
  hexdf <- data.frame (hcell2xy (h),  ID = h@cell)
  hexdf <- merge (counts, hexdf)
  # ggplotting (use the command below) this yields the correct bin sizes, but the figure has a bit weird appearance: 0 count hexagons are drawn, but only where some other facet has this bin populated.
  hexdf$counts [hexdf$counts == 0] <- NA

  p <- ggplot(hexdf, aes(x=x, y=y, fill = counts)) + geom_hex(stat="identity") + facet_wrap(~factor) + coord_equal () + scale_fill_continuous (low = "grey80", high = "#000040", na.value = "#00000000")

  output$plot <- renderPlotly({
    ggplotly(p)
  })

  d <- reactive(event_data("plotly_click"))

  output$click <- renderPrint({
    if (is.null(d())){
      "Click on a state to view event data"
    }
    else{
      #str(d())
      #d()$curveNumber
      hexdf[d()$curveNumber,]
      #hexdf[d()$curveNumber,]$ID
    }
  })
}

shinyApp(ui, server)
