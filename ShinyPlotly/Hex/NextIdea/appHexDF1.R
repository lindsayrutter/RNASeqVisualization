library(shiny)
library(plotly)
library(data.table)
library(GGally)
library(reshape2)
library(hexbin)

ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("click")
  # HTML("Check the work:"),
  # plotlyOutput("plot1")
)

server <- function(input, output, session) {

  # Curve number to ID
  cnToID <- function(h){
    df <- data.frame(table(h@cID))
    colnames(df) <- c("ID","count")
    cnID <- df[order(df$count,as.character(df$ID)),]
    cnID$curveNumber <- seq(0, nrow(cnID)-1)
    return(cnID)
  }

  # Create data
  set.seed(1)
  bindata <- data.frame(x=rnorm(100), y=rnorm(100))
  h <- hexbin (bindata, xbins = 5, IDs = TRUE, xbnds = range (bindata$x), ybnds = range (bindata$y))
  hexdf <- data.frame (hcell2xy (h),  ID = h@cell, counts = h@count)
  p <- ggplot(hexdf, aes(x=x, y=y, fill = counts, ID=ID)) + geom_hex(stat="identity")
  cnID <- cnToID(h)

  output$plot <- renderPlotly({
    p2 <- ggplotly(p)
    for (i in 1:nrow(hexdf)){
      p2$x$data[[i]]$text <- gsub("<.*$", "", p2$x$data[[i]]$text)
    }
    p2
  })

  d <- reactive(event_data("plotly_click"))

  output$click <- renderPrint({
    if (is.null(d())){
      "Click on a state to view event data"
    }
    else{
      clickID <- as.numeric(as.character(cnID[which(cnID$curveNumber==d()$curveNumber),]$ID))
      #clickID
      clickHex <- bindata[which(h@cID==clickID),]
      clickHex
    }
  })

  # output$plot1 <- renderPlot({
  #   clickID <- as.numeric(as.character(cnID[which(cnID$curveNumber==d()$curveNumber),]$ID))
  #   clickHex <- bindata[which(h@cID==clickID),]
  #
  #   #Check your work: plot raw data over hexagons
  #   p.check <- ggplot(hexdf, aes(x=x, y=y, fill = counts)) + geom_hex(stat="identity") +
  #     geom_point(data = clickHex, aes(x=x, y=y)) + coord_equal()
  #   ggplotly(p.check)# + aes(label= myIndex) )
  # })

  }

shinyApp(ui, server)
