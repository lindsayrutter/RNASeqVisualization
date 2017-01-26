# This script succesfully allows a user to click on a geom_hex and obtain the observations from the original data frame that were in that geom_hex. Only for ONE plot. Checks by also overplotting points on top of the geom_hexs.

library(shiny)
library(plotly)
library(data.table)
library(GGally)
library(reshape2)
library(hexbin)

ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("click"),
  verbatimTextOutput("test")
)

server <- function(input, output, session) {

  # Curve number to ID
  cnToID <- function(h){
    df <- data.frame(table(h@cID))
    colnames(df) <- c("hexID","count")
    cnID <- df[order(df$count,as.character(df$hexID)),]
    cnID$curveNumber <- seq(0, nrow(cnID)-1)
    return(cnID)
  }

  # Create data
  set.seed(1)
  # Last two arguments are needed to draw points on geom_hex
  bindata <- data.frame(ID = paste0("ID",1:100), x=rnorm(100), y=rnorm(100), C=1, D=1, E=1, counts=-1, hexID=-1)
  bindata$ID <- as.character(bindata$ID)
  h <- hexbin (bindata[,-1], xbins = 5, IDs = TRUE, xbnds = range (bindata$x), ybnds = range (bindata$y))
  hexdf <- data.frame (hcell2xy (h),  hexID = h@cell, counts = h@count)
  p <- ggplot(hexdf, aes(x=x, y=y, fill = counts, hexID=hexID)) + geom_hex(stat="identity")
  cnID <- cnToID(h)

  p2 <- ggplotly(p)
  for (i in 1:nrow(hexdf)){
    p2$x$data[[i]]$text <- gsub("<.*$", "", p2$x$data[[i]]$text)
  }

  d <- reactive(event_data("plotly_click"))
  clickID <- reactive(as.numeric(as.character(cnID[which(cnID$curveNumber==d()$curveNumber),]$hexID)))
  clickHex <- reactive(bindata[which(h@cID==clickID()),])

  output$test <- renderPrint({
    print("clickHex")
    str(clickHex())
  })

  output$plot <- renderPlotly({
    if (!is.null(d())){
      pp <- p + geom_point(data = clickHex(), aes(x=x, y=y)) + coord_equal()
      p2 <- ggplotly(pp)
      for (i in 1:nrow(hexdf)){
        p2$x$data[[i]]$text <- gsub("<.*$", "", p2$x$data[[i]]$text)
      }
    }
    p2
  })

  output$click <- renderPrint({
    if (is.null(d())){
      "Click on a state to view event data"
    }
    else{
      str(clickHex())
    }
  })

  }

shinyApp(ui, server)
