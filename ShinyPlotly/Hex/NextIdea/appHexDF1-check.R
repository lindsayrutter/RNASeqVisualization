library(shiny)
library(plotly)
library(GGally)
library(reshape2)
library(hexbin)

ui <- fluidPage(
  plotlyOutput("plot"),
  checkboxInput("squarePoints", label = "Switch to points?"),
  verbatimTextOutput("click"),
  HTML("Check the work:"),
  plotlyOutput("plot1")
)

server <- function(input, output, session) {
  #Create data
  set.seed(1)
  bindata <- data.frame(myIndex = factor(paste0("ID",1:100)), x=rnorm(100), y=rnorm(100))

  h <- hexbin (bindata[,2:3], xbins = 5, IDs = TRUE, xbnds = range(bindata$x), ybnds = range(bindata$y))

  # As we have the cell IDs, we can merge this data.frame with the proper coordinates
  hexdf <- data.frame (hcell2xy (h),  ID = h@cell, counts = h@count)

  #New code added below ###
  counts <- hexTapply(h, bindata$myIndex, table)  #list of 26
  counts <- t(simplify2array (counts))
  counts <- melt (counts)                 #2600 rows = 26 hexagons * 100 observations
  colnames (counts)  <- c ("ID", "myIndex", "present")

  allhex <- merge (counts, hexdf)         #2600 rows = 26 hexagons * 100 observations
  #rename hex coordinates
  names(allhex)[names(allhex) %in% c("x", "y")] <- c("hex.x", "hex.y")
  allhex <- merge(allhex, bindata)
  somehex <- allhex[allhex$present > 0,]  #100 rows (original data)

  #Plotly graphs objects in a certain order, so sort the lookup data by the same order
  #in which it's plotted.
  #No idea how curveNumber plots data. First by counts, then by ...?
  #pointNumber seems more straightforward.
  sorthex <- hexdf[with(hexdf, order(ID)), ]

  #Create a switch to change between geom_hex() and geom_point()
  switchPoints <- reactive(if(input$squarePoints) {
    geom_point(shape = 22, size = 10)
  } else {
    geom_hex(stat = "identity")
  })

  hexdf$myIndex <- "na" #Added here for second plotly
  ### New code added above ###

  p <- reactive(ggplot(hexdf, aes(x=x, y=y, fill = counts))  + coord_equal() +
                  switchPoints() )

  output$plot <- renderPlotly({
    ggplotly(p())
  })

  d <- reactive(event_data("plotly_click"))
  #pointNumber = index starting from 0
  hexID <- reactive(sorthex[d()$pointNumber + 1, "ID"])

  output$click <- renderPrint({
    if (is.null(d())){
      "Click on a state to view event data"
    }
    else{
      list(
        str(d()),
        somehex[somehex$ID == hexID(),]
      )
    }
  })

  #Check your work: plot raw data over hexagons
  p.check <- ggplot(hexdf, aes(x=x, y=y, fill = counts)) + geom_hex(stat="identity") +
    geom_point(data = somehex, aes(x=x, y=y)) + coord_equal()

  output$plot1 <- renderPlotly({
    ggplotly(p.check + aes(label= myIndex) )
  })


}

shinyApp(ui, server)
