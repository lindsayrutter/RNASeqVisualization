library(shiny)
library(plotly)
library(data.table)
library(GGally)
library(reshape2)
library(hexbin)

ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("click")
)

server <- function(input, output, session) {
  #Create data
  set.seed(1)
  bindata <- data.frame(x=rnorm(100), y=rnorm(100))

  h <- hexbin (bindata, xbins = 5, IDs = TRUE, xbnds = range (bindata$x), ybnds = range (bindata$y))

  # As we have the cell IDs, we can merge this data.frame with the proper coordinates
  hexdf <- data.frame (hcell2xy (h),  ID = h@cell, counts = h@count)

  # I have tried different methods of generating the ggplot object
  #p <- ggplot(hexdf, aes(x=x, y=y, fill = counts)) + geom_hex(stat="identity")
  #p <- ggplot(hexdf, aes(x=x, y=y, fill = ID)) + geom_hex(stat="identity")
  #p <- ggplot(hexdf, aes(x=x, y=y, fill = counts, colours = ID)) + geom_hex(stat="identity")
  #p <- ggplot(hexdf, colours = ID, aes(x=x, y=y, colours = ID, fill = counts)) + geom_hex(stat="identity")
  p <- ggplot(hexdf, aes(x=x, y=y, fill = counts, ID=ID)) + geom_hex(stat="identity")

  output$plot <- renderPlotly({
    ggplotly(p)
  })

  d <- reactive(event_data("plotly_click"))

  output$click <- renderPrint({
    if (is.null(d())){
      "Click on a state to view event data"
    }
    else{
      str(d())
      #Next line would deliver all observations from original data frame (bindata) that are in the clicked hexbin... if d() from event_data() was returning ID instead of curveNumber
      #bindata[which(h@cID==d()$curveNumber),]
    }
  })
}

shinyApp(ui, server)
