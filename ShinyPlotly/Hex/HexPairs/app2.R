library(shiny)
library(plotly)
library(data.table)
library(GGally)
library(hexbin)

ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("click")
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

  set.seed(1)
   #dat <- data.frame(ID = paste0("ID",1:100), x = runif(100), y = runif(100), C = runif(100), D = runif(100), E = runif(100))
   #dat$ID <- as.character(dat$ID)
   #data <- dat



  bindata <- data.frame(ID = paste0("ID",1:100), A=rnorm(100), B=rnorm(100), C=rnorm(100), D=rnorm(100), E=rnorm(100))
  bindata$ID <- as.character(bindata$ID)

  my_fn <- function(data, mapping, ...){
    x = data[,c(as.character(mapping$x))]
    y = data[,c(as.character(mapping$y))]
    h <- hexbin(x=x, y=y, xbins=5, IDs=TRUE, xbnds=range(x), ybnds=range(y))
    hexdf <- data.frame (hcell2xy (h),  hexID = h@cell, counts = h@count)
    p <- ggplot(hexdf, aes(x=x, y=y, fill = counts, hexID=hexID)) + geom_hex(stat="identity")
    #cnID <- cnToID(h)
    p
  }

  # To 7 (include counts column, but not hexID)
  p <- ggpairs(bindata[,2:6], lower = list(continuous = my_fn))
  myMax = max(bindata[,2:6])
  myMin = min(bindata[,2:6])
  pS <- p
  for(i in 2:p$nrow) {
    for(j in 1:(i-1)) {
      pS[i,j] <- p[i,j] +
        scale_x_continuous(limits = c(myMin, myMax)) +
        scale_y_continuous(limits = c(myMin, myMax))
    }
  }

  output$plot <- renderPlotly({
    ggplotly(pS)
  })

  d <- reactive(event_data("plotly_click"))

  output$click <- renderPrint({
    if (is.null(d())){
      "Click on a state to view event data"
    }
    else{
      str(d())
      d()$curveNumber
      data[d()$curveNumber,]
      data[d()$curveNumber,]$ID
    }
  })
}

shinyApp(ui, server)
