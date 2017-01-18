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
  h <- hexbin (bindata[,-1], xbins = 5, IDs = TRUE, xbnds = range (bindata$A), ybnds = range (bindata$B))
  hexdf <- data.frame (hcell2xy (h),  hexID = h@cell, counts = h@count)
  p <- ggplot(hexdf, aes(x=x, y=y, fill = counts, hexID=hexID)) + geom_hex(stat="identity")
  cnID <- cnToID(h)


  my_fn <- function(data, mapping, ...){
    # data is 100 rows of
    #A           B C D E
    #1 -0.3410670 -0.70756823 1 1 1
    #2  1.5024245  1.97157201 1 1 1
    #3  0.5283077 -0.08999868 1 1 1
    #str(data)
    # data[,c(varA)] ; varA = "A"
    #mappingX <- as.name("A")
    #is.name(an) # TRUE
    #mode(an)   # name
    #typeof(an) # symbol
    xbnds = range (data[,c(as.character(mapping$x))])
    ybnds = range (data[,c(as.character(mapping$y))])

    #h <- hexbin (data, xbins = 5, IDs = TRUE, xbnds = range (data[,c(as.character(mapping$x))]), ybnds = range (data[,c(as.character(mapping$y))]))

    #if(!missing(xbnds) && any(sign(xbnds - range(x)) == c(1,-1)))

    print(str(mapping$x))
    print(str(mapping$y))
    print(xbnds)
    print(ybnds)
    print(range(x))
    print(range(y))
    #print(head(data))
    #print(str(h))

    #hexdf <- data.frame (hcell2xy (h),  hexID = h@cell, counts = h@count)
    #p <- ggplot(hexdf, aes(x=mapping$x, y=mapping$y, fill = counts, hexID=hexID)) + geom_hex(stat="identity")
    #cnID <- cnToID(h)
    #p
  }



  if(!missing(xbnds) && any(sign(xbnds - range(x)) == c(1,-1)))

  # To 7 (include counts column, but not hexID)
  p <- ggpairs(bindata[,2:6], lower = list(continuous = my_fn))
  myMax = max(data[,2:6])
  myMin = min(data[,2:6])
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
