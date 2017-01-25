# This script is same as app-Scale.R, only now the confidence band is shaded region.

library(shiny)
library(plotly)
library(data.table)
library(GGally)

ui <- fluidPage(
  plotlyOutput("plot", height = "500px"),
  verbatimTextOutput("click")
)

server <- function(input, output, session) {

  # If change to set.seed(2), then it starts working
  set.seed(1)
  dat <- data.frame(ID = paste0("ID",1:20), A = runif(20), B = runif(20), C = runif(20), D = runif(20), E = runif(20))
  dat$ID <- as.character(dat$ID)
  data <- dat

  ciVal = 0.5
  myMax = max(data[,2:6])
  myMin = min(data[,2:6])
  myMid = (myMax-myMin)/2

  # Works okay!
  my_fn <- function(data, mapping, ...){
    x <- data[,c(as.character(mapping$x))]
    y <- data[,c(as.character(mapping$y))]
    keep <- sign(resid(lm(y-x-ciVal ~ 0)))==1 | sign(resid(lm(y-x+ciVal ~ 0)))==-1
    df <- data.frame(x = x[keep], y = y[keep])

    # Create one alpha transparent point in a plot if it otherwise has no dataframe. This is needed to prevent the plots from squishing together.
    if (nrow(df)==0){
      df <- data.frame(x = myMid, y = myMid)
      p <- ggplot(data = df, aes(x=x,y=y)) + geom_point(size=0.5, alpha=0) + geom_abline(intercept = 0, color = "red", size = 0.25) + geom_abline(intercept = ciVal, color ="blue", size = 0.25) + geom_abline(intercept = -1*ciVal, color ="blue", size = 0.25)
    }
    else{
      p <- ggplot(data = df, aes(x=x,y=y)) + geom_point(size=0.5) + geom_abline(intercept = 0, color = "red", size = 0.25) + geom_abline(intercept = ciVal, color ="blue", size = 0.25) + geom_abline(intercept = -1*ciVal, color ="blue", size = 0.25)
    }
    p
  }

  ggplot(data = df, aes(x=x,y=y)) + geom_point(size=0.5) + geom_abline(intercept = 0, color = "red", size = 0.25) + geom_ribbon(ymin = geom_abline(intercept = ciVal), ymax = geom_abline(intercept = -1*ciVal), alpha = "0.2")



  geom_ribbon(data=subset(x, 2 <= x & x <= 3), aes(ymin=twox,ymax=x2), fill="blue", alpha="0.5")

  # geom_ribbon
  pl = data.frame(Time = 0:10, menle = rnorm(11))
  pl$menlelb = pl$menle -1
  pl$menleub = pl$menle +1
  ggplot(pl, aes(Time)) +
    geom_line(aes(y=menle), colour="blue") +
    geom_ribbon(aes(ymin=menlelb, ymax=menleub), alpha=0.2)

  dat <- data.frame(cond1=c("a","a","b","b"),
                    cond2=c("c","d","c","d"),
                    x=c(1,5),
                    y=c(1,5),
                    sl=c(1,1.2,0.9,1.1),
                    int=c(0,0.1,0.1,0),
                    slopeU=c(1.1,1.3,1.2,1.2),
                    slopeL=c(.9,1,0.7,1))
  p <- ggplot(dat,aes(x=x,y=y,colour=cond1))
  p <- p + facet_grid(. ~ cond2)
  p <- p + geom_blank()
  p <- p + geom_abline(aes(intercept=int,slope=sl,colour=cond1),data=dat)
  p

  # create data #
  x<-as.data.frame(c(1,2,3,4))
  colnames(x)<-"x"
  x$twox<-2*x$x
  x$x2<-x$x^2
  # Set colours #
  blue<-rgb(0.8, 0.8, 1, alpha=0.25)
  clear<-rgb(1, 0, 0, alpha=0.0001)
  # Define region to fill #
  x$fill <- "no fill"
  x$fill[(x$x2 > x$twox) & (x$x <= 3 & x$x >= 2)] <- "fill"
  ggplot(x, aes(x=x, y=twox)) + geom_line(aes(y = twox)) + geom_line(aes(y = x2)) +
    geom_ribbon(data=subset(x, 2 <= x & x <= 3), aes(ymin=twox,ymax=x2), fill="blue", alpha="0.5") +
    scale_y_continuous(expand = c(0, 0), limits=c(0,20)) +
    scale_x_continuous(expand = c(0, 0), limits=c(0,5)) + scale_fill_manual(values=c(clear,blue))

  p <- ggpairs(data[,2:6], lower = list(continuous = my_fn))

  pS <- p
  for(i in 2:p$nrow) {
    for(j in 1:(i-1)) {
      pS[i,j] <- p[i,j] +
        scale_x_continuous(limits = c(myMin, myMax)) +
        scale_y_continuous(limits = c(myMin, myMax))
    }
  }

  ggPS <- ggplotly(pS)

  # Find the alpha transparent points, and ensure they have no interactivity
  myLength <- length(ggPS[["x"]][["data"]])
  for (i in 1:myLength){
    if (is.numeric(ggPS[["x"]][["data"]][[i]]$marker$opacity)){
      if (ggPS[["x"]][["data"]][[i]]$marker$opacity==0){
        ggPS[["x"]][["data"]][[i]]$hoverinfo <- "none"
      }
    }
    if (startsWith(ggPS[["x"]][["data"]][[i]]$text[1], "intercept") ||
        startsWith(ggPS[["x"]][["data"]][[i]]$text[1], "density") ||
        startsWith(ggPS[["x"]][["data"]][[i]]$text[1], "Corr")){
      ggPS[["x"]][["data"]][[i]]$hoverinfo <- "none"
    }
  }

  output$plot <- renderPlotly({
    ggPS
  })

  # Right now, hoverInfo works okay, but if click on the transparent dot, still get information about it in event_data()

  d <- reactive(event_data("plotly_selected"))

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
