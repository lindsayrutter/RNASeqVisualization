# This script is same as app-SeedSet.R, only now min max are scaled for all subplots (not just bottom-left)

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
  data <- data.frame(ID = paste0("ID",1:20), A = runif(20), B = runif(20), C = runif(20), D = runif(20), E = runif(20))
  data$ID <- as.character(data$ID)

  ciVal = 0.5
  myMax = max(data[,2:6])
  myMin = min(data[,2:6])
  myMid = (myMax-myMin)/2
  data2 <- data.frame(x = c(myMin, myMax), y = c(myMin, myMax))

  # Works okay!
  my_fn <- function(data, mapping, ...){
    x <- data[,c(as.character(mapping$x))]
    y <- data[,c(as.character(mapping$y))]
    keep <- abs(x - y) >= ciVal
    df <- data.frame(x = x[keep], y = y[keep])

    # Create one alpha transparent point in a plot if it otherwise has no dataframe. This is needed to prevent the plots from squishing together.
    if (nrow(df)==0){
      df <- data.frame(x = myMid, y = myMid)
      p <- ggplot(data = df, aes(x=x,y=y)) +
            geom_point(size=0.5, alpha=0) +
            geom_ribbon(data=data2, aes(x=x, ymin = y-ciVal, ymax = y+ciVal), fill = "lightgrey") +
            geom_abline(intercept = 0, color = "white", size = 0.25) +
            scale_x_continuous(limits = c(myMin, myMax)) +
            scale_y_continuous(limits = c(myMin, myMax)) +
            scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))
    }
    else{
      p <- ggplot(data = df, aes(x=x,y=y)) +
            geom_point(colour = "grey", size=0.5) +
            geom_ribbon(data=data2, aes(x=x, ymin = y-ciVal, ymax = y+ciVal), fill = "lightgrey") +
            geom_abline(intercept = 0, color = "white", size = 0.25) +
            scale_x_continuous(limits = c(myMin, myMax)) +
            scale_y_continuous(limits = c(myMin, myMax)) +
            scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))
    }
    p
  }

  p <- ggpairs(data[,2:6], lower = list(continuous = my_fn)) + theme_bw()

  ggPS <- ggplotly(p)

  # Find the alpha transparent points, and ensure they have no interactivity
    myLength <- length(ggPS[["x"]][["data"]])
    for (i in 1:myLength){
      if (is.numeric(ggPS[["x"]][["data"]][[i]]$marker$opacity)){
        if (ggPS[["x"]][["data"]][[i]]$marker$opacity==0){
            ggPS[["x"]][["data"]][[i]]$hoverinfo <- "none"
        }
      }
     #if (startsWith(ggPS[["x"]][["data"]][[i]]$text[1], "intercept") ||
        # startsWith(ggPS[["x"]][["data"]][[i]]$text[1], "density") ||
       #  startsWith(ggPS[["x"]][["data"]][[i]]$text[1], "Corr")){
         ggPS[["x"]][["data"]][[i]]$hoverinfo <- "none"
      #}
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
