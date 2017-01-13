library(fields)
library(dplyr)

ui <- basicPage(
  plotOutput("plot1", click = "plot_click"),
  verbatimTextOutput("info")
)

server <- function(input, output) {
  output$plot1 <- renderPlot({
    p <- ggplot(mtcars, aes(x=wt, y=mpg)) + geom_hex()
    pB <- ggplot_build(p)
    p
    #p <- ggplot(mtcars, aes(x=wt, y=mpg)) + geom_hex()
    #ggplotly(p)
  })
  
  B = as.matrix(select(pB$data[[1]],x,y),ncol=2)
  
  hexInd = c()
  for (i in 1: nrow(mtcars)){
    A = matrix(select(mtcars[i,],wt,mpg),ncol=2)
    hexInd = c(hexInd,which(rdist(A,B) == min(rdist(A,B))))
  }
  mtcars$hexInd = hexInd
  
  output$info <- renderPrint({
    # With base graphics, need to tell it what the x and y variables are.
    mousePoint <- nearPoints(mtcars, input$plot_click, xvar = "wt", yvar = "mpg")
    xVar <- mousePoint$wt
    yVar <- mousePoint$mpg
    C <- matrix(c(xVar,yVar),ncol=2)
    hexIndPoint <- which(rdist(C,B) == min(rdist(C,B)))
    mtcars[mtcars$hexInd %in% hexIndPoint,]
  })
}

shinyApp(ui, server)