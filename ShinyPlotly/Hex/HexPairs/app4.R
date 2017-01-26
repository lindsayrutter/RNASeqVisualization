ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("select"),
  plotlyOutput("plot2"),
  plotlyOutput("plot3")
)

server <- function(input, output, session) {

  # Create data
  set.seed(50)
  data <- data.frame(ID = paste0("Obsvn",1:100), A=rnorm(100), B=rnorm(100), C=rnorm(100))

  output$plot <- renderPlotly({
    plot <- qplot(A, B, data=data)
    ggplotly(plot, source = "subset") %>% layout(dragmode = "select")
  })



  # Reorganzing the original data structure into dat_long format to be plotted in line plot.
  datt <- data.frame(t(data))
  data.frame(t(data[,-c(ncol(data), ncol(data)-1)]))
  names(datt) <- as.matrix(datt[1, ])
  datt <- datt[-1, ]
  datt[] <- lapply(datt, function(x) type.convert(as.character(x)))
  setDT(datt, keep.rownames = TRUE)[]
  dat_long <- melt(datt, id.vars ="rn" )

  output$plot2 <- renderPlotly({
    plot_ly(dat_long, x= ~rn, y= ~value, type = 'scatter', mode = 'lines+markers', color = ~variable)  %>% layout(dragmode="box", showlegend = FALSE)
  })


  output$plot3 <- renderPlotly({

    d <- event_data("plotly_selected",source="subset")
    if (is.null(d)) "Click and drag events (i.e., select/lasso) appear here (double-click to clear)" else d


    temp <- subset(data)[subset(d, curveNumber == 0)$pointNumber + 1,]
    temp

    dattb <- data.frame(t(temp))
    data.frame(t(temp[,-c(ncol(temp), ncol(temp)-1)]))
    names(dattb) <- as.matrix(dattb[1, ])
    dattb <- dattb[-1, ]
    dattb[] <- lapply(dattb, function(x) type.convert(as.character(x)))
    setDT(dattb, keep.rownames = TRUE)[]
    dat_long <- melt(dattb, id.vars ="rn" )
    dat_long
    #dat_long2 <- melt(temp, id.vars ="rn" )
    #dat_long2

    plot_ly(dat_long, x= ~rn, y= ~value, type = 'scatter', mode = 'lines+markers', color = ~variable)  %>% layout(dragmode="box", showlegend = FALSE)
  })


}


shinyApp(ui, server)
