library(shiny)
library(plotly)
library(data.table)

server <- function(input, output, session) {

  set.seed(1)
  dat <- data.frame(ID = paste0("ID",1:10), A = runif(10), B = runif(10), C = runif(10), D = runif(10), E = runif(10))
  dat$ID <- as.character(dat$ID)

  output$plot <- renderPlotly({
    plot_ly(data = dat, x = ~A, y = ~B)
  })

  d <- reactive(event_data("plotly_selected"))

  output$click <- renderPrint({
    if (is.null(d())){
      "Click on a state to view event data"
    }
    else{
      str(d())
      d()$pointNumber
      dat[(d()$pointNumber+1),]
      dat[(d()$pointNumber+1),]$ID
      str(dat[(d()$pointNumber+1),]$ID)
    }
  })

  # Convert DF from scatterplot to PCP
  datt <- data.frame(t(dat))
  names(datt) <- as.matrix(datt[1, ])
  datt <- datt[-1, ]
  datt[] <- lapply(datt, function(x) type.convert(as.character(x)))
  setDT(datt, keep.rownames = TRUE)[]
  colnames(datt)[1] <- "x"
  dat_long <- melt(datt, id.vars ="x" )

  output$plot2 <- renderPlotly({
    plot_ly(dat_long[dat_long$variable %in% dat[(d()$pointNumber+1),]$ID,], x= ~x, y= ~value, type = 'scatter', mode = 'lines+markers', color = ~variable)  %>% layout(dragmode="box", showlegend = FALSE)
  })

}







