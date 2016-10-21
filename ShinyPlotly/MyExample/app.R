library(shiny)
library(plotly)
library(data.table)

ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("click")
)

server <- function(input, output, session) {

  output$plot <- renderPlotly({

    plot_ly(z = state.area, text = state.name, locations = state.abb,
            type = 'choropleth', locationmode = 'USA-states') %>%
      layout(geo = g)
  })

  output$click <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)) "Click on a state to view event data" else str(d)
  })

}

shinyApp(ui, server)

##############################################################

dat <- data.frame(ID = paste0("ID",1:10), A = runif(10), B = runif(10), C = runif(10), D = runif(10), E = runif(10))
dat$ID <- as.character(dat$ID)

plot_ly(data = dat, x = ~A, y = ~B)

datt <- data.frame(t(dat))

names(datt) <- as.matrix(datt[1, ])
datt <- datt[-1, ]
datt[] <- lapply(datt, function(x) type.convert(as.character(x)))
setDT(datt, keep.rownames = TRUE)[]
colnames(datt)[1] <- "x"

dat_long <- melt(datt, id.vars ="x" )

p <- plot_ly(dat_long, x= ~x, y= ~value, type = 'scatter', mode = 'lines+markers', color = ~variable)  %>% layout(dragmode="box", showlegend = FALSE)
