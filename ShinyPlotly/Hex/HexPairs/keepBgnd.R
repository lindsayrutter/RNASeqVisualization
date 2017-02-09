# This script has a plotly background scatterplot. When a user clicks on a point of interest in the scatterplot, a horizontal line geom_hline is draw through that point. Currently, I think the entire plot is redrawn each time that happens (both the background scatterplot and the geom_hline.) I am wondering if it is possible to not have the scatterplot redrawn each time - but to simply reposition the geom_hline each time a user clicks on the scatterplot?

library(shiny)
library(plotly)

ui <- shinyUI(fluidPage(
  plotlyOutput("plot")
))

server <- shinyServer(function(input, output, session) {

  set.seed(1)
  data <- data.frame(ID = paste0("ID",1:10), A=rnorm(10), B=rnorm(10))
  data$ID <- as.character(data$ID)

  # Create static plot
  pS <- qplot(A, B, data=data)

  # Save static plot in reactive
  plotDat <- reactiveValues(main=pS)

  # Creates interactive plotly pI anytime static pS is reset
  observe({
    pI <- ggplotly(plotDat$main)
    output$plot <- renderPlotly({pI %>% layout(hovermode = 'closest')})
  })

   d <- reactive(event_data("plotly_click"))

   # This is only run when d() is changed (when user clicks on point)
  observeEvent(d(),{
      # Reset static plot to have no lines
      plotDat$main <- pS
      # Add line to static plot
      plotDat$main <- plotDat$main + geom_hline(yintercept = d()$y)
  })
})

shinyApp(ui = ui, server = server)
