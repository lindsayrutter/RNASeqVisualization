library(ggplot2)
library(shiny)

runApp(list(
  ui = fluidPage(
    plotOutput("plot",
               brush = brushOpts("plotBrush",
                                 delay = 5000,
                                 resetOnNew = TRUE)
               # resetOnNew = TRUE clears the brush
               # each time a new plot is displayed.
    ),
    p("Brushed Points:"),
    verbatimTextOutput("brushedPoints")
  ),
  server = function(input, output, session) {
    values <- reactiveValues(brush = NULL)

    output$plot <- renderPlot({
      if(is.null(values$brush)){
        ggplot(cars, aes(speed, dist)) + geom_point()
      } else {
        ggplot(cars, aes(speed, dist)) + geom_blank()
      }
    })

    observeEvent(input$plotBrush,{
      #Run this whenever points are brushed
      output$plot <- renderPlot({
        if(is.null(values$brush)){
          ggplot(cars, aes(speed, dist)) + geom_point()
          values$brush <- input$plotBrush
        } else {
          ggplot(cars, aes(speed, dist)) + geom_blank()
        }
      })
    }
    )

    output$brushedPoints <- renderPrint({
      values$brush
    })
  }
))
