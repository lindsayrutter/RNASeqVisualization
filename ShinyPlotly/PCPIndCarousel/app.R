library(shiny)
library(plotly)
library(data.table)
library(dplyr)

ui <- shinyUI(pageWithSidebar(
  headerPanel("Dynamic number of plots"),

  sidebarPanel(
    plotlyOutput("plot"),
    verbatimTextOutput("click")
  ),

  mainPanel(
    # This is the dynamic UI for the plots
    uiOutput("plots")
  )
))


server <- shinyServer(function(input, output) {

  set.seed(1)
  dat <- data.frame(ID = paste0("ID",1:10), x = runif(10), y = runif(10))
  dat$ID <- as.character(dat$ID)

  output$plot <- renderPlotly({
    plot_ly(data = dat, x= ~x, y= ~y)
  })

  d <- reactive(event_data("plotly_selected"))

  output$click <- renderPrint({
    if (is.null(d())){
      "Click on a state to view event data"
    }
    else{
      str(length(d()$y))
    }
  })

  observeEvent(d(),{
  # Insert the right number of plot output objects into the web page
  output$plots <- renderUI({

    lengthY <- reactive((length(d()$y)))
    if (lengthY()<1){
      plot_output_list <- list()
    }
    else{
      plot_output_list <- lapply(1:lengthY(), function(i) {
        plotname <- paste("plot", i, sep="")
        plotOutput(plotname, height = 280, width = 250)
      })
    }

    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list)
  })
  })

  # Call renderPlot for each one. Plots are only actually generated when they
  # are visible on the web page.
  observeEvent(d(),{
    lengthY <- reactive((length(d()$y)))
  for (i in 1:lengthY()) {
    # Need local so that each item gets its own number. Without it, the value
    # of i in the renderPlot() will be the same across all instances, because
    # of when the expression is evaluated.
    local({
      my_i <- i
      plotname <- paste("plot", my_i, sep="")

      output[[plotname]] <- renderPlot({
        plot(1:my_i, 1:my_i,
             xlim = c(1, lengthY()),
             ylim = c(1, lengthY()),
             main = paste("1:", my_i, ".  n is ", input$n, sep = "")
        )
      })
    })
  }
  })
})

shinyApp(ui, server)
