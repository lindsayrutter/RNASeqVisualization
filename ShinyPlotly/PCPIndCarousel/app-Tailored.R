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
  dat <- data.frame(ID = paste0("ID",1:10), A.1 = runif(10), A.2 = runif(10), A.3 = runif(10), B.1 = runif(10), B.2 = runif(10), B.3 = runif(10))
  dat$ID <- as.character(dat$ID)

  # Convert DF from scatterplot to PCP
  datt <- data.frame(t(dat))
  names(datt) <- as.matrix(datt[1, ])
  datt <- datt[-1, ]
  datt[] <- lapply(datt, function(x) type.convert(as.character(x)))
  setDT(datt, keep.rownames = TRUE)[]
  colnames(datt)[1] <- "x"
  dat_long <- melt(datt, id.vars ="x" )
  dat_long <- separate(dat_long, x, c("group", "rep"), remove=FALSE)
  dat_long$group <- factor(dat_long$group)

  output$plot <- renderPlotly({
    plot_ly(dat_long, x= ~x, y= ~value, type = 'scatter', mode = 'lines+markers', color = ~variable)  %>% layout(dragmode="box", showlegend = FALSE)
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
        plotlyOutput(plotname, height = 280, width = 250)
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

      output[[plotname]] <- renderPlotly({
        plot_ly(x = c(4, 4), y = c(0, 10), mode = "lines") %>% add_trace(x = c(3, 5), y = c(lengthY(), lengthY()))
      })
    })
  }
  })
})

shinyApp(ui, server)
