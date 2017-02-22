library(shiny)
library(plotly)
library(data.table)
library(dplyr)

ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("click")#,
  # There could be >1 ploti's here. I am not sure how to do this.
  #plotlyOutput("plots")
)

server <- function(input, output, session) {

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
      length(d()$y)
    }
  })

  #dynamically create the right number of htmlOutput
  output$plots <- renderUI({
    plot_output_list <- lapply(d()$y, function(i) {
      plotname <- paste0("plot", i)
      htmlOutput(plotname)
    })
    tagList(plot_output_list)
  })

  # PROBLEM HERE - The for loop must be in a reactive context
  observe({
    for (i in 1:length(d()$y)) {
      local({
        my_i <- i
        plotname <- paste0("plot", my_i)

        output[[plotname]] <- renderPlotly({
          yVar <- d()$y[i]
          plot_ly(x = c(4, 4), y = c(0, 10), mode = "lines") %>% add_trace(x = c(3, 5), y = c(yVar, yVar))
        })
      })
    }
  })

}

shinyApp(ui, server)
