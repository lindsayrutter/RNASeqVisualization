# setwd("~/RNASeqVisualization/ShinyPlotly/eechidna-master
# library(shiny)
# runApp("App")

library(dplyr)
library(shiny)
library(plotly)
library(data.table)
library(eechidna)
library(gtools)

palette = c('#1B9E77', '#F0027F', '#E6AB02', '#66A61E', '#7570B3', '#D95F02', '#3690C0')

brush_opts <- function(id, ...) {
  brushOpts(id = id, direction = "x", resetOnNew = TRUE, ...)
}

ui <- fluidPage(
  shinyjs::useShinyjs(),
  fluidRow(
    column(
      width = 1,
      checkboxInput("show", "Show Controls")
    ),
    column(
      width = 1,
      actionButton("clear", "Clear Selections")
    )
  ),
  conditionalPanel(
    "input.show",
    fluidRow(
      column(
        width = 2,
        checkboxInput("persist", "Persistant selections?", FALSE),
        shinyjs::colourInput("color", "Selection color:", palette = "limited", allowedCols = palette)
      )
    )
  ),
  fluidRow(
    column(
      width = 3,
      plotlyOutput("ScatterPlot")
    ),
    column(
      width = 3,
      plotlyOutput("PCP")
    )
  )
)


server <- function(input, output) {

  # initiate selection data and *input brushes* as reactive values so we can
  # "clear the world" - http://stackoverflow.com/questions/30588472/is-it-possible-to-clear-the-brushed-area-of-a-plot-in-shiny/36927826#36927826
  rv <- reactiveValues(
    data = data.frame(ID = as.character(paste0("ID",1:10)), A = runif(10), B = runif(10), C = runif(10), D = runif(10), E = runif(10), fill = factor(rep("gray", 10), levels = c("gray", palette)))
  )

  # clear brush values and remove the div from the page
  observeEvent(input$clear, {
    rv$data$fill <- "gray"
  })

  # reusable function for "telling the world" about the selection
  # it should modify the reactive value _once_ since shiny will send messages
  # on every modification
  updateRV <- function(selected) {
    print(input$color)
    if (input$persist) {
      rv$data$fill[selected] <- input$color
    } else {
      fill <- rv$data$fill
      fill[rv$data$fill %in% input$color] <- "gray"
      print(input$color)
      fill[selected] <- input$color
      rv$data$fill <- fill
    }
  }

  observeEvent(event_data("plotly_selected"), {
    selected <- rv$data$ID %in% event_data("plotly_selected")$key
    updateRV(selected)
  })

  output$ScatterPlot <- renderPlotly({
    p <- ggplot(rv$data, aes(A, B, colour = fill, text = ID, key = ID)) +
      scale_colour_identity() + theme_bw() +
      theme(legend.position = "none") +
      geom_point(alpha = 0.5) + ylab(NULL) +
      xlab("Read Count A") +
      ylab("Read Count B") +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.major = element_blank())
    ggplotly(p, tooltip = "text") %>% layout(dragmode = "select")
  })

  output$PCP <- renderPlotly({
    # Convert DF from scatterplot to PCP
    data2 <- melt(rv$data, id.vars = c("ID", "fill"), measure.vars = c("A","B","C","D","E"))
    data3 <- data2[mixedorder(data2$ID),]
    p <- ggplot(data3, aes(x = variable, y = value, colour = fill, text = ID, key = ID)) + geom_line(aes(group = ID), alpha = 0.8) + geom_point(alpha = 0.5, size = 0.001) + scale_colour_identity() + theme_bw() + theme(legend.position = "none") + xlab(NULL) + ylab("Read count")
    ggplotly(p, tooltip = "text") %>% layout(dragmode = "select")
  })
}

shinyApp(ui, server)
