library(shiny)
library(plotly)
library(data.table)
library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(GGally)
library(edgeR)

palette = c('#1B9E77', '#F0027F', '#E6AB02', '#66A61E', '#7570B3', '#D95F02', '#3690C0')

brush_opts <- function(id, ...) {
  brushOpts(id = id, direction = "x", resetOnNew = TRUE, ...)
}

ui <- fluidPage(
  shinyjs::useShinyjs(),
  fluidRow(
    column(
      width = 2,
      checkboxInput("show", "Show Controls")
    ),
    column(
      width = 2,
      actionButton("clear", "Clear Selections")
    ),
    column(
      width = 2,
      actionButton("delete", "Delete Selections")
    ),
    column(
      width = 2,
      downloadButton('downloadData', 'Download')
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
      width = 10,
      plotlyOutput("RectPlot")
    )
  )
)

server <- function(input, output) {

  load("sigTab.rda")
  data = sig.tab
  # initiate selection data and *input brushes* as reactive values so we can "clear the world"
  rv <- reactiveValues(
    data = cbind(data, fill = factor(rep("gray", nrow(data)), levels = c("gray", palette)))
  )

  # clear brush values and remove the div from the page
  observeEvent(input$clear, {
    rv$data$fill <- "gray"
  })

  observeEvent(input$delete, {
    selected <- rv$data$genes %in% event_data("plotly_selected")$key
    # selected is format: logi [1:10] TRUE FALSE FALSE FALSE FALSE FALSE FALSE
    # cat(file=stderr(), "selected", selected, str(selected))
    rv$data <- filter(rv$data,!selected)
  })

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
    selected <- rv$data$genes %in% event_data("plotly_selected")$key
    updateRV(selected)
  })

  # change text = genes to text = ID
  output$RectPlot <- renderPlotly({
    # p <- ggplot(rv$data, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, text = genes, key = genes, colour = fill)) + geom_rect(fill = 'purple', colour = 'black', size = 0.3, alpha = 0.1)

    p <- ggplot(rv$data, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, colour = fill, text = genes, key = genes)) + geom_rect(size = 0.3, fill = NA) + scale_colour_identity() + theme_bw()
    ggplotly(p, tooltip = "text") %>% layout(dragmode = "select")
  })
}

shinyApp(ui, server)

# library(ggplot2)
# library(dplyr)
# library(plotly)
# set.seed(1)
# data <- data.frame(Name = paste0("Name",seq(1:10)), xmin = runif(10, 0, 1), xmax = runif(10, 1, 2), ymin = runif(10, 0, 1), ymax = runif(10, 1, 2), fill = sample(c("black","purple"),10, replace=TRUE) )
# 
# p <- ggplot(data, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = NULL, colour = fill, text = Name)) + geom_rect(size = 0.3, alpha = 0.5) + scale_colour_identity() + theme_bw()
# ggplotly(p, tooltip = "text") %>% layout(dragmode = "select")

