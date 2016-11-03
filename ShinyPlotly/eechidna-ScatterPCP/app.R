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
  # Creates data frame called data as follows:
  # ID         A         B         C         D         E fill
  # 1 ID1 0.2655087 0.2059746 0.9347052 0.4820801 0.8209463 gray
  # 2 ID2 0.3721239 0.1765568 0.2121425 0.5995658 0.6470602 gray
  # 3 ID3 0.5728534 0.6870228 0.6516738 0.4935413 0.7829328 gray
  # 4 ID4 0.9082078 0.3841037 0.1255551 0.1862176 0.5530363 gray
  # 5 ID5 0.2016819 0.7698414 0.2672207 0.8273733 0.5297196 gray
  # 6 ID6 0.8983897 0.4976992 0.3861141 0.6684667 0.7893562 gray
  rv <- reactiveValues(
    set.seed(1),
    data <- data.frame(ID = paste0("ID",1:10), A = runif(10), B = runif(10), C = runif(10), D = runif(10), E = runif(10), fill = factor(rep("gray", 10), levels = c("gray", palette))),
    data$ID <- as.character(data$ID)
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
    selected <- rv$data$Electorate %in% event_data("plotly_selected")$key
    updateRV(selected)
  })

  # Convert DF from scatterplot to PCP
  datt <- data.frame(t(data))
  fillVec <- datt[]
  #datt <- data.frame(t(select(data, -fill)))
  names(datt) <- as.matrix(datt[1, ])
  datt <- datt[-1, ]
  datt[] <- lapply(datt, function(x) type.convert(as.character(x)))
  setDT(datt, keep.rownames = TRUE)[]
  colnames(datt)[1] <- "x"
  dat_long <- melt(datt, id.vars ="x" )
    
  output$ScatterPlot <- renderPlotly({
    plot_ly(data = data, x = ~A, y = ~B)
  })
  
  output$PCP <- renderPlotly({
    plot_ly(dat_long, x= ~x, y= ~value, type = 'scatter', mode = 'lines+markers', color = ~variable)  %>% layout(dragmode="box", showlegend = FALSE)
  })
  
}

shinyApp(ui, server)
