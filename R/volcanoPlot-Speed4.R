library(shiny)
library(plotly)

ui <- shinyUI(pageWithSidebar(
  headerPanel("Click the button"),
  sidebarPanel(
    sliderInput("threshP", "P-value:", min = 0, max = 1, value=1, step=0.05),
    sliderInput("threshFC", "Fold change:", min = 0, max = 10, value=0, step=0.5),

    actionButton("goButton", "Go!")
  ),
  mainPanel(
    plotlyOutput("plot1"),
    verbatimTextOutput("selectedValues")
  )
))

server <- shinyServer(function(input, output) {

  set.seed(1)
  #threshP <- reactive(input$threshP)
  #threshFC <- reactive(input$threshFC)

  dat <- data.frame(ID = paste0("ID",1:14), FC=runif(14,0,10), pval=runif(14,0,1))
  dat$ID <- as.character(dat$ID)

  # x-axis FC, y-axis pval
  xMax = max(dat$FC)
  xMin = min(dat$FC)
  yMax = max(dat$pval)
  yMin = min(dat$pval)

  output$plot1 <- renderPlotly({
    # Take a dependency on input$goButton
    input$goButton

    # Use isolate() to avoid dependency on input$obs
    datInput <- isolate(subset(dat , pval < input$threshP & input$threshFC < FC))
    p <- qplot(datInput$FC, datInput$pval)
    ggplotly(p)
  })
})

shinyApp(ui, server)
