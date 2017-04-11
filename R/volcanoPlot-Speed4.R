library(shiny)
library(plotly)

ui <- shinyUI(pageWithSidebar(
  headerPanel("Click the button"),
  sidebarPanel(
    sliderInput("threshP", "P-value:", min = 0, max = 1, value=1, step=0.05),
    #sliderInput("threshFC", "Fold change:", min = 0, max = 10, value=0, step=0.5),
    uiOutput("slider"),
    actionButton("goButton", "Go!")
  ),
  mainPanel(
    plotlyOutput("plot1"),
    verbatimTextOutput("click")
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
  maxTemp = max(abs(xMax), abs(xMin))

  #make dynamic slider
  output$slider <- renderUI({
    sliderInput("threshFC", "Fold change:", min=0, max=ceiling(maxTemp), value=ceiling((maxTemp)/3), step=0.5)
  })

  output$plot1 <- renderPlotly({
    # Take a dependency on input$goButton
    input$goButton

    # Use isolate() to avoid dependency on input$obs
    datInput <- isolate(subset(dat , pval < input$threshP & input$threshFC < FC))
    p <- qplot(datInput$FC, datInput$pval) +xlim(0, ceiling(maxTemp)) +ylim(0,1)
    ggplotly(p)
  })

  d <- reactive(event_data("plotly_selected"))

  output$click <- renderPrint({
    if (is.null(d())){
      "Click on a state to view event data"
    }
    else{
      str(d()$pointNumber)
      #datInput[d()$pointNumber,]
    }
  })

})

shinyApp(ui, server)
