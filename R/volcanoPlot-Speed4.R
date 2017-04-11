library(shiny)
library(plotly)

ui <- shinyUI(pageWithSidebar(
  headerPanel("Click the button"),
  sidebarPanel(
    sliderInput("val1", "Value 1:", min = 0, max = 1, value=0.5, step=0.1),
    sliderInput("val2", "Value 2:", min = 0, max = 1, value=0.5, step=0.1),
    actionButton("goButton", "Go!")
  ),
  mainPanel(
    plotlyOutput("plot1"),
    verbatimTextOutput("click")
  )
))

server <- shinyServer(function(input, output) {

  set.seed(1)
  dat <- data.frame(Case = paste0("case",1:15), val1=runif(15,0,1), val2=runif(15,0,1))
  dat$Case <- as.character(dat$Case)

  xMax = max(dat$val1)
  xMin = min(dat$val1)
  yMax = max(dat$val2)
  yMin = min(dat$val2)
  maxTemp = max(abs(xMax), abs(xMin))

  observeEvent(input$goButton,
       output$plot1 <- renderPlotly({
         # Use isolate() to avoid dependency on input$val1 and input$val2
         datInput <- isolate(subset(dat, val1 > input$val1 & val2 > input$val2))
         p <- qplot(datInput$val1, datInput$val2) +xlim(0, ceiling(maxTemp)) +ylim(0,1)
         ggplotly(p)
       })
  )

  d <- reactive(event_data("plotly_selected"))

  output$click <- renderPrint({
    if (is.null(d())){
      "Click on a state to view event data"
    }
    else{
      #str(d()$pointNumber)
      datInput[d()$pointNumber,] #Error: object 'datInput' not found
    }
  })
})

shinyApp(ui, server)
