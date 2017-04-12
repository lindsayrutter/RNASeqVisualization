library(shiny)
library(plotly)
library(htmltools)
library(shinyBS)

ui <- shinyUI(pageWithSidebar(
  headerPanel("Click the button"),
  sidebarPanel(
    sliderInput("val1", "Value 1:", min = 0, max = 1, value=0.5, step=0.1),
    sliderInput("val2", "Value 2:", min = 0, max = 1, value=0.5, step=0.1),
    uiOutput("uiExample")#,
    #actionButton("goButton", "Go!")
    ),
  mainPanel(
    plotlyOutput("plot1"),
    verbatimTextOutput("click")
  )
))

set.seed(1)
dat <- data.frame(Case = paste0("case",1:15), val1=runif(15,0,1), val2=runif(15,0,1))
dat$Case <- as.character(dat$Case)

xMax = max(dat$val1)
xMin = min(dat$val1)
yMax = max(dat$val2)
yMin = min(dat$val2)
maxTemp = max(abs(xMax), abs(xMin))

server <- shinyServer(function(input, output) {

  output$uiExample <- renderUI({
    tags$span(
      #tipify(bsButton("pointlessButton", "Button", style = "primary", size = "small"),
      #       "A Pointless Button", "This button is pointless!")
      tipify(actionButton("goButton", "Go!"), "A Pointless Button", "This button is pointless!")
    )
  })

  # datInput only validated once the go button is clicked
  datInput <- eventReactive(input$goButton, {
    subset(dat, val1 > input$val1 & val2 > input$val2)
  })

  output$plot1 <- renderPlotly({
    # will wait to render until datInput is validated
    plot_dat <- datInput()

    p <- qplot(plot_dat$val1, plot_dat$val2) + xlim(0, ceiling(maxTemp)) +ylim(0,1)
    ggplotly(p)
  })

  d <- reactive(event_data("plotly_selected"))
  output$click <- renderPrint({
    if (is.null(d())){
      "Click on a state to view event data"
    }
    else{
      #str(d()$pointNumber)
      datInput()[d()$pointNumber+1,] #Working now
    }
  })
})

shinyApp(ui, server)
