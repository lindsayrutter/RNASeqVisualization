library(ggplot2)
library(shiny)
library(plotly)
library(htmlwidgets)

ui <- basicPage(
  plotlyOutput("plot1"),
  verbatimTextOutput("rectdf")
)

server <- function(input, output) {

  p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point(alpha=0) + xlim(0,5) +ylim(-3,3)
  gp <- ggplotly(p)

  set.seed(3)
  myDF <- data.frame(X1=rnorm(10,-1), X2=rnorm(10,-1), X3=rnorm(10,-1), X4=rnorm(10,1), X5=rnorm(10,1), X6=rnorm(10,1))
  colNms <- colnames(myDF)
  nVar <- length(colNms)

  inputRectDf <- reactive({
    req(input$rects)
    # data comes back as a big character vector
    # so we reformat it as a dataframe here
    df <- data.frame(t(matrix(input$rects,nrow=8)))
    names(df) <- names(input$rects)[1:8]
    return(df)
  })
  output$rectdf <- renderPrint({print(inputRectDf())})


  output$plot1 <- renderPlotly({
    gp %>% layout(dragmode="select") %>%
      onRender("
       function(el, x, data) {
       var rects = [];

       var myDF = data.myDF
       var Traces = [];
       var dLength = myDF.length
       var vLength = data.nVar
       var cNames = data.colNms
       for (a=0; a<dLength; a++){
       xArr = [];
       yArr = [];
       for (b=0; b<vLength; b++){
       xArr.push(b)
       yArr.push(myDF[a][cNames[b]]);
       }
       var pcpLine = {
       x: xArr,
       y: yArr,
       mode: 'lines',
       line: {
       color: 'orange',
       width: 1
       },
       opacity: 0.9,
       }
       Traces.push(pcpLine);
       }
       Plotly.addTraces(el.id, Traces);

       el.on('plotly_selected', function(e) {
         var dLength = myDF.length
         var selectedPCP = []
         var xMin = e.range.x[0]
         var xMax = e.range.x[1]
         var yMin = e.range.y[0]
         var yMax = e.range.y[1]

         console.log([xMin, xMax, yMin, yMax])

         var Traces = []
         var drawRect = {
           type: 'rect',
           x0: xMin,
           y0: yMin,
           x1: xMax,
           y1: yMax,
           line: {
             color: 'gray',
             width: 1
           },
           fillcolor: 'gray',
           opacity: 0.25
         }
         rects.push(drawRect);
         var update = {
          shapes:rects
         }
         Plotly.relayout(el.id, update)
         Shiny.onInputChange('rects', rects); // make the rects available to shiny
       })
       }", data = list(myDF = myDF, nVar = nVar, colNms = colNms))})

  }
shinyApp(ui, server)
