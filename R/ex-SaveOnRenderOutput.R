library(plotly)
library(htmlwidgets)
library(shiny)

ui <- shinyUI(fluidPage(
  tags$h3('',class ='val2'),
  sliderInput("ci", "Value:", min = 0, max = 34, value=34, step=1),
  plotlyOutput("myPlot"),
  textOutput("outval2")
))

server <- shinyServer(function(input, output) {

  ci <- reactive(input$ci)

  output$myPlot <- renderPlotly({
    mdf <- mtcars %>% filter(mpg<ci())
    myPlot <- qplot(data=mdf, mpg, cyl)
    ggplotly(myPlot) %>% onRender("function(el, x, data) {
                                            val2 = data * 2
                                            console.log(val2)
                                            $('h3.val2').text('val2:'+val2);
                                            Shiny.onInputChange('val2', val2);
                                            }", data=ci())
  })
  output$outval2 <- renderPrint({sprintf("The calculated value is:%d",input$val2)})
}
)
shinyApp(ui, server)
