ui <- basicPage(
  plotOutput("plot1", brush = "plot_brush"
  ),
  verbatimTextOutput("info")
)

server <- function(input, output) {

  output$plot1 <- renderPlot({
    plot(mtcars$wt, mtcars$mpg)
  })

  output$info <- renderText({
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1),
             " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
    }

    paste0("brush: ", xy_range_str(input$plot_brush))
  })
}

shinyApp(ui, server)
