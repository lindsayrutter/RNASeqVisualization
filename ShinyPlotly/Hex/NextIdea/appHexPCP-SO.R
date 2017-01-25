library(shiny)
library(plotly)
library(data.table)
library(reshape2)

ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("select"),
  plotlyOutput("plot2"),
  plotlyOutput("plot3")
)

server <- function(input, output, session) {

  # Create data
  set.seed(50)
  data <- data.frame(ID = paste0("Obsvn",1:100), A=rnorm(100), B=rnorm(100), C=rnorm(100))

  output$plot <- renderPlotly({
    plot <- qplot(A, B, data=data)
    ggplotly(plot)
  })

  sel <- reactive(event_data("plotly_selected"))

  output$select <- renderPrint({
    if (is.null(sel())){
      "Row of data corresponding to selected point(s)"
    }
    else{
      sel()$pointNumber+1
    }
  })

  # Reorganzing the original data structure into dat_long format to be plotted in line plot.
  datt <- data.frame(t(data))
  data.frame(t(data[,-c(ncol(data), ncol(data)-1)]))
  names(datt) <- as.matrix(datt[1, ])
  datt <- datt[-1, ]
  datt[] <- lapply(datt, function(x) type.convert(as.character(x)))
  setDT(datt, keep.rownames = TRUE)[]
  dat_long <- melt(datt, id.vars ="rn" )

  output$plot2 <- renderPlotly({
    plot_ly(dat_long, x= ~rn, y= ~value, type = 'scatter', mode = 'lines+markers', color = ~variable)  %>% layout(dragmode="box", showlegend = FALSE)
  })

  # Plot2 had too many lines (because all rows in the original dataset were used, and each line represents a row). I would like to only plot lines for the rows that correspond to points selected by the user. Hence, I would like to reorganize the original data structure that is subsetted by the rows selected by the user (data[sel()$pointsNumber+1,]) into dat_long format to be plotted in line plot
  datt <- reactive(data.frame(t(data[sel()$pointsNumber+1,])))
  reactive(data.frame(t(data[,-c(ncol(data), ncol(data)-1)])))
  reactive(names(datt()) <- as.matrix(datt()[1, ]))
  reactive(datt() <- datt()[-1, ])
  reactive(datt()[] <- lapply(datt(), function(x) type.convert(as.character(x))))
  reactive(setDT(datt(), keep.rownames = TRUE)[])
  dat_long2 <- reactive(melt(datt(), id.vars ="rn" ))

  output$plot3 <- renderPlotly({
    plot_ly(dat_long2(), x= ~rn, y= ~value, type = 'scatter', mode = 'lines+markers', color = ~variable)  %>% layout(dragmode="box", showlegend = FALSE)
  })

}

shinyApp(ui, server)
