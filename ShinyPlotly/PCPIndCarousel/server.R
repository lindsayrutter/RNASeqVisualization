library(googleVis)
shinyServer(function(input, output) {
  #dynamically create the right number of htmlOutput
  output$plots <- renderUI({
    plot_output_list <- lapply(unique(mtcars[,input$choosevar]), function(i) {
      plotname <- paste0("plot", i)
      htmlOutput(plotname)
    })

    tagList(plot_output_list)
  })

  # Call renderPlot for each one. Plots are only actually generated when they
  # are visible on the web page.
  for (i in 1:max(unique(mtcars[,"gear"]),unique(mtcars[,"carb"]))) {
    local({
      my_i <- i
      plotname <- paste0("plot", my_i)

      output[[plotname]] <- renderGvis({
        data <- mtcars[mtcars[,input$choosevar]==my_i,]
        if(dim(data)[1]>0){
          gvisColumnChart(
            data, xvar='hp', yvar='mpg'
          )}
        else NULL
      })
    })
  }

})
