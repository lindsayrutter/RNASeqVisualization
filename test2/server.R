# server.R

library("shiny")

shinyServer(
  function(session, input, output) {

    observe({
      if (is.null(input$submit) || input$submit == 0){return()}
      js_string <- 'alert("Do you want to submit now?");'
      session$sendCustomMessage(type='jsCode', list(value = js_string))
      text <- isolate(input$inText)
      output$outText <- renderUI({
        h4(text)
      })
    })

  }
)
