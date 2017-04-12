library(shiny)
library(shinyBS)

runApp(
  # Ui
  list(ui = pageWithSidebar(
    headerPanel("Test App"),

    sidebarPanel(
      bsButton("renderButton", "Render"),
      actionLink("link", "Help") ),

    mainPanel("Hello World!")
  ),

  # Server
  server = function(input, output, session) {

    # ... dealing with renderButton ...
    addPopover(session=session, id="link", title="",
               content="Testing.", placement = "bottom",
               trigger = "click", options = NULL)

  })
)
