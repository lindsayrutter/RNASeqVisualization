# ui.R

library("shiny")

shinyUI(
  basicPage(
    tags$head(
      tags$style(type='text/css',
                 "select, textarea, input[type='text'] {margin-bottom: 0px;}"
                 , "#submit {
                 color: rgb(255, 255, 255);
                 text-shadow: 0px -1px 0px rgba(0, 0, 0, 0.25);
                 background-color: rgb(189,54,47);
                 background-image: -moz-linear-gradient(center top , rgb(238,95,91), rgb(189,54,47));
                 background-repeat: repeat-x;
                 border-color: rgba(0, 0, 0, 0.1) rgba(0, 0, 0, 0.1) rgba(0, 0, 0, 0.25);
                 }"
      ),
      tags$script(HTML('
          Shiny.addCustomMessageHandler("jsCode",
            function(message) {
              eval(message.value);
            }
          );'
      ))
      )
    ,
    textInput(inputId = "inText", label = "", value = "type text here")
    ,
    actionButton(inputId = "submit", label = "Submit")
    #
    #   alternative approach: button with pop-up
    #    , tags$button("Activate", id = "ButtonID", type = "button", class = "btn action-button", onclick = "return confirm('Are you sure?');" )
    ,
    tags$br()
    ,
    tags$hr()
    ,
    uiOutput("outText")
)
)
