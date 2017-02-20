shinyUI(pageWithSidebar(
  headerPanel("Dynamic number of plots"),
  sidebarPanel(
    selectInput(inputId = "choosevar",
                label = "Choose Cut Variable:",
                choices = c("Nr. of Gears"="gear", "Nr. of Carburators"="carb"))
  ),
  mainPanel(
    # This is the dynamic UI for the plots
    uiOutput("plots")
  )
))
