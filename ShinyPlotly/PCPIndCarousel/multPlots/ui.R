shinyUI(pageWithSidebar(

  headerPanel("Dynamic number of plots"),

  sidebarPanel(
    sliderInput("n", "Number of plots", value=0, min=0, max=5)
  ),

  mainPanel(
    # This is the dynamic UI for the plots
    uiOutput("plots")
  )
))
