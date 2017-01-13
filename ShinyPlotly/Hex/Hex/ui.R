library(shiny)
library(plotly)
library(data.table)

ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("click")
)
