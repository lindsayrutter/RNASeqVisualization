library(shiny)
library(plotly)
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)

ui <- fluidPage(
  fluidRow(
    column(8, plotlyOutput("plot")),
  # verbatimTextOutput("click"),
    column(4, plotlyOutput("plot2"))
  )
)
