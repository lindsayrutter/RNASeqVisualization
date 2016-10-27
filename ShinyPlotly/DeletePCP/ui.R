library(shiny)
library(plotly)
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)

ui <- fluidPage(
  plotlyOutput("plot")
  # verbatimTextOutput("click")
)
