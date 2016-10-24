library(shiny)
library(plotly)
library(data.table)
library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(GGally)
library(edgeR)

ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("click")
  #plotlyOutput("plot2")
)
