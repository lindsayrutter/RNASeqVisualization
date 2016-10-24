library(shiny)
library(plotly)
library(data.table)
library(dplyr)
library(tidyr)

library(rtracklayer)
library(Rsamtools)
library(grid)
library(GenomicAlignments)
library(ggplot2)
#library(GGally) #
library(edgeR)
library(stringr)
library(EDASeq)
library(matrixStats)
library(gridExtra)
library(reshape2)
library(scales)
library(gtools)

ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("click"),
  plotlyOutput("plot2")
)
