library(shiny)
library(plotly)
library(data.table)
library(dplyr)
library(bsplus)

# ui <- shinyUI(pageWithSidebar(
#   headerPanel("Dynamic number of plots"),
#
#   sidebarPanel(
#     plotlyOutput("plot")
#   ),
#
#   mainPanel(
#     # This is the dynamic UI for the plots
#     bs_carousel(id = "tabPrev", use_indicators = TRUE) %>%
#       bs_append(content = uiOutput("plots"))
#     )
#   )
# )

ui <- shinyUI(fluidPage(
  fluidRow(
    column(8,
           bs_carousel(id = "tabPrev", use_indicators = TRUE) %>%
           bs_append(content = uiOutput("plots"))),
    column(4,
           plotlyOutput("plot")
    ))))

server <- shinyServer(function(input, output) {

  set.seed(1)
  dat <- data.frame(ID = paste0("ID",1:10), A.1 = runif(10), A.2 = runif(10), A.3 = runif(10), B.1 = runif(10), B.2 = runif(10), B.3 = runif(10))
  dat$ID <- as.character(dat$ID)

  # Convert DF from scatterplot to PCP
  datt <- data.frame(t(dat))
  names(datt) <- as.matrix(datt[1, ])
  datt <- datt[-1, ]
  datt[] <- lapply(datt, function(x) type.convert(as.character(x)))
  setDT(datt, keep.rownames = TRUE)[]
  colnames(datt)[1] <- "x"
  dat_long <- melt(datt, id.vars ="x" )
  dat_long <- separate(dat_long, x, c("group", "rep"), remove=FALSE)
  dat_long$group <- factor(dat_long$group)

  output$plot <- renderPlotly({
    plot_ly(dat_long, x= ~x, y= ~value, type = 'scatter', mode = 'lines+markers', color = ~variable) %>% layout(dragmode="box", showlegend = FALSE)
  })

  d <- reactive(event_data("plotly_selected"))

  observeEvent(d(),{
  # Insert the right number of plot output objects into the web page
    output$plots <- renderUI({

    lengthY <- reactive((length(unique(d()$curveNumber))))
    if (lengthY()<1){
      plot_output_list <- list()
    }
    else{
      plot_output_list <- lapply(1:lengthY(), function(i) {
        plotname <- paste("plot", i, sep="")
        plotlyOutput(plotname, height = 280, width = 250)
      })
    }

    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list)
  })
  })

  # Call renderPlot for each one. Plots are only actually generated when they
  # are visible on the web page.
  observeEvent(d(),{
    lengthY <- reactive(length(unique(d()$curveNumber)))
    for (i in 1:lengthY()) {
      # Need local so that each item gets its own number. Without it, the value
      # of i in the renderPlot() will be the same across all instances, because
      # of when the expression is evaluated.
      local({
        my_i <- i
        curveY <- reactive(d()$curveNumber[my_i])
        plotname <- paste("plot", my_i, sep="")

        ax <- list(title = "", showticklabels = TRUE)
        ay <- list(title = "Read Count")
        indDat <- as.data.frame(dat_long[variable %in% dat[curveY()+1,]$ID])
        g1 <- levels(indDat$group)[1]
        g2 <- levels(indDat$group)[2]
        g1m <- mean(filter(indDat, group==g1)$value)
        g2m <- mean(filter(indDat, group==g2)$value)

        output[[plotname]] <- renderPlotly({
          indDat %>% plot_ly(x = ~group, y = ~value, type = "scatter", marker = list(size = 10), color = ~group, colors = "Set2", hoverinfo = "text", text = paste0("Read count = ", format(round(indDat$value, 2), nsmall = 2))) %>% layout(xaxis = ax, yaxis = ay, legend = list(x = 0.35, y = -0.26)) %>% add_segments(x = g1, xend = g2, y = g1m, yend = g2m, showlegend = FALSE, line = list(color='#000000')) %>% add_trace(x = g1, y= g1m, showlegend = FALSE, hoverinfo = "text", text = paste0("Mean Read Count = ", round(g1m, digits = 2)), marker = list(color='#000000')) %>% add_trace(x = g2, y= g2m, showlegend = FALSE, hoverinfo = "text", text = paste0("Mean Read Count = ", round(g2m, digits = 2)), marker = list(color='#000000'))
        })
      })
    }
  })
})

shinyApp(ui, server)
