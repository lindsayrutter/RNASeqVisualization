# setwd("~/RNASeqVisualization/ShinyPlotly/eechidna-master")
# library(shiny)
# runApp("App")

library(dplyr)
library(shiny)
library(plotly)
library(eechidna)

age = c("Age00_04", "Age05_14", "Age15_19", "Age20_24", "Age25_34","Age35_44", "Age45_54", "Age55_64", "Age65_74",  "Age75_84", "Age85plus")
religion = c("Christianity", "Catholic", "Buddhism", "Islam", "Judaism", "NoReligion")
other = c("Population", "MedianIncome", "Unemployed", "Bachelor", "Postgraduate", "BornOverseas", "Indigenous", "EnglishOnly", "OtherLanguageHome", "Married", "DeFacto", "FamilyRatio", "Internet", "NotOwned")
palette = c('#1B9E77', '#F0027F', '#E6AB02', '#66A61E', '#7570B3', '#D95F02', '#3690C0')

# a bit of data cleaning
nat_data_cart <- eechidna::nat_data_cart
nat_data_cart$Electorate <- nat_data_cart$ELECT_DIV
abs2011 <- eechidna::abs2011[c("ID", "Electorate", "State", age, religion, other)]
longAbs <- tidyr::gather(
  abs2011, variable, value, -ID, -Electorate, -State
)
longAbs$value <- as.numeric(longAbs$value)
longAbs <- longAbs[!is.na(longAbs$value),]
longAbs$variable <- factor(
  longAbs$variable,
  levels = unique(longAbs$variable)
)
isAge <- grepl("^Age", longAbs$variable)
ageDat <- longAbs[isAge, ]
isReg <- longAbs$variable %in% religion
religionDat <- longAbs[isReg, ]
otherDat <- longAbs[!isAge & !isReg, ]

# 1st preference votes for candidates for the House for each electorate
aec13 <- as.data.frame(eechidna::aec2013_fp_electorate)

# by default, we show parties that won at least 1 electorate
relevantParties <- aec13 %>%
  group_by(PartyAb) %>%
  summarise(n = sum(ifelse(Elected == "Y", 1, 0))) %>%
  filter(n > 0)

# proportion of first preference votes for each party by electorate
voteProps <- aec13 %>%
  group_by(Electorate, PartyAb) %>%
  summarise(n = sum(Total_OrdinaryVotes_in_electorate)) %>%
  mutate(prop = n / sum(n))

# create a sensible ranking for PartyAb
m <- voteProps %>%
  group_by(PartyAb) %>%
  summarise(m = mean(prop)) %>%
  arrange(m)

lvls <- as.data.frame(m)$PartyAb
aec13$PartyAb <- factor(aec13$PartyAb, levels = lvls)
voteProps$PartyAb <- factor(voteProps$PartyAb, levels = lvls)

# 2 party preferred data
aec13pp <- eechidna::aec2013_2cp_electorate %>%
  mutate(FullName = paste(GivenNm, Surname)) %>%
  group_by(Electorate) %>%
  summarise(
    difference = abs(diff(TotalVotes) / sum(TotalVotes)),
    parties = paste(PartyAb[order(TotalVotes, decreasing = TRUE)], collapse = " over "),
    candidates = paste(FullName[order(TotalVotes, decreasing = TRUE)], collapse = " over ")
  ) %>%
  arrange(difference) %>%
  mutate(Electorate = factor(Electorate, levels = Electorate)) %>%
  mutate(tooltip = paste0(Electorate, "<br />", parties, "<br />", candidates))

# there are multiple brushes in the UI, but they have common properties
brush_opts <- function(id, ...) {
  brushOpts(id = id, direction = "x", resetOnNew = TRUE, ...)
}

ui <- fluidPage(
  shinyjs::useShinyjs(),
  fluidRow(
    column(
      width = 1,
      checkboxInput("show", "Show Controls")
    ),
    column(
      width = 1,
      actionButton("clear", "Clear Selections")
    )
  ),
  conditionalPanel(
    "input.show",
    fluidRow(
      column(
        width = 2,
        checkboxInput("persist", "Persistant selections?", FALSE),
        shinyjs::colourInput("color", "Selection color:", palette = "limited", allowedCols = palette)
      ),
      column(
        width = 6,
        selectizeInput(
          "parties", "Select parties:", unique(eechidna::aec2013_fp$PartyAb),
          selected = relevantParties$PartyAb,
          multiple = TRUE
        )
      )
    )
  ),
  fluidRow(
    column(
      width = 3,
      plotlyOutput("pp")
    ),
    column(
      width = 3,
      plotlyOutput("voteProps")
    )
  )
)


server <- function(input, output) {

  # initiate selection data and *input brushes* as reactive values so we can
  # "clear the world" - http://stackoverflow.com/questions/30588472/is-it-possible-to-clear-the-brushed-area-of-a-plot-in-shiny/36927826#36927826
  # Creates data frame (150 observations) called data as follows:
  #Electorate  fill
  #1          Lingiari black
  #2           Solomon black
  #3         Fremantle black
  #4          Wide Bay black
  #5            Wright black
  rv <- reactiveValues(
    data = data.frame(
      Electorate = nat_data_cart$Electorate,
      fill = factor(rep("gray", nrow(nat_data_cart)), levels = c("gray", palette)),
      stringsAsFactors = FALSE
    )
  )

  # clear brush values and remove the div from the page
  observeEvent(input$clear, {
    rv$data$fill <- "gray"
  })

  # reusable function for "telling the world" about the selection
  # it should modify the reactive value _once_ since shiny will send messages
  # on every modification
  updateRV <- function(selected) {
    print(input$color)
    if (input$persist) {
      rv$data$fill[selected] <- input$color
    } else {
      fill <- rv$data$fill
      fill[rv$data$fill %in% input$color] <- "gray"
      print(input$color)
      fill[selected] <- input$color
      rv$data$fill <- fill
    }
  }

  observeEvent(event_data("plotly_selected"), {
    selected <- rv$data$Electorate %in% event_data("plotly_selected")$key
    updateRV(selected)
  })

  output$voteProps <- renderPlotly({
    voteProps <- voteProps[voteProps$PartyAb %in% input$parties, ]
    dat <- dplyr::left_join(voteProps, rv$data, by = "Electorate")
    p <- ggplot(dat, aes(x = PartyAb, y = prop, colour = fill,
                         key = Electorate, text = Electorate)) +
      #geom_jitter(width = 0.25, alpha = 0.5) +
      geom_line(aes(group = Electorate), alpha = 0.2) +
      geom_point(alpha = 0.5, size = 0.001) +
      scale_colour_identity() + theme_bw() +
      theme(legend.position = "none") + coord_flip() +
      xlab(NULL) + ylab("Proportion of 1st preferences")
    ggplotly(p, tooltip = "text") %>% layout(dragmode = "select")
  })

  output$pp <- renderPlotly({
    dat <- dplyr::left_join(aec13pp, rv$data, by = "Electorate")
    dat$Electorate <- factor(dat$Electorate, levels = levels(aec13pp$Electorate))
    p <- ggplot(dat, aes(difference, Electorate, colour = fill,
                    key = Electorate, text = tooltip)) +
      scale_colour_identity() + theme_bw() +
      theme(legend.position = "none") +
      geom_point(alpha = 0.5) + ylab(NULL) +
      xlab("Absolute difference in vote totals") +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank())
    ggplotly(p, tooltip = "text") %>% layout(dragmode = "select")
  })

}

shinyApp(ui, server)
