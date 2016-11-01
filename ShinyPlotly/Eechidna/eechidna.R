# development version of plotly requires developmental version of scales >= 0.4.0.9003
devtools::install_github("hadley/scales")
library(scales)

# eechidna requires development version of plotly >= 4.5.5
devtools::install_github("ropensci/plotly", force = TRUE)

library(plotly)

devtools::install_github("ropenscilabs/eechidna")

library(eechidna)

##################################################################

library(eechidna)
library(plyr)
library(dplyr)

glimpse(abs2011)

ggplot(data = abs2011,
       aes(x = reorder(State, -Age00_04),
           y = Age00_04,
           colour = State)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "State",
       y = "% Aged between 0 and 4") +
  theme(legend.position = "none") +
  coord_flip()

ggplot(data = abs2011,
       aes(x = Postgraduate,
           y = MedianIncome)) +
  geom_point(colour = "steelblue",
             alpha = 0.75)

##################################################################
