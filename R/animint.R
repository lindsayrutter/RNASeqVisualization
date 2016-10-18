library("ggplot2")

library(devtools)
install_github("tdhock/animint")
library("animint")

data(tips, package = "reshape2")
tips$sex_smoker <- with(tips, interaction(sex, smoker))
p <- ggplot() +
  geom_point(data = tips,
             aes(x = total_bill, y = tip, colour = sex_smoker))
p

