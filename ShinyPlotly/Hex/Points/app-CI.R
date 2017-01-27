library(ggplot2)

set.seed(1)
x = runif(20,0,3)
y = runif(20,0,2)

ciVal <- 0.5
myMax = max(c(x,y))
myMin = min(c(x,y))
keep <- abs(x - y) >= ciVal

data <- data.frame(dp = paste0("DataPoint",1:sum(keep==TRUE)), x = x[keep], y = y[keep])
data$dp <- as.character(data$dp)
data2 <- data.frame(x = seq(myMin, myMax, 0.01), y = seq(myMin, myMax, 0.01))

ggplot(data = data, aes(x=x,y=y)) +
  geom_point(size=2) +
  geom_ribbon(data=data2, aes(x=x, ymin = y-ciVal, ymax = y+ciVal), fill = "lightgrey") +
  geom_abline(intercept = 0, color = "white", size = 0.25) +
  #geom_abline(intercept = ciVal, color ="blue", size = 0.25) +
  #geom_abline(intercept = -1*ciVal, color ="blue", size = 0.25) +
  scale_x_continuous(limits = c(myMin, myMax)) +
  scale_y_continuous(limits = c(myMin, myMax)) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))
