# First solve missing hexagons, then solve adding points
library(hexbin)
library(ggplot2)

set.seed(1)
data <- data.frame(A=rnorm(100), B=rnorm(100), C=rnorm(100), D=rnorm(100), E=rnorm(100))
maxVal = max(abs(data))
maxRange = c(-1*maxVal, maxVal)

x = data[,c("A")]
y = data[,c("E")]
h <- hexbin(x=x, y=y, xbins=5, shape=1, IDs=TRUE, xbnds=maxRange, ybnds=maxRange)
hexdf <- data.frame (hcell2xy (h),  hexID = h@cell, counts = h@count)

p <- ggplot(hexdf, aes(x=x, y=y, fill = counts, hexID=hexID)) + geom_hex(stat="identity") + coord_cartesian(xlim = c(maxRange[1], maxRange[2]), ylim = c(maxRange[1], maxRange[2]))

dat <- data[c(1:5),]

p + geom_point(data = dat, aes(x=A, y=B))
p + geom_point() + geom_point(dat, aes(A, B))

#############################################################################
# Solution 1
p + geom_point(data = dat, aes(x=A, y=B), inherit.aes = FALSE)

# Solution 2
p <- ggplot() +
  geom_hex(data = hexdf, aes(x=x, y=y, fill = counts), stat="identity") +
  coord_cartesian(xlim = c(maxRange[1], maxRange[2]), ylim = c(maxRange[1], maxRange[2]))

p + geom_point(data = dat, aes(x = A, y = B))

