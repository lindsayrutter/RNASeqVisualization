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

# Both objects below indicate there are 17 hexagons
# hexdf
# table(h@cID)

# However, plotting only shows 16 hexagons
ggplot(hexdf, aes(x=x, y=y, fill = counts, hexID=hexID)) + geom_hex(stat="identity") + scale_x_continuous(limits = maxRange) + scale_y_continuous(limits = maxRange)
