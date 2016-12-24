library (reshape2)
library(ggplot2) #Actual plotting
library(hexbin)

set.seed(1) #Create data
bindata <- data.frame(x=rnorm(100), y=rnorm(100))
fac_probs <- dnorm(seq(-3, 3, length.out=26))
fac_probs <- fac_probs/sum(fac_probs)
bindata$factor <- sample(letters, 100, replace=TRUE, prob=fac_probs)
bindata$factor <- as.factor (bindata$factor)

# Calculate the basic hexagon grid
h <- hexbin (bindata, xbins = 5, IDs = TRUE,
             xbnds = range (bindata$x),
             ybnds = range (bindata$y))

# Calculate the counts depending on bindata$factor
counts <- hexTapply (h, bindata$factor, table)
counts <- t (simplify2array (counts))
counts <- melt (counts)
colnames (counts)  <- c ("ID", "factor", "counts")

# As we have the cell IDs, we can merge this data.frame with the proper coordinates
hexdf <- data.frame (hcell2xy (h),  ID = h@cell)
hexdf <- merge (counts, hexdf)

ggplot(hexdf, aes(x=x, y=y, fill = counts)) + geom_hex(stat="identity") + facet_wrap(~factor) + coord_equal () + scale_fill_continuous (low = "grey80", high = "#000040", na.value = "#00000000")

# This yields the correct bin sizes, but the figure has a bit weird appearance: 0 count hexagons are drawn, but only where some other facet has this bin populated. To suppres the drawing, we can set the counts there to NA and make the na.value completely transparent (it defaults to grey50)
hexdf$counts [hexdf$counts == 0] <- NA
ggplot(hexdf, aes(x=x, y=y, fill = counts)) + geom_hex(stat="identity") + facet_wrap(~factor) + coord_equal () + scale_fill_continuous (low = "grey80", high = "#000040", na.value = "#00000000")

# Can do ggplotly(p)
