library(reshape2)
library(ggplot2) #Actual plotting
library(hexbin)

set.seed(1) #Create data
bindata <- data.frame(x=rnorm(100), y=rnorm(100))
fac_probs <- dnorm(seq(-3, 3, length.out=26))
fac_probs <- fac_probs/sum(fac_probs)
bindata$factor <- sample(letters, 100, replace=TRUE, prob=fac_probs)
bindata$factor <- as.factor (bindata$factor)

# Calculate the basic hexagon grid
# Change xbins to larger number for finer resolution
h <- hexbin (bindata, xbins = 5, IDs = TRUE, xbnds = range (bindata$x), ybnds = range (bindata$y))

# Calculate the counts depending on bindata$factor
counts <- hexTapply (h, bindata$factor, table)
counts <- t (simplify2array (counts))
counts <- melt (counts)
colnames (counts)  <- c ("ID", "factor", "counts")

# As we have the cell IDs, we can merge this data.frame with the proper coordinates
hexdf <- data.frame (hcell2xy (h),  ID = h@cell)
hexdf <- merge (counts, hexdf)

p <- ggplot(hexdf, aes(x=x, y=y, fill = counts)) + geom_hex(stat="identity") + facet_wrap(~factor) + coord_equal () + scale_fill_continuous (low = "grey80", high = "#000040", na.value = "#00000000")

ggplotly(p)

# This yields the correct bin sizes, but the figure has a bit weird appearance: 0 count hexagons are drawn, but only where some other facet has this bin populated. To suppres the drawing, we can set the counts there to NA and make the na.value completely transparent (it defaults to grey50)
hexdf$counts [hexdf$counts == 0] <- NA
ggplot(hexdf, aes(x=x, y=y, fill = counts)) + geom_hex(stat="identity") + facet_wrap(~factor) + coord_equal () + scale_fill_continuous (low = "grey80", high = "#000040", na.value = "#00000000")


###########################################################

# Can do ggplotly(p)

# This strategy works as long as the binwidths are correct without facetting. If the binwidths are set very small, the resolution may still yield too large dx and dy. In that case, we can supply hexGrob with two adjacent bins (but differing in both x and y) with NA counts for each facet.
dummy <- hgridcent (xbins = 5, xbnds = range (bindata$x), ybnds = range (bindata$y), shape = 1)

dummy <- data.frame (ID = 0, factor = rep (levels (bindata$factor), each = 2), counts = NA, x = rep (dummy$x [1] + c (0, dummy$dx/2), nlevels (bindata$factor)), y = rep (dummy$y [1] + c (0, dummy$dy), nlevels (bindata$factor)))

# An additional advantage of this approach is that we can delete all the rows with 0 counts already in counts, in this case reducing the size of hexdf by roughly 3/4 (122 rows instead of 520). The plot looks exactly the same as above, but you can visualize the difference with na.value not being fully transparent.
counts <- counts [counts$counts > 0 ,]
hexdf <- data.frame (hcell2xy (h),  ID = h@cell)
hexdf <- merge (counts, hexdf)
hexdf <- rbind (hexdf, dummy)

#######################################################
library(lattice)
library(hexbin)
p <- hexbinplot(y ~ x | factor, bindata, xbnds = "panel", ybnds = "panel", xbins=5, layout=c(7,3))
ggplotly(p) # doesn't work (ggplotly can't work for trellis)

#######################################################
p <- ggplot(bindata, aes(x=x, y=y, group=factor)) + facet_wrap(~factor) + stat_bin2d(binwidth=c(0.6, 0.6))
ggplotly(p)

#######################################################
set.seed(100)
d <- diamonds[sample(nrow(diamonds), 1000), ]

p <- ggplot(data = d, aes(x = carat, y = price)) + 
  geom_point(size = 1) +
  facet_wrap(~ cut)
ggplotly(p)

p <- ggplot(hexdf, aes(x=x, y=y, fill = counts)) +
  geom_hex(stat="identity") +
  facet_wrap(~factor)

(gg <- ggplotly(p))


