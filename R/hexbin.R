library(hexbin)

x <- rnorm(20000)
y <- rnorm(20000)
hbin <- hexbin(x,y, xbins = 40)
plot(hbin)

########################  Need to redo this part.
library("marray")
data(swirl, package = "marray") ## use swirl dataset
hb1 <- hexbin(maA(swirl[,1]), maM(swirl[,1]), xbins = 40)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 1))
nb <- plot(hb1, type = 'n', xlab = 'A', ylab = 'M', main = "M vs A plot with points", legend = 0, newpage = FALSE)
pushHexport(nb$plot.vp)
grid.points(maA(swirl[,1]), maM(swirl[,1]),pch = 16,gp = gpar(cex = .4))
popViewport()
nb$hbin <- hb1
hexVP.abline(nb$plot.vp,h = 0,col = gray(.6))
hexMA.loess(nb)
popViewport()
pushViewport(viewport(layout.pos.col = 2,layout.pos.row = 1))
hb <- plotMAhex(swirl[,1], newpage = FALSE, main = "M vs A plot with hexagons", legend = 0)
hexVP.abline(hb$plot.vp,h = 0,col = gray(.6))
hexMA.loess(hb)
popViewport()

#################################################################
set.seed(1) #Create data
bindata <- data.frame(x=rnorm(100), y=rnorm(100))
fac_probs <- dnorm(seq(-3, 3, length.out=26))
fac_probs <- fac_probs/sum(fac_probs)
bindata$factor <- sample(letters, 100, replace=TRUE, prob=fac_probs)

library(ggplot2) #Actual plotting
library(hexbin)
library(plotly)

p <- ggplot(bindata, aes(x=x, y=y)) + geom_hex()
# ggplot(bindata, aes(x=x, y=y)) + geom_hex() + facet_wrap(~factor)
ggplotly(p)



