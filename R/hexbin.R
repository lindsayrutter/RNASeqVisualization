library(hexbin)

x <- rnorm(20000)
y <- rnorm(20000)
hbin <- hexbin(x,y, xbins = 40)
plot(hbin)

