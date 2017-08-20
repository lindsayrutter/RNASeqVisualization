d <- ggplot(diamonds[1:100,], aes(carat, price))
d2 <- d + geom_hex()
myDat <- ggplot_build(d2)

myDat$data[[1]]

xVal <- sort(unique(myDat$data[[1]]$x))
yVal <- sort(unique(myDat$data[[1]]$y))

h <- xVal[2]-xVal[1]
v <- yVal[2]-yVal[1]

##########################################################

stat_binhex(mapping = aes(x=longitude, y=latitude),
            data = origin_latlng,
            color = 'white',
            alpha=0.75,
            geom = "hex",
            position = "identity",
            bins = 25,
            na.rm=TRUE)
stat_binhex(mapping = aes(x=longitude, y=latitude, label = ..count.., fill = {print(sum(..count..))}),
            data = origin_latlng,
            color = 'white',
            alpha=0.75,
            geom = "text",
            size = 3,
            position = "identity",
            hjust = 0.5,
            vjust = -0.3,
            bins = 25,
            na.rm=TRUE)

