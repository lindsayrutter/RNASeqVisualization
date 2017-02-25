library(plotly)
library(data.table)
library(GGally)
library(hexbin)

cnToID <- function(h){
  df <- data.frame(table(h)) #h@cID
  colnames(df) <- c("hexID","count")
  cnID <- df[order(df$count,as.character(df$hexID)),]
  cnID$curveNumber <- seq(1, nrow(cnID))
  return(cnID)
}

set.seed(1)
bindata <- data.frame(ID = paste0("ID",1:100), A=rnorm(100), B=rnorm(100), C=rnorm(100), D=rnorm(100), E=rnorm(100))
bindata$ID <- as.character(bindata$ID)

maxVal = max(abs(bindata[,2:6]))
maxRange = c(-1*maxVal, maxVal)

my_fn <- function(data, mapping, ...){
  x = data[,c(as.character(mapping$x))]
  y = data[,c(as.character(mapping$y))]
  h <- hexbin(x=x, y=y, xbins=5, shape=1, IDs=TRUE, xbnds=maxRange, ybnds=maxRange)
  hexdf <- data.frame (hcell2xy (h),  hexID = h@cell, counts = h@count)
  attr(hexdf, "cID") <- h@cID
  p <- ggplot(hexdf, aes(x=x, y=y, fill = counts, hexID=hexID)) + geom_hex(stat="identity")
  p
}

p <- ggpairs(bindata[,2:6], lower = list(continuous = my_fn))
pS <- p
for(i in 2:p$nrow) {
  for(j in 1:(i-1)) {
    pS[i,j] <- p[i,j] +
      coord_cartesian(xlim = c(maxRange[1], maxRange[2]), ylim = c(maxRange[1], maxRange[2]))
  }
}

cnToPlot = data.frame()
cN=1
i=2
n=ncol(bindata)-1
while (i<=n){
  ki=i
  kj=i-1
  while (ki<=n){
    myLength <- length(table(attr(pS[ki,kj]$data, "cID")))
    cnToPlot = rbind(cnToPlot, cbind(ki = rep(ki, myLength), kj = rep(kj, myLength), curveNumber = cN:(cN+myLength-1)))
    ki=ki+1
    cN=cN+myLength+1
  }
  cN=cN+i
  i=i+1
}

ggPS <- ggplotly(pS)

myLength <- length(ggPS[["x"]][["data"]])
for (i in 1:myLength){
  item =ggPS[["x"]][["data"]][[i]]$text[1]
  if (!is.null(item))
    if (!startsWith(item, "co")){
      ggPS[["x"]][["data"]][[i]]$hoverinfo <- "none"
    }
}

cN <- 157
cnP <- cnToPlot[which(cnToPlot$curveNumber==cN),]
cnH <- cnToID(attr(pS[cnP$ki,cnP$kj]$data, "cID"))
cnHex <- cbind(cnH[,c(1,2)], curveNumber = cnToPlot[intersect(which(cnToPlot$ki==cnP$ki), which(cnToPlot$kj==cnP$kj)),]$curveNumber)
hexVal <- as.numeric(as.character(cnHex[which(cnHex$curveNumber==cN),]$hexID))
obsns <- which(attr(pS[cnP$ki,cnP$kj]$data, "cID")==hexVal)
dat <- bindata[obsns,]

ggPS %>% onRender("
          function(el, x, data) {
          el = el;
          x=x;
          var data = data[0];
          console.log(el)
          console.log(x)
          console.log(data)

          console.log(document.getElementsByClassName('cartesianlayer')[0].childNodes.length);

          el.on('plotly_click', function(e) {
            console.log(e.points[0])
            xVar = (e.points[0].xaxis._id).replace(/[^0-9]/g,'')
            if (xVar.length == 0) xVar = 1
            console.log(xVar)

            yVar = (e.points[0].yaxis._id).replace(/[^0-9]/g,'')
            if (yVar.length == 0) yVar = 1
            console.log(yVar)

            cN = e.points[0].curveNumber
            split1 = (x.data[cN].text).split(' ')
            hexID = (x.data[cN].text).split(' ')[2]
            counts = split1[1].split('<')[0]
            console.log(cN)
            console.log(hexID)
            console.log(counts)
          })}
           ", data = pS[3,2]$data)


