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

x = bindata[,c("A")]
y = bindata[,c("B")]
h <- hexbin(x=x, y=y, xbins=5, shape=1, IDs=TRUE, xbnds=maxRange, ybnds=maxRange)
hexdf <- data.frame (hcell2xy (h),  hexID = h@cell, counts = h@count)
attr(hexdf, "cID") <- h@cID
p <- ggplot(hexdf, aes(x=x, y=y, fill = counts, hexID=hexID)) + geom_hex(stat="identity")
p

ggPS <- ggplotly(p)

ggPS %>% onRender("
           function(el, x) {
           console.log(el)
           var myGraph = document.getElementById(el.id);
           el.on('plotly_click', function(e) {
           var pts = '';
           console.log(e)
           console.log(e.points[0].curveNumber)
           for(var i=0; i < e.points.length; i++){
           var annotate_text = 'x = '+e.points[i].x +
           'y = '+e.points[i].y.toPrecision(4);

           var annotation = {
           text: annotate_text,
           x: e.points[i].x,
           y: parseFloat(e.points[i].y.toPrecision(4))
           }
           console.log(annotation)
           console.log(el.layout.annotations)
           annotations = el.layout.annotations || [];
           annotations.push(annotation);
           Plotly.relayout(el.id,{annotations: annotations})
           }
           })}
           ")

