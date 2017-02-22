library(plotly)
library(data.table)
library(GGally)
library(hexbin)
library(htmlwidgets)

set.seed(1)
bindata <- data.frame(ID = paste0("ID",1:100), A=rnorm(100), B=rnorm(100))
bindata$ID <- as.character(bindata$ID)

x = bindata[,c("A")]
y = bindata[,c("B")]
h <- hexbin(x=x, y=y, xbins=5, shape=1, IDs=TRUE)
hexdf <- data.frame (hcell2xy (h),  hexID = h@cell, counts = h@count)
attr(hexdf, "cID") <- h@cID
pS <- ggplot(hexdf, aes(x=x, y=y, fill = counts, hexID=hexID)) + geom_hex(stat="identity")

ggPS <- ggplotly(pS)

myLength <- length(ggPS[["x"]][["data"]])
for (i in 1:myLength){
  item =ggPS[["x"]][["data"]][[i]]$text[1]
  if (!is.null(item))
    if (!startsWith(item, "co")){
      ggPS[["x"]][["data"]][[i]]$hoverinfo <- "none"
    }
}

ggPS %>% onRender("
          function(el, x, data) {
            //console.log(el)
            //console.log(x)
            //console.log(data)

            myGraph = document.getElementById(el.id);
            el.on('plotly_click', function(e) {

            cN = e.points[0].curveNumber
            split1 = (x.data[cN].text).split(' ')
            hexID = (x.data[cN].text).split(' ')[2]
            counts = split1[1].split('<')[0]
            console.log(hexID)
            console.log(counts)

           })}
           ", data = pS$data)

# I know how to access values from outside onRender
hexID=40
obsns <- which(attr(pS$data, "cID")==hexID)
dat <- bindata[obsns,]
