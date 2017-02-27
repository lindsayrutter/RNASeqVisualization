library(plotly)
library(data.table)
library(GGally)
library(hexbin)
library(htmlwidgets)

set.seed(1)
bindata <- data.frame(ID = paste0("ID",1:100), A=rnorm(100), B=rnorm(100), C=rnorm(100), D=rnorm(100), E=rnorm(100))
bindata$ID <- as.character(bindata$ID)
colNames = colnames(bindata[-1])

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

ggPS <- ggplotly(pS)

myLength <- length(ggPS[["x"]][["data"]])
for (i in 1:myLength){
  item =ggPS[["x"]][["data"]][[i]]$text[1]
  if (!is.null(item))
    if (!startsWith(item, "co")){
      ggPS[["x"]][["data"]][[i]]$hoverinfo <- "none"
    }
}

for(i in 2:(p$nrow)) {
  for(j in 1:(p$nrow-1)) {
    bindata[[paste(i,j,sep="-")]] <- attr(pS[i,j]$data, "cID")
  }
}

ggPS %>% onRender("
          function(el, x, data) {
          el = el;
          x=x;
          console.log(el)
          console.log(x)
          titleName = document.getElementsByClassName('g-xtitle')
          console.log(titleName[0].textContent)

          myLength = Math.sqrt(document.getElementsByClassName('cartesianlayer')[0].childNodes.length);
          console.log(myLength)

          el.on('plotly_click', function(e) {
            //console.log(e.points[0])
            xVar = (e.points[0].xaxis._id).replace(/[^0-9]/g,'')
            if (xVar.length == 0) xVar = 1
            yVar = (e.points[0].yaxis._id).replace(/[^0-9]/g,'')
            if (yVar.length == 0) yVar = 1
            myX = myLength + 1 - (yVar - myLength * (xVar - 1))
            myY = xVar

            cN = e.points[0].curveNumber
            split1 = (x.data[cN].text).split(' ')
            hexID = (x.data[cN].text).split(' ')[2]
            counts = split1[1].split('<')[0]

            selected_rowsA = [];
            selected_rowsE = [];
            //selected_rows = [];
            axisNames = data.colNames
            //console.log(axisNames)
            //ax0 = axisNames.slice(0, 5)
            //console.log(ax0)

            data.bindata.forEach(function(row){
            //if(row[myX+'-'+myY]==hexID) selected_rows.push([row.A, row.B]);
            //if(row[myX+'-'+myY]==hexID) selected_rows.push([row['A', 'B', 'C']]);
            //if(row[myX+'-'+myY]==hexID) selected_rows.push([row[[axisNames.slice(0, 5)]]]);
            //if(row[myX+'-'+myY]==hexID) selected_rows.push([row[ax0]]);
            if(row[myX+'-'+myY]==hexID){
              selected_rowsA.push(row.A);
              selected_rowsE.push(row.E);
              //selected_rows.push(row)
            }
            });
            console.log(selected_rowsA);
            console.log(selected_rowsE);
            //console.log(selected_rows);
            //console.log(selected_rows.A);

            var trace1 = {
                x: selected_rowsA,
                y: selected_rowsE,
                mode: 'markers',
                marker: {
                  color: 'orange',
                  size: 5
                }
            };

            Plotly.addTraces(el.id, trace1);
          })}
           ", data = list(bindata=bindata, colNames = colNames))
