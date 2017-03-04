library(plotly)
library(htmlwidgets)
library(GGally)

dat <- mtcars[,c(3,4,7)]
dat[,3] = dat[,3]*8

p <- ggpairs(dat)

myMax = max(abs(dat))
myMin = min(abs(dat))
myRange = c(myMax, myMin)

p2 <- p
for(x in 2:p$nrow) {
  for(y in 1:(x-1)) {
    p2[x,y] <- p[x,y] +
      coord_cartesian(xlim = c(myRange), ylim = c(myRange))
  }
}

p3 <- ggplotly(p2)

p3 %>% onRender("function(el, x, data) {

                len = Math.sqrt(document.getElementsByClassName('cartesianlayer')[0].childNodes.length);

                // AxisNames stores the names of the 3 rows ('disp','hp','qsec')
                AxisNames = [];
                for (i = 1; i < (len+1); i++) {
                AxisNames.push(document.getElementsByClassName('infolayer')[0].childNodes[i].textContent);
                }

                el.on('plotly_click', function(e) {
                  data1 = data[Math.floor(Math.random() * 32) + 1];
                  data2 = data[Math.floor(Math.random() * 32) + 1];
                  var myData = [data1, data2];
                  console.log(myData);

                  var Traces = [];
                  var i=0;
                  var k=1;
                  while ((i*len+k)<=Math.pow((len-1),2)) {
                        var xArr = [];
                        for (a=0; a<myData.length; a++){
                          xArr.push(myData[a][AxisNames[i]])
                        }
                    while ((i+k)<len){
                        var yArr = [];
                        for (a=0; a<myData.length; a++){
                          yArr.push(myData[a][AxisNames[(len-k)]])
                        }

                      var trace = {
                        x: xArr,
                        y: yArr,
                        mode: 'markers',
                        marker: {
                          color: 'green',
                          size: 20
                        },
                        xaxis: 'x' + (i+1),
                        yaxis: 'y' + (i*len+k)
                      };
                      Traces.push(trace);
                      k++;
                    }
                    i++;
                    k=1;
                  }
                  Plotly.addTraces(el.id, Traces);
                }
                )}", data = dat)
