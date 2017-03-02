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

                // Number of rows in data frame is myLength=3
                myLength = Math.sqrt(document.getElementsByClassName('cartesianlayer')[0].childNodes.length);

                // AxisNames stores the names of the 3 rows ('disp','hp','qsec')
                AxisNames = [];
                for (i = 1; i < (myLength+1); i++) {
                AxisNames.push(document.getElementsByClassName('infolayer')[0].childNodes[i].textContent);
                }

                el.on('plotly_click', function(e) {
                data1 = data[Math.floor(Math.random() * 32) + 1];
                data2 = data[Math.floor(Math.random() * 32) + 1];
                var myData = [data1, data2];

                var trace1 = {
                  x: [myData[0]['disp'], myData[1]['disp']],
                  y: [myData[0]['qsec'], myData[0]['qsec']],
                  mode: 'markers',
                  marker: {
                    color: 'green',
                    size: 20
                  },
                  xaxis: 'x',
                  yaxis: 'y'
                };

                var trace2 = {
                  x: [myData[0]['disp'], myData[1]['disp']],
                  y: [myData[0]['hp'], myData[0]['hp']],
                  mode: 'markers',
                  marker: {
                    color: 'green',
                    size: 20
                  },
                  xaxis: 'x',
                  yaxis: 'y2'
                };

                var trace3 = {
                  x: [myData[0]['hp'], myData[0]['hp']],
                  y: [myData[0]['qsec'], myData[0]['qsec']],
                  mode: 'markers',
                  marker: {
                    color: 'green',
                    size: 20
                  },
                  xaxis: 'x2',
                  yaxis: 'y4'
                };

                Plotly.addTraces(el.id, [trace1,trace2,trace3]);
                }
                )}", data = dat)
