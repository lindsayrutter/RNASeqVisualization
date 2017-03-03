library(plotly)
library(htmlwidgets)
library(GGally)

set.seed(1)
dat <- mtcars[,c(1,3,4,7)]
dat[,1] = dat[,1]*10
dat[,4] = rnorm(32,250,70)
dat$qsec2 = dat$qsec+10

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

                  var Traces = [];
                  var i=0;
                  var k=1;
                  while ((i*len+k)<=Math.pow((len-1),2)) {
                    while ((i+k)<len){
                      var trace = {
                        x: [myData[0][AxisNames[i]], myData[1][AxisNames[i]]],
                        y: [myData[0][AxisNames[(len-k)]], myData[1][AxisNames[(len-k)]]],
                        mode: 'markers',
                        marker: {
                          color: 'green',
                          size: 10
                        },
                        xaxis: 'x' + (i+1),
                        yaxis: 'y' + (i*len+k)
                      };
                      console.log('added Trace');
                      Traces.push(trace);
                      console.log(Traces);
                      k++;
                    }
                    i++;
                    k=1;
                  }
                  console.log('Print final traces');
                  console.log(Traces);
                  Plotly.addTraces(el.id, Traces);
                }
                )}", data = dat)
