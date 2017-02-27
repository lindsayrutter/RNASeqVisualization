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

    console.log(el)
    // Number of rows is myLength=3
    myLength = Math.sqrt(document.getElementsByClassName('cartesianlayer')[0].childNodes.length);

    // AxisNames stores the names of the 3 rows ('disp','hp','qsec')
    AxisNames = [];
    for (i = 1; i < (myLength+1); i++) {
      AxisNames.push(document.getElementsByClassName('infolayer')[0].childNodes[i].textContent);
    }
    console.log(AxisNames)


    el.on('plotly_click', function(e) {
      data1 = data[Math.floor(Math.random() * 32) + 1];
      data2 = data[Math.floor(Math.random() * 32) + 1];

      var myData = [data1, data2];
      console.log(myData)

      var allData = [];

      for (i = 0; i < myData.length; i++){
        for (j = 0 ; j < myLength; j++){
          allData.push(myData[i][AxisNames[j]])
        }
      }
      console.log(myData[0][AxisNames[0]])
      console.log(allData)

      //result = data1.axisData( AxisNames );
      //console.log(result)

      var trace1 = {
      x: [e.points[0].x],
      y: [e.points[0].y],
      mode: 'markers',
      marker: {
      color: 'green',
      size: 20
      }
      };
      Plotly.addTraces(el.id, trace1);
      }
      )}", data = dat)
