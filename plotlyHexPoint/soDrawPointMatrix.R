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

    // Number of rows is myLength=3
    myLength = Math.sqrt(document.getElementsByClassName('cartesianlayer')[0].childNodes.length);

    // AxisNames stores the names of the 3 rows ('disp','hp','qsec')
    AxisNames = [];
    for (i = 1; i < (myLength+1); i++) {
      AxisNames.push(document.getElementsByClassName('infolayer')[0].childNodes[i].textContent);
    }

    el.on('plotly_click', function(e) {
      // Grab two random rows of the 32 rows from mtcars dataset and store in myData. In my real code (not this MWE), myData represents an array of 1 or more objects, where each object contains a value for each column in the dataset - here there are three columns - disp, hp, and qsec.
      data1 = data[Math.floor(Math.random() * 32) + 1];
      data2 = data[Math.floor(Math.random() * 32) + 1];
      var myData = [data1, data2];

      //May not be necessary, but this creates one array allData that contains all column values for all randomly selected rows. Since this example has 3 columns (disp, hp, and qsec) and two randomly selected rows, allData has a length of 6.
      var allData = [];
      for (i = 0; i < myData.length; i++){
        for (j = 0 ; j < myLength; j++){
          allData.push(myData[i][AxisNames[j]])
        }
      }
      console.log(allData)

      //This correctly plots the disp on the x-axis and qsec on the y-axis of both randomly selected data frame rows and plots it into the correct scatterplot (bottom left one that has x-axis of disp and y-axis of qsec). This needs to be automated, so that the corresponding x and y values for the 2 randomly selected data frame rows are also plotted on all other scatterplot matrices.
      var trace1 = {
      x: [allData[0], allData[3]],
      y: [allData[2], allData[5]],
      mode: 'markers',
      marker: {
      color: 'green',
      size: 20
      }
      };

      Plotly.addTraces(el.id, trace1);
      }
      )}", data = dat)
