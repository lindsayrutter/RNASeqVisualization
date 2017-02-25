library(plotly)
library(htmlwidgets)

p <- ggpairs(mtcars[,c(3,4,7)])

maxVal = max(abs(mtcars[,c(3,4,7)]))
minVal = min(abs(mtcars[,c(3,4,7)]))
maxRange = c(minVal, maxVal)

pS <- p
for(i in 2:p$nrow) {
  for(j in 1:(i-1)) {
    pS[i,j] <- p[i,j] +
      coord_cartesian(xlim = c(maxRange[1], maxRange[2]), ylim = c(maxRange[1], maxRange[2]))
  }
}

ggPS <- ggplotly(pS)

ggPS %>% onRender("
function(el, x) {
  console.log(el)
  var myGraph = document.getElementsByClassName('subplot xy2');
  console.log(el.'plot-container plotly')
  el.on('plotly_click', function(e)
    {

      var trace1 = {
        x: [200, 300, 400],
        y: [300, 350, 400],
        mode: 'markers',
        marker: {
          color: 'green',
          size: 20
        }
      };

    Plotly.addTraces(el[0][0][0][3][1][6][4], trace1);
    //Plotly.addTraces(myGraph, trace1);
  }
)}
")
