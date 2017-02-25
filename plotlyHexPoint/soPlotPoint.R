library(plotly)
library(htmlwidgets)

g <- ggplot(mtcars, aes(x=mpg, y=cyl)) + geom_point()
gP <- ggplotly(g)
gP %>%
  layout(
    xaxis = list(range = c(8, 35)),
    yaxis = list(range = c(3, 9))
  )

gP %>% onRender("
function(el, x) {
  myGraph = document.getElementById(el.id);
  el.on('plotly_click', function(e)
    {

      var trace1 = {
        x: [20, 25, 30],
        y: [5, 5, 5],
        mode: 'markers',
        marker: {
          color: 'green',
          size: 20
        }
      };
      //for (var i = 0; i < 100; i+= 1){
      //  trace1.x.push(e.points[0].x + Math.random())
      //  trace1.y.push(e.points[0].y + Math.random())
      //}
    Plotly.addTraces(el.id, trace1);
  }
)}
")
