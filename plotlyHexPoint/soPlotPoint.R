library(plotly)
library(htmlwidgets)

g <- ggplot(mtcars, aes(x=mpg, y=cyl)) + geom_point()
gP <- ggplotly(g)

gP %>% onRender("
          function(el, x) {
            myGraph = document.getElementById(el.id);

            el.on('plotly_click', function(e) {

              console.log(e)
              console.log(e.points[0].x)

              var trace1 = {
                x: [e.points[0].x-.3, e.points[0].x-.3, e.points[0].x+.3, e.points[0].x+.3],
                y: [e.points[0].y-.3, e.points[0].y+.3, e.points[0].y-.3, e.points[0].y+.3],
                type: 'scatter',
                fillColor : 'red',
                size: 20
              };

              Plotly.addTraces(el.id, trace1);
           })}
           ")

#Plotly.addTraces(el.id, trace1);
# restyle
# redraw
