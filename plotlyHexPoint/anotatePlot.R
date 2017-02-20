library(plotly)

trace_0 <- rnorm(100, mean = 5)
trace_1 <- rnorm(100, mean = 0)
trace_2 <- rnorm(100, mean = -5)
x <- c(1:100)

data <- data.frame(x, trace_0, trace_1, trace_2)

data %>% plot_ly(x = ~x, y = ~trace_0, name = 'trace 0', type = 'scatter', mode = 'lines') %>%
  layout(hovermode = "closest", showlegend = FALSE) %>%
  onRender("
           function(el, x) {
           console.log(el)
           var myGraph = document.getElementById(el.id);
           el.on('plotly_click', function(e) {
           var pts = '';
           console.log(e)
           console.log(e.points.length)
           for(var i=0; i < e.points.length; i++){
           var annotate_text = 'x = '+e.points[i].x +
           'y = '+e.points[i].y.toPrecision(4);
           
           var annotation = {
           text: annotate_text,
           x: e.points[i].x,
           y: parseFloat(e.points[i].y.toPrecision(4))
           }
           console.log(annotation)
           console.log(el.layout.annotations)
           annotations = el.layout.annotations || [];
           annotations.push(annotation);
           Plotly.relayout(el.id,{annotations: annotations})
           }
           })}
           ") 
