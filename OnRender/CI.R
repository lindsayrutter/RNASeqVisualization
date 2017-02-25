########### Ex: Change scatterplot opacity ###########
# Supposed to keep clicked point black, but it turns all gray

library(plotly)
library(htmlwidgets)

set.seed(1)
data <- data.frame(ID = paste0("ID",1:20), x = runif(20), y = runif(20))
data$ID <- as.character(data$ID)

p <- ggplot(data = data, aes(x=x,y=y)) + geom_point(size=0.5)

ggplotly(p) %>%
  onRender('
           function(el, x) {
           var graphDiv = document.getElementById(el.id);
           // reduce the opacity of every trace except for the hover one
           el.on("plotly_click", function(e) {
           Plotly.restyle(graphDiv, "opacity", 1)
           var traces = [];
           for (var i = 0; i < x.data[0].text.length; i++) {
            if (i !== e.points[0].pointNumber){
              traces.push(i);
            }
           }

           console.log(traces)
           Plotly.restyle(graphDiv, "opacity", 0.2, traces);
           })
           }
           ')















ggplotly(p) %>%
  onRender('
           function(el, x) {
            var graphDiv = document.getElementById(el.id);
            // reduce the opacity of every trace except for the hover one

            console.log(el)
            console.log(x)
            console.log(graphDiv)
            console.log(x.data[0].text.length);

            el.on("plotly_click", function(e) {

              //console.log(e.points[0].curveNumber)

              Plotly.restyle(graphDiv, "opacity", 1)
              var traces = [];

              for (var i = 0; i < x.data[0].text.length; i++) {
                if (i !== e.points[0].curveNumber) {
                  //console.log(i)
                  traces.push(i);
                }
              }
              //traces.push(8)

              //console.log(traces)
              Plotly.restyle(graphDiv, "opacity", 0.2, traces);
           })
          }
          ')





########### Ex: Change scatterplot opacity w/ CI ###########
# Supposed to keep clicked point black and turn rest of points gray, but it keeps all points black and turns CI gray
set.seed(1)
data <- data.frame(ID = paste0("ID",1:20), x = runif(20), y = runif(20))
data$ID <- as.character(data$ID)

ciVal = 0.5
myMax = max(data[,2:3])
myMin = min(data[,2:3])
myMid = (myMax-myMin)/2
data2 <- data.frame(x = c(myMin, myMax), y = c(myMin, myMax))

keep <- abs(data$x - data$y) >= ciVal
df <- data.frame(data[keep,])

p <- ggplot(data = df, aes(x=x,y=y)) +
  geom_point(size=0.5) +
  geom_ribbon(data=data2, aes(x=x, ymin = y-ciVal, ymax = y+ciVal), fill = "lightgrey") +
  geom_abline(intercept = 0, color = "white", size = 0.25) +
  scale_x_continuous(limits = c(myMin, myMax)) +
  scale_y_continuous(limits = c(myMin, myMax)) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))

ggplotly(p) %>%
  onRender('
           function(el, x) {
            var graphDiv = document.getElementById(el.id);
            // reduce the opacity of every trace except for the hover one
            el.on("plotly_click", function(e) {
              Plotly.restyle(graphDiv, "opacity", 1)
              var traces = [];
              for (var i = 0; i < x.data.length; i++) {
                if (i !== e.points[0].curveNumber) traces.push(i);
              }
              Plotly.restyle(graphDiv, "opacity", 0.2, traces);
           })
          }
          ')

################ Ex: Goes to URL of scatterplot points ################
library(plotly)
library(htmlwidgets)
search <- paste0(
  "http://google.com/#q=", curl::curl_escape(rownames(mtcars))
)
plot_ly(mtcars, x = ~wt, y = ~mpg, color = ~factor(vs)) %>%
  add_markers(text = ~search, hoverinfo = "x+y") %>%
  onRender("
    function(el, x) {
      el.on('plotly_click', function(d) {
        // d.points is an array of objects which, in this case,
        // is length 1 since the click is tied to 1 point.
        var pt = d.points[0];
        var url = pt.data.text[pt.pointNumber];
        // DISCLAIMER: this won't work from RStudio
        window.open(url);
      });
    }
  ")



