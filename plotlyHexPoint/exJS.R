########### Ex: Hovers on patients PCP ###########

library(plotly)
library(htmlwidgets)

set.seed(1056)

nPatients <- 50
nVisits <- 10

df <- data.frame(
  fev1_perc = rnorm(n = nPatients * nVisits, mean = 100, sd = 10),
  uin = rep(seq(nPatients), each = nVisits),
  visit = rep(seq(nVisits), nPatients)
)
c1 <- list(color = toRGB("steelblue", 0.5))
c2 <- list(color = toRGB("orange", 0.5))

# The color mapping is used only to generate multiple traces
# (which makes it easier to highlight lines on the JS side)
df %>%
  plot_ly(
    x = ~visit, y = ~fev1_perc, split = ~factor(uin), marker = c1, line = c2
  ) %>%
  layout(hovermode = "closest", showlegend = FALSE) %>%
  onRender('
           function(el, x) {
           var graphDiv = document.getElementById(el.id);
           // reduce the opacity of every trace except for the hover one
           el.on("plotly_hover", function(e) {
           var traces = [];
           for (var i = 0; i < x.data.length; i++) {
           if (i !== e.points[0].curveNumber) traces.push(i);
           }
           Plotly.restyle(graphDiv, "opacity", 0.2, traces);
           })
           el.on("plotly_unhover", function(e) {
           var traces = [];
           for (var i = 0; i < x.data.length; i++) traces.push(i);
           Plotly.restyle(graphDiv, "opacity", 1, traces);
           })
           }
           ')

########### Ex: Clicks on patients PCP ###########
df %>%
  plot_ly(
    x = ~visit, y = ~fev1_perc, split = ~factor(uin), marker = c1, line = c2
  ) %>%
  layout(hovermode = "closest", showlegend = FALSE) %>%
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

########### Ex: Change scatterplot opacity ###########
# Supposed to keep clicked point black, but it turns all gray
set.seed(1)
data <- data.frame(ID = paste0("ID",1:20), x = runif(20), y = runif(20))
data$ID <- as.character(data$ID)

p <- ggplot(data = data, aes(x=x,y=y)) +
  geom_point(size=0.5)

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

ggPS <- ggplotly(p)

ggPS %>% onRender("
           function(el, x) {
            console.log(el)
            el.on('plotly_click', function(e){
              var trace1 = {
              x: [e.points[0].x],
              y: [e.points[0].y],
              mode: 'markers',
              marker: {
                color: 'red',
                size: 10
              }
            };
            Plotly.addTraces(el.id, trace1);
            })
           }")




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



