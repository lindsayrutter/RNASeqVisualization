# Hovers on patients PCP

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





# Try click instead of hover
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









# Goes to URL of scatterplot points
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



