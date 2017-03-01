library(plotly)
library(broom)

dat <- mtcars
dat$mpg <- dat$mpg * 10

p <- ggplot(data = dat, aes(x=disp,y=mpg)) + geom_point(size=0.5)

ggplotly(p) %>%
  onRender("
           function(el, x, data) {
            var graphDiv = document.getElementById(el.id);
            // reduce the opacity of every trace except for the hover one
            el.on('plotly_click', function(e) {
              Plotly.restyle(graphDiv, 'opacity', 1)


              //var traces = [];
              //for (var i = 0; i < x.data.length; i++) {
              //  if (i !== e.points[0].curveNumber) traces.push(i);
              //}
              Plotly.restyle(graphDiv, 'opacity', 0.2, traces);
           })
          }
          ", data=dat)

p <- plot_ly(dat, x = ~disp, color = I("black")) %>%
  add_markers(y = ~mpg, text = rownames(dat), showlegend = FALSE) %>%
  add_lines(x=y) %>%
  add_ribbons(data = augment(m),
              ymin = ~.fitted - 1.96 * .se.fit,
              ymax = ~.fitted + 1.96 * .se.fit,
              line = list(color = 'rgba(7, 164, 181, 0.05)'),
              fillcolor = 'rgba(7, 164, 181, 0.2)',
              name = "Standard Error")


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
