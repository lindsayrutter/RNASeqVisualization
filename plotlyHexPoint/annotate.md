---
title: "annotePlot"
output: html_document
---




```r
########### Ex: Change scatterplot opacity ###########
# Supposed to keep clicked point black, but it turns all gray
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

p <- ggplot(data = data, aes(x=x,y=y)) +
  geom_point(size=0.5) +
  scale_x_continuous(limits = c(myMin, myMax)) +
  scale_y_continuous(limits = c(myMin, myMax)) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))
```

```
## Scale for 'x' is already present. Adding another scale for 'x', which
## will replace the existing scale.
```

```
## Scale for 'y' is already present. Adding another scale for 'y', which
## will replace the existing scale.
```

```r
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
```

```
## PhantomJS not found. You can install it with webshot::install_phantomjs(). If it is installed, please make sure the phantomjs executable can be found via the PATH variable.
```

```
## Warning in normalizePath(f2): path[1]="./webshot8acd10d4ba6d.png": No such
## file or directory
```

```
## Warning in file(con, "rb"): cannot open file './webshot8acd10d4ba6d.png':
## No such file or directory
```

```
## Error in file(con, "rb"): cannot open the connection
```
