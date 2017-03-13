library(plotly)
library(htmlwidgets)

ui <- shinyUI(fluidPage(
  sliderInput("treshold", "Threshold:", min = 0, max = 4, value=1, step=0.1),
  plotlyOutput("myPlot")
))

server <- shinyServer(function(input, output) {
  set.seed(1)
  dat = data.frame(mpg = rnorm(50,-30,20), disp=rnorm(50,-30,20))

  p <- ggplot(data = dat, aes(x=disp,y=mpg))

#  regRes <- observeEvent(input$threshold,

  res <- lm(mpg~disp, data=dat)

  q<-qplot(mpg, disp, data=dat, geom=c('point', 'smooth'), method='lm')

  q <- ggplot(dat, aes(mpg,disp)) + geom_smooth(method="lm") #+ geom_point()


  dat.lm <- lm(mpg~disp,data=dat)

  # Response variable
  diseasesev<-c(1.9,3.1,3.3,4.8,5.3,6.1,6.4,7.6,9.8,12.4)

  # Predictor variable
  temperature<-c(2,1,5,5,20,20,23,10,30,25)
  # change to c(2,1,5,5,20,20,23,10,30,25) to make red line better resolution

  ## For convenience, the data may be formatted into a dataframe
  severity <- as.data.frame(cbind(diseasesev,temperature))

  severity.lm <- lm(diseasesev~temperature,data=severity)
  pred <- as.data.frame(predict(severity.lm,se.fit=TRUE,
                                level=0.95,interval="confidence"))
  dat <- data.frame(diseasesev,temperature,
                    in_interval = diseasesev <=pred$fit.upr & diseasesev >=pred$fit.lwr, pred)

  ggplot(dat,aes(y=diseasesev,x=temperature)) +
    stat_smooth(method='lm')  +
    geom_point(aes(colour=in_interval)) +
    geom_line(aes(y=fit.lwr),colour=I('red')) +
    geom_line(aes(y=fit.upr),colour=I('red'))



  #> head(res)
  #$coefficients
  #(Intercept)         disp
  #-28.91859904  -0.03354243

  visreg(res)

  pre <- predict(res, interval="confidence")


#  )

  ggplotly(p) %>%
    onRender("
             function(el, x, data) {

                console.log(data)

               //AxisNames = [];
               //for (i = 1; i < 3; i++) {
                //  AxisNames.push(document.getElementsByClassName('infolayer')[0].childNodes[i].textContent);
               //}

             //var selRows = [];
             //data.dat.forEach(function(row){
             //if(Math.abs(row[AxisNames[0]]-row[AxisNames[1]]) > Math.sqrt(2)*data.cv) selRows.push(row);
             //});
             //console.log(selRows);

             //var xArr = [];
             //for (a=0; a<selRows.length; a++){
            //   xArr.push(selRows[a][AxisNames[0]])
             //}
             //var yArr = [];
             //for (a=0; a<selRows.length; a++){
            //   yArr.push(selRows[a][AxisNames[1]])
             //}

             //var tracePoints = {
             //  x: xArr,
              // y: yArr,
              // hoverinfo: 'none',
              // mode: 'markers',
              // marker: {
            //    color: 'black',
            //    size: 4
            //   }
            // }

            // var traceHiLine = {
            //   x: [data.minLine, data.maxLine - Math.sqrt(2)*data.cv],
            //   y: [data.minLine + Math.sqrt(2)*data.cv, data.maxLine],
            //   mode: 'lines',
            //   line: {
            //     color: 'gray',
            //     width: 1
            //   },
            //   opacity: 0.25
            // }

             //var traceLoLine = {
            //   x: [data.minLine + Math.sqrt(2)*data.cv, data.maxLine],
            //   y: [data.minLine, data.maxLine - Math.sqrt(2)*data.cv],
            //   mode: 'lines',
            //   fill: 'tonexty',
            //   line: {
            //     color: 'gray',
            //     width: 1
            //   },
            //   opacity: 0.25
             //}

            // Plotly.addTraces(el.id, traceHiLine);
            // Plotly.addTraces(el.id, traceLoLine);
            // Plotly.addTraces(el.id, tracePoints);

            // el.on('plotly_selected', function(e) {
            //   console.log(e)
            //   numSel = e.points.length
            //   console.log(numSel)

            //   var selX = [];
            //   for (a=0; a<numSel; a++){
            //     selX.push(e.points[a].x)
            //   }
            //   var selY = [];
            //   for (a=0; a<numSel; a++){
            //     selY.push(e.points[a].y)
            //   }

            //   console.log(selX);
            //   console.log(selY);

            //   var selPoints = {
            //     x: selX,
            //     y: selY,
            //     hoverinfo: 'none',
            //     mode: 'markers',
            //     marker: {
            //      color: 'red',
            //      size: 4
            //     }
            //   }

          //   Plotly.addTraces(el.id, selPoints);
            // })

             }
             ", data = dat)})

shinyApp(ui, server)
