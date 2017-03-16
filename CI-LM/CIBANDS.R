y<-c(1.9,3.1,3.3,4.8,5.3,6.1,6.4,7.6,9.8,12.4)
# Predictor variable, (Centigrade)
x<-c(2,1,5,5,20,20,23,10,30,25)

## For convenience, the data may be formatted into a dataframe
dat <- as.data.frame(cbind(y,x))

## Fit a linear model for the data and summarize the output from function lm()
dat.lm <- lm(y~x,data=dat)

# Function to plot the linear regression and overlay the confidence intervals
ci.lines<-function(model, conf= .95, interval = "confidence"){
  x <- model[[12]][[2]]
  y <- model[[12]][[1]]
  xm<-mean(x)
  n<-length(x)
  ssx<- sum((x - mean(x))^2)
  # T-distribution with vector of probabilities and df
  st<- qt(1-(1-conf)/2,(n-2))
  # x and y vectors to plot best fit line
  xv<-seq(min(x),max(x),(max(x) - min(x))/100)
  yv<- coef(model)[1]+coef(model)[2]*xv

  se <- switch(interval,
              # First part is residual standard error
               confidence = summary(model)[[6]] * sqrt(1/n+(xv-xm)^2/ssx),
               prediction = summary(model)[[6]] * sqrt(1+1/n+(xv-xm)^2/ssx)
  )

  ci<-st*se
  uyv<-yv+ci
  lyv<-yv-ci
  limits1 <- min(c(x,y))
  limits2 <- max(c(x,y))

  predictions <- predict(model, level = conf, interval = interval)

  # Calculate predictions without R
  lwr = c()
  upr = c()
  predY = c()
  sei = c()
  for (i in 1:n){
    sei[i] = summary(model)[[6]] * sqrt(1/n+(x[i]-xm)^2/ssx)
    predY[i] = coef(model)[1]+coef(model)[2]*x[i]
    lwr[i] = predY[i] - sei[i] *st
    upr[i] = predY[i] + sei[i] *st
  }

  insideCI <- predictions[,'lwr'] < y & y < predictions[,'upr']

  plot(x[insideCI],y[insideCI],
       pch=16,pty="s",xlim=c(limits1,limits2),ylim=c(limits1,limits2))

  abline(model)

  points(x[!insideCI],y[!insideCI], pch = 16, col = 'red')

  # lty = linetype, col = color
  lines(xv,uyv,lty=2,col=3)
  lines(xv,lyv,lty=2,col=3)
}

ci.lines(dat.lm, conf= .95 , interval = "confidence")
ci.lines(severity.lm, conf= .85 , interval = "prediction")
