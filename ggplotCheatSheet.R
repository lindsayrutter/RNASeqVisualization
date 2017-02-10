a <- ggplot(seals, aes(x = long, y = lat))
a + geom_curve(aes(yend = lat + delta_lat, xend = long + delta_long, curvature = z))

b <- ggplot(economics, aes(date, unemploy))
b + geom_ribbon(aes(ymin=unemploy-900, ymax=unemploy+900))

# One continuous
c <- ggplot(mpg, aes(hwy))
c + geom_area(stat="bin")
c + geom_density(kernel="gaussian")
c + geom_dotplot()
c + geom_freqpoly()
c + geom_histogram(binwidth=5)

# One discrete
d <- ggplot(mpg,aes(fl))
d + geom_bar()

# Two continuous
e <- ggplot(mpg, aes(cty, hwy))
e + geom_label(aes(label=cty), nudge_x=1, nudge_y=1, check_overlap=TRUE)
e + geom_jitter(height=2, width=2)
e + geom_rug(sides="bl")
e + geom_smooth(method=lm)
e + geom_text(aes(label=cty), nudge_x=1, nudge_y=1)

# Discrete X Continuous Y
f <- ggplot(mpg, aes(class, hwy))
f + geom_bar(stat = "identity")
f + geom_boxplot()
f + geom_dotplot(binaxis ="y")
f + geom_violin(scale = "area")

# Continous Bivariate Distribution
h <- ggplot(diamonds, aes(carat, price))
h + geom_bin2d(binwidth = c(0.25, 500))
h + geom_density2d()
h2 <- h + geom_hex()
