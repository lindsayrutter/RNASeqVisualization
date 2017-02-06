# Script attempting to show that normalized-looking boxplots may show up as otherwise in PCP
dat <- data.frame(ID = paste0("ID",1:50), A.1=sort(rnorm(50,10)), A.2=rnorm(50,10), A.3=rnorm(50,10), B.1=rnorm(50,10), B.2=rnorm(50,10), B.3=rnorm(50,10))

boxplot(dat[,2:7])

# Convert DF from scatterplot to PCP
datt <- data.frame(t(dat))
names(datt) <- as.matrix(datt[1, ])
datt <- datt[-1, ]
datt[] <- lapply(datt, function(x) type.convert(as.character(x)))
setDT(datt, keep.rownames = TRUE)[]
dat_long <- melt(datt, id.vars ="rn" )

p <- ggplot(dat_long) + geom_line(aes(x = rn, y = value, group = variable, color = variable))
