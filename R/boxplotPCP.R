# Script demonstrating that boxplots do not show individual observations. In contrast, parallel coordinate plots do show individual observations and allow for us to better see whether replications are consistent for individual observations.

library(ggplot2)
library(cowplot) # Need for combining ggplot2 output into one plot aesthetically

# This function creates a boxplot and parallel coordinate plot for five replications
makePlots <- function(A.1, A.2, A.3, A.4, A.5, i){
  dat <- data.frame(ID = paste0("ID", 1:50), A.1, A.2, A.3, A.4, A.5)
  datM <- melt(dat, id.vars = "ID")
  colnames(datM) <- c("ID", "Group", "Value")

  boxPlots[[i]] <<- ggplot(datM, aes(Group, Value)) + geom_boxplot()

  # Convert DF from scatterplot to PCP
  datt <- data.frame(t(dat))
  names(datt) <- as.matrix(datt[1, ])
  datt <- datt[-1, ]
  datt[] <- lapply(datt, function(x) type.convert(as.character(x)))
  setDT(datt, keep.rownames = TRUE)[]
  dat_long <- melt(datt, id.vars ="rn" )
  colnames(dat_long) <- c("Group", "ID", "Value")

  pcpPlots[[i]] <<- ggplot(dat_long) + geom_line(aes(x = Group, y = Value, group = ID, color = ID)) + theme(legend.position="none")
}

set.seed(1)
boxPlots <<- vector('list', 2)
pcpPlots <- vector('list', 2)

# In the first case, we purposely create individual observations that will be inconsistent across their replications
A.1=sort(rnorm(50,10))
A.2=rev(sort(rnorm(50,10)))
A.3=sort(rnorm(50,10))
A.4=rev(sort(rnorm(50,10)))
A.5=sort(rnorm(50,10))

makePlots(A.1, A.2, A.3, A.4, A.5, 1)

# In the second case, we purposely create individual observations that will be more consistent across their replications
A.2=sort(rnorm(50,10))
A.4=sort(rnorm(50,10))

makePlots(A.1, A.2, A.3, A.4, A.5, 2)

# View the first case
plot_grid(boxPlots[[1]], pcpPlots[[1]], labels=c("A", "B"), ncol = 1, nrow = 2)

# View the second case
plot_grid(boxPlots[[2]], pcpPlots[[2]], labels=c("A", "B"), ncol = 1, nrow = 2)
