set.seed(3)

pcpDat <- data.frame(ID = paste0("ID",1:10), A=rnorm(10,-1), B=rnorm(10,-1), C=rnorm(10,-1), D=rnorm(10,1), E=rnorm(10,1), F=rnorm(10,1), pvalue = runif(10,0,1))
pcpDat$ID <- as.character(pcpDat$ID)
colNms <- colnames(pcpDat[, c(2:(ncol(pcpDat)))])
nVar <- length(colNms)

