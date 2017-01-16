cnToID <- function(h){
  df <- data.frame(table(h@cID))
  colnames(df) <- c("ID","count")
  cnID <- df[order(df$count,as.character(df$ID)),]
  cnID$curveNumber <- seq(0, nrow(cnID)-1)
  return(cnID)
}

