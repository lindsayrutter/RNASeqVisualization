library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(GGally)
library(edgeR)

coty <- read_delim("R/SISBID-2016-master/data/GSE61857_Cotyledon_normalized.txt.gz", delim="\t", col_types="cddddddddd", col_names=c("ID", "C_S1_R1", "C_S1_R2", "C_S1_R3", "C_S2_R1", "C_S2_R2", "C_S2_R3", "C_S3_R1", "C_S3_R2", "C_S3_R3"), skip=1)
coty <- as.data.frame(coty)
GGally::ggscatmat(head(coty,1000), columns=2:7, alpha=0.1) # use just the first 1000

sub <- coty %>% select(ID, C_S1_R1, C_S1_R2, C_S2_R1, C_S2_R2)
ggplot(sub, aes(x=C_S1_R1, xend=C_S1_R2, y=C_S2_R1, yend=C_S2_R2)) +
  geom_segment() + xlim(c(0, 17.5)) + ylim(c(0, 17.5)) +
  theme(aspect.ratio = 1)

coty$ID <- as.factor(coty$ID)

plotPCP <- ggparcoord(coty[1:1000,],
           columns=2:10, scale="globalminmax",
           alphaLines=0.1,
           aes(clickSelects = ID))

plotScat <- ggscatmat(head(coty,1000), columns=2:7, alpha=0.1, aes(clickSelects = ID))

############################################

sub <- coty[1,] %>%
  gather(sample, expr, -ID) %>%
  separate(sample, c("tissue", "stage", "rep")) %>%
  mutate(stage = as.numeric(substr(stage, 2, 2)))
sub.m <- sub %>% group_by(ID, stage) %>%
  summarise(expr = mean(expr))

plotInd <- ggplot(sub, aes(x=stage, y=expr, colour=factor(ID))) + geom_point(alpha=0.4) +
  geom_line(data=sub.m, aes(x=stage, y=expr)) +
  ggtitle(paste("Transcript: ", sub$ID[1])) + ylab("Read Count")



##################################################################
data(tips, package = "reshape")
tips$sex_smoker <- with(tips, interaction(sex, smoker))

p1 <- ggplot() + theme(legend.position = "none") +
  geom_point(data = tips,
             aes(x = sex, y = smoker,
                 clickSelects = sex_smoker, colour = sex_smoker),
             position = "jitter")

p2 <- ggplot() +
  geom_point(data = tips,
             aes(x = total_bill, y = tip,
                 showSelected = sex_smoker, colour = sex_smoker))

plots <- list(plot1 = p1, plot2 = p2)
structure(plots, class = "animint")


########################################
library(plotly)
library(data.table)

set.seed(1)
dat <- data.frame(ID = paste0("ID",1:10), A = runif(10), B = runif(10), C = runif(10), D = runif(10), E = runif(10))
dat$ID <- as.character(dat$ID)

plot_ly(data = dat, x = ~A, y = ~B)

datt <- data.frame(t(dat))

names(datt) <- as.matrix(datt[1, ])
datt <- datt[-1, ]
datt[] <- lapply(datt, function(x) type.convert(as.character(x)))
setDT(datt, keep.rownames = TRUE)[]
colnames(datt)[1] <- "x"

dat_long <- melt(datt, id.vars ="x" )

p <- plot_ly(dat_long, x= ~x, y= ~value, type = 'scatter', mode = 'lines+markers', color = ~variable)  %>% layout(dragmode="box", showlegend = FALSE)
