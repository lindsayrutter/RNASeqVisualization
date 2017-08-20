load("../All_leaves040615.rda")
ct <- countTable
ct2 <- assays(ct)[[1]]
rownames(ct2) <- ct@rowRanges@elementMetadata@listData$ID
colnames(ct2) <- unlist(strsplit(colnames(ct2), "\\."))[seq(1, 17*3, 3)]
leaves.all <- ct2
load("../All_roots.rda")
ct.roots <- countTable
ct2 <- assays(ct.roots)[[1]]
rownames(ct2) <- ct.roots@rowRanges@elementMetadata@listData$ID
colnames(ct2) <- unlist(strsplit(colnames(ct2), "\\."))[seq(1, 18*3, 3)]
roots.all <- ct2
all <- cbind(leaves.all, roots.all[,c(1:15,17:18)])
y <- DGEList(counts=all)

L120 = y[,c("ML08R","ML14R","ML22R","ML11R","ML27R","ML33R")]
colnames(L120)=c("N.1","N.2","N.3","P.1","P.2","P.3") #L120$samples$group
L120 <- calcNormFactors(L120)
group <- factor(c(1,1,1,2,2,2))
L120$samples$group <- group
design <- model.matrix(~group)
rownames(design) <- colnames(L120)
L120 <- estimateDisp(L120, design, robust=TRUE)
fit <- glmFit(L120, design)
lrt <- glmLRT(fit)
topTags(lrt)

metrics = list()
metrics[["NvsP"]] = data.frame(ID = as.factor(rownames(lrt[[14]])), logFC = lrt[[14]]$logFC, PValue = lrt[[14]]$PValue)
save(metrics,file="metrics.Rda")
