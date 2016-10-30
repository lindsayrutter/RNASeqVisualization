############################################################################
# CS1 vs. CS2

coty <- read_delim("R/SISBID-2016-master/data/GSE61857_Cotyledon_normalized.txt.gz", delim="\t", col_types="cddddddddd", col_names=c("ID", "C_S1_R1", "C_S1_R2", "C_S1_R3", "C_S2_R1", "C_S2_R2", "C_S2_R3", "C_S3_R1", "C_S3_R2", "C_S3_R3"), skip=1)
coty <- as.data.frame(coty)
colnames(coty) <- c("ID","CS1.1","CS1.2","CS1.3","CS2.1","CS2.2","CS2.3","CS3.1","CS3.2","CS3.3")
countTable <- coty
nRep <- 3
nPerm <- 10
countTable <- countTable[,1:(2*nRep+1)]
outDir <- "PermLineup_CS1CS2"

getLineups(countTable, nRep, nPerm, outDir)

############################################################################
# CS1 vs. CS3

coty <- read_delim("R/SISBID-2016-master/data/GSE61857_Cotyledon_normalized.txt.gz", delim="\t", col_types="cddddddddd", col_names=c("ID", "C_S1_R1", "C_S1_R2", "C_S1_R3", "C_S2_R1", "C_S2_R2", "C_S2_R3", "C_S3_R1", "C_S3_R2", "C_S3_R3"), skip=1)
coty <- as.data.frame(coty)
colnames(coty) <- c("ID","CS1.1","CS1.2","CS1.3","CS2.1","CS2.2","CS2.3","CS3.1","CS3.2","CS3.3")
countTable <- coty
nRep <- 3
nPerm <- 10
countTable <- countTable[,c(1,2:4,8:10)]
outDir <- "PermLineup_CS1CS3"

getLineups(countTable, nRep, nPerm, outDir)

############################################################################
# CS2 vs. CS3

coty <- read_delim("R/SISBID-2016-master/data/GSE61857_Cotyledon_normalized.txt.gz", delim="\t", col_types="cddddddddd", col_names=c("ID", "C_S1_R1", "C_S1_R2", "C_S1_R3", "C_S2_R1", "C_S2_R2", "C_S2_R3", "C_S3_R1", "C_S3_R2", "C_S3_R3"), skip=1)
coty <- as.data.frame(coty)
colnames(coty) <- c("ID","CS1.1","CS1.2","CS1.3","CS2.1","CS2.2","CS2.3","CS3.1","CS3.2","CS3.3")
countTable <- coty
nRep <- 3
nPerm <- 10
countTable <- countTable[,c(1,5:10)]
outDir <- "PermLineup_CS2CS3"

getLineups(countTable, nRep, nPerm, outDir)
