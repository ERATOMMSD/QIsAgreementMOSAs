setwd(".")
rm(list = ls(all = TRUE))

data <- read.table(file = "inputData.csv", head = TRUE, sep = ";")
QIs <- c("HV", "IGD", "EP", "GD", "GS", "ED", "PFS", "C")
ALGs <- c("CELLDE", "MOCELL", "NSGAII", "PAES", "SMPSO", "SPEA2")

data$CaseStudy <- as.character(data$CaseStudy)
#TP1 remains TP1
#TP2, TP3 and TP4 should become TP2_1, TP2_2, TP2_3
data$CaseStudy[data$CaseStudy == "TP2"] <- "TP2_1"
data$CaseStudy[data$CaseStudy == "TP3"] <- "TP2_2"
data$CaseStudy[data$CaseStudy == "TP4"] <- "TP2_3"
data$CaseStudy <- as.factor(data$CaseStudy)

#Problems <- c("RA", "TS", "TRA", "RP", "TM", "TP1", "TP2", "TP3", "TP4", "RM", "ITO")
Problems <- as.vector(unique(data$CaseStudy))

data$PairAlgs <- as.factor(paste(data$A, data$B, sep = "_"))

dataDiffStructure <- data.frame()
for (p in Problems) {
  dataP <- subset(data, data$CaseStudy == p)
  for (ca in unique(data$PairAlgs)) {
    dataPca <- subset(dataP, dataP$PairAlgs == ca)
    if (NROW(dataPca) > 0) {
      stopifnot(NROW(dataPca) == 1)
      for (i in 1:(length(QIs) - 1)) {
        for (j in (i + 1):length(QIs)) {
          row <- data.frame(
            qi1vsqi2 = paste(QIs[i], QIs[j], sep = "vs"),
            qi1 = QIs[i],
            qi2 = QIs[j],
            Problem = p,
            PairAlgs = ca,
            AlgA = dataPca[, c("A")],
            AlgB = dataPca[, c("B")],
            resQI1 = as.character(dataPca[, QIs[i]]),
            resQI2 = as.character(dataPca[, QIs[j]]),
            Agree = as.character(dataPca[, QIs[i]]) == as.character(dataPca[, QIs[j]])
          )
          dataDiffStructure <- rbind(dataDiffStructure, row)
          
        }
      }
    }
  }
}
write.table(
  dataDiffStructure,
  file = "inputDataDiffStructure.txt",
  sep = "\t",
  quote = FALSE,
  row.names = FALSE
)
