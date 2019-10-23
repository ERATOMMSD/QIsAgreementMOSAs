setwd(".")
rm(list = ls(all = TRUE))
library("ggpubr") # for visualizing the distribution
library("Kendall")#for Kendall
library("effsize")#for A12 test https://rdrr.io/cran/effsize/man/VD.A.html
library("fmsb") # for Kappa.test
#k            Interpretation
#k< 0 	      Poor agreement
#0.01 - 0.20 	Slight agreement
#0.21 - 0.40 	Fair agreement
#0.41 - 0.60 	Moderate agreement
#0.61 - 0.80 	Substantial agreement
#0.81 - 0.99 	Almost perfect agreement
#1 Perfect agreement

options(scipen = 2)#for scientific notation
options(digits = 3)

RECTANGULAR_PLOT = TRUE

suffix = ""
w = 7
h = 7
if(RECTANGULAR_PLOT) {
  w = 10
  h = 5
  suffix = "_rect"
}

data <- read.table(file = "inputData/inputData.csv", head = TRUE, sep = ";")
QIs <- c("HV", "IGD", "EP", "GD", "GS", "ED", "PFS", "C")
ALGs <- c("CELLDE", "MOCELL", "NSGAII", "PAES", "SMPSO", "SPEA2")

#renaming of case studies
#to trace the origin of a case study from a given problem
data$CaseStudy <- as.character(data$CaseStudy)
#TP1 remains TP1
#TP2, TP3 and TP4 should become TP2_1, TP2_2, TP2_3
data$CaseStudy[data$CaseStudy == "TP2"] <- "TP2_1"
data$CaseStudy[data$CaseStudy == "TP3"] <- "TP2_2"
data$CaseStudy[data$CaseStudy == "TP4"] <- "TP2_3"
data$CaseStudy <- as.factor(data$CaseStudy)

#we here consider each case study as a separate problem
Problems <- as.vector(unique(data$CaseStudy))

data$pairAlgs <- as.factor(paste(data$A, data$B, sep = "_"))

#same data contained in the input file inputData.csv, but in a different format
#inputDataDiffStructure.txt is computed from inputData.csv using script inputData/buildDifferentDataStructure.R
dataDiffStructure <- read.table(file = "inputData/inputDataDiffStructure.txt", head = TRUE, sep = "\t")


#####################################

# Functions

agreeTests <- function(dataForAgreeEval, QIi, QIj) {
  # overall agreement
  #http://www.john-uebersax.com/stat/raw.htm
  numExps <- NROW(dataForAgreeEval)
  sameEval <- subset(dataForAgreeEval, dataForAgreeEval[, QIi] == dataForAgreeEval[, QIj])
  overallAgreement <- NROW(sameEval) / numExps

  resQIiQIj <- dataForAgreeEval[, c(QIi, QIj)]

  #McNemar-Bowker test
  #http://www.john-uebersax.com/stat/mcnemar.htm
  contingencyTable <- table(resQIiQIj)
  mcNemar <- mcnemar.test(contingencyTable)
  #computing the Kappa value could raise a warning or an error.
  #in this case, we mark the Kappa value as "ERROR"
  kTest = tryCatch({
    Kappa.test(contingencyTable)
  }, warning = function(w) {
    "ERROR"
  }, error = function(e) {
    "ERROR"
  })
  #Kappa returns an error when there is perfect agreement.
  #Therefore, we modify the estimate, the judgement, and the p-value
  #accordingly
  kTestEstimate = as.numeric(ifelse(kTest != "ERROR", kTest$Result$estimate, 1))
  kTestJudgement = as.character(ifelse(kTest != "ERROR", kTest$Judgement, 'Perfect agreement'))
  kTestP = as.character(ifelse(kTest != "ERROR", kTest$Result$p.value, 0))

  testsForQIiAndQIj <-
    data.frame(
      q1vsq2 = paste(QIi, QIj, sep = "vs"),
      QI1 = QIi,
      QI2 = QIj,
      OverallAgreement = overallAgreement,
      kTestEst = kTestEstimate,
      kTestJudg = kTestJudgement,
      kTestPvalue = kTestP,
      bowkerP = mcNemar$p.value,
      stringsAsFactors = FALSE
    )
  #when the is perfect overall agreement, but the judgment of K or Bowker are NA (due to few samples), we can safely fix them
  testsForQIiAndQIj$kTestJudg[is.na(testsForQIiAndQIj$kTestJudg) & testsForQIiAndQIj$OverallAgreement == 1] <- 'Perfect agreement'
  testsForQIiAndQIj$bowkerP[is.na(testsForQIiAndQIj$bowkerP) & testsForQIiAndQIj$OverallAgreement == 1] <- 1
  testsForQIiAndQIj$q1vsq2 <- as.factor(testsForQIiAndQIj$q1vsq2)
  unique(testsForQIiAndQIj)
}

#We used Mann-Whitney’s U test to determine the significance of results,
#and Vargha and Delaney's A12 statistics as the effect size measure to
#determine the strength of the significance.
#We compared two algorithms, i.e., A and B with respect to a QI (e.g., HV) as follows:
#1) If a p-value computed by U test was less than 0.05, and A12 value was greater than 0.5,
#it means that A is significantly better than B in terms of the QI (i.e., HV);
#2) If a p-value computed by U test was less than 0.05, and A12 was less than 0.5,
#it means that B is significantly better than A in terms of the QI (i.e., HV);
#3) If a p-value was greater than 0.05, it means there is No Significant (ND) difference between
#A and B in terms of the QI (i.e., HV).
catVDAwilcox <- function(data1, data2, element1, element2) {
  UtestPvalue <- wilcox.test(data1, data2, exact = FALSE)$p.value
  A12est <- VD.A(data1, data2)$estimate #A12
  category <-
    ifelse(UtestPvalue >= 0.05,
           "EQUAL",
           ifelse(A12est > 0.5, "GREATER", "LOWER"))
  row <- data.frame(element1, element2, category, A12est, UtestPvalue)
}

#####################################


#RQ1 (28 rows of pairs of quality indicators)
rq1 <- data.frame()
for (i in 1:(length(QIs) - 1)) {
  for (j in (i + 1):length(QIs)) {
    testsForQIiAndQIj <- agreeTests(data, QIs[i], QIs[j])
    rq1 <- rbind(rq1, testsForQIiAndQIj)
  }
}
rq1 <- rq1[order(-rq1$kTestEst),]
write.table(rq1, file = "results/rq1.txt", sep = "\t", quote = FALSE, row.names = FALSE)

pdf(file = paste0("plots/rq1_OA_K", suffix, ".pdf"), width = w, height = h)
par(mar = c(7, 3, 0.1, 0.1))
plot(
  rq1$kTestEst,
  xaxt = 'n',
  ylim = c(0, 1),
  #xlab = "Quality indicator pair",
  xlab = "",
  ylab = "",
  las = 1,
  pch = 1
)
#title(xlab="Quality indicator pair", mgp=c(5,1,0))
axis(1, at = seq(nrow(rq1)), labels = rq1$q1vsq2, las = 2)
lines(rq1$kTestEst)
lines(rq1$OverallAgreement)
points(rq1$OverallAgreement, pch = 6)
legend(2, 1, legend = c("K-value", "Overall agreement"), pch = c(1, 6), cex = 0.8)
dev.off()

pdf(file = paste0("plots/rq1_OA_K_pValues", suffix, ".pdf"), width = w, height = h)
par(mar = c(7, 3, 0.1, 0.1))
plot(
  rq1$kTestPvalue,
  xaxt = 'n',
  ylim = c(0, 1),
  #xlab = "Quality indicator pair",
  xlab = "",
  ylab = "",
  las = 1,
  pch = 1
)
#title(xlab="Quality indicator pair", mgp=c(5,1,0))
axis(1, at = seq(nrow(rq1)), labels = rq1$q1vsq2, las = 2)
lines(rq1$kTestPvalue)
lines(rq1$bowkerP)
points(rq1$bowkerP, pch = 8)
legend(2, 1, legend = c("Kappa", "Bowker"), pch = c(1, 8), cex = 0.8)
dev.off()

#http://www.sthda.com/english/wiki/normality-test-in-r
#Shapiro-Wilk normality test
shapiro.test(rq1$OverallAgreement) #W = 1, p-value = 0.2 (normal distribution)
shapiro.test(rq1$kTestEst) #W = 0.9, p-value = 0.002 (not normal distribution)
#visual test of normality
ggdensity(rq1$OverallAgreement)
ggqqplot(rq1$OverallAgreement)
ggdensity(rq1$kTestEst)
ggqqplot(rq1$kTestEst)
#The distribution if Kappa is not normal, and so we apply the non-parametric test Kendall
rq1KendallOAvsK <- Kendall(rq1$OverallAgreement, rq1$kTestEst) #tau = 0.835, 2-sided pvalue < 2e-16
write.table(matrix(c("pair", "tau", "p-value", "OAvsK", rq1KendallOAvsK$tau, rq1KendallOAvsK$sl), ncol = 3, byrow = TRUE),
            file = "results/rq1Kendall.txt", sep = "\t", quote = FALSE, row.names = FALSE, col.names = FALSE)

wilcox.test(rq1$OverallAgreement, rq1$kTestEst, exact = FALSE) #p-value = 0.00003

#####################################

#RQ2 (28*11=308 rows of pairs of quality indicators (28) and problems (11))
#similar to RQ1, but the results are disaggregated by problem (11 case studies)
rq2 <- data.frame()
rq2_1_Kendall_Problem <- data.frame()
for (p in Problems) {
  dataP <- subset(data, data$CaseStudy == p)
  #numExpsPerP <- NROW(dataP) #15 for all the problems, except for RM for which are 10 (CellDE is not applicable for RM)
  for (i in 1:(length(QIs) - 1)) {
    for (j in (i + 1):length(QIs)) {
      testsForQIiAndQIj <- cbind(Problem = p, agreeTests(dataP, QIs[i], QIs[j]))
      rq2 <- rbind(rq2, testsForQIiAndQIj)
    }
  }
  rq2Problem <- subset(rq2, rq2$Problem == p)

  #rq2.1
  shapOA <- shapiro.test(rq2Problem$OverallAgreement)
  shapK <- shapiro.test(rq2Problem$kTestEst)
  kendallOAvsK <- Kendall(rq2Problem$OverallAgreement, rq2Problem$kTestEst)
  rq2_1_Kendall_Problem <-
    rbind(
      rq2_1_Kendall_Problem,
      data.frame(
        Problem = p,
        OAvsK_tau = kendallOAvsK$tau,
        OAvsK_p = kendallOAvsK$sl,
        shapOA_p = shapOA$p.value,
        shapK_p = shapK$p.value
      )
    )
}
rq2 <- rq2[order(-rq2$OverallAgreement),]
write.table(rq2, file = "results/rq2.txt", sep = "\t", quote = FALSE, row.names = FALSE)

#rq2.1: Kendall's test for each problem
rq2_1_Kendall_Problem$normalOA <- ifelse(rq2_1_Kendall_Problem$shapOA_p < 0.05, "FALSE", "TRUE")
rq2_1_Kendall_Problem$normalK <- ifelse(rq2_1_Kendall_Problem$shapK_p < 0.05, "FALSE", "TRUE")
rq2_1_Kendall_Problem$correlated <- ifelse(rq2_1_Kendall_Problem$OAvsK_p < 0.05, "TRUE", "FALSE")
write.table(rq2_1_Kendall_Problem, file = "results/rq2_1_Kendall_Problem.txt", sep = "\t", quote = FALSE, row.names = TRUE)

#rq2.1: Kruskal-Wallis for checking whether there exist sig. differences among QIs pairs
kruskal.test(rq2$OverallAgreement, rq2$Problem, correct = FALSE) #p-value = 0.00009
kruskal.test(rq2$kTestEst, rq2$Problem, correct = FALSE) #p-value = 2e-08
#same results with correction
#kruskal.test(rq2$OverallAgreement, rq2$Problem, correct = TRUE) #p-value = 0.00009
#kruskal.test(rq2$kTestEst, rq2$Problem, correct = TRUE) #p-value = 2e-08

#rq2.1: "Mann-Whitney U test" and "Vargha and Delaney's A12" to assess the differences
#we do all three tables because there are significant differences (see previous Kruskall-Wallis test)
rq2_1_UtestA12_Prob_OA <- data.frame()
rq2_1_UtestA12_Prob_Kappa <- data.frame()
for (p1 in Problems) {
  p1data <- subset(rq2, rq2$Problem == p1)
  for (p2 in Problems) {
    if (p1 != p2) {
      p2data <- subset(rq2, rq2$Problem == p2)
      rq2_1_UtestA12_Prob_OA <- rbind(rq2_1_UtestA12_Prob_OA, catVDAwilcox(p1data$OverallAgreement, p2data$OverallAgreement, p1, p2))
      rq2_1_UtestA12_Prob_Kappa <- rbind(rq2_1_UtestA12_Prob_Kappa, catVDAwilcox(p1data$kTestEst, p2data$kTestEst, p1, p2))
    }
  }
}
rq2_1_UtestA12_Prob_OA_table <- table(rq2_1_UtestA12_Prob_OA[, c(1, 3)])
rq2_1_UtestA12_Prob_Kappa_table <- table(rq2_1_UtestA12_Prob_Kappa[, c(1, 3)])
write.table(rq2_1_UtestA12_Prob_OA_table, file = "results/rq2_1_UtestA12_Prob_OA_table.txt", sep = "\t", quote = FALSE, row.names = TRUE)
write.table(rq2_1_UtestA12_Prob_Kappa_table, file = "results/rq2_1_UtestA12_Prob_Kappa_table.txt", sep = "\t", quote = FALSE, row.names = TRUE)


#rq2.2
#Kendall for checking whether the two measures are consistent
Kendall(rq2$OverallAgreement, rq2$kTestEst)#tau = 0.855, 2-sided pvalue < 2e-16
#rq2.2 - comparison of each pair of QIs
pdf(file = paste0("plots/rq2_2_OA_QIs", suffix, ".pdf"), width = w, height = h)
par(cex.lab=1.2, cex.axis=1.2)
par(mar = c(5.8, 4, 0.1, 0.1))
boxplot(OverallAgreement ~ q1vsq2, data = rq2, las = 2, ylab = "OA across search problems",
  at = rank(tapply(rq2$OverallAgreement, rq2$q1vsq2, median), ties.method = "first"), ylim = c(0, 1)
)
#title(main = "rq2.2 OverallAgreement~pair of QIs")
dev.off()
pdf(file = paste0("plots/rq2_2_Kappa_QIs", suffix, ".pdf"), width = w, height = h)
par(cex.lab=1.2, cex.axis=1.2)
par(mar = c(5.8, 4, 0.1, 0.1))
boxplot(kTestEst ~ q1vsq2, data = rq2, las = 2, ylab = "Kappa across search problems",
  at = rank(tapply(rq2$kTestEst, rq2$q1vsq2, median), ties.method = "first"), ylim = c(-0.5, 1), yaxt='n'
)
axis(2, at=seq(from = -0.5, to = 1, by = 0.25), las=2)
#title(main = "rq2.2 kTestEst~pair of QIs")
dev.off()
#rq2_2 - Kruskal-Wallis for checking whether there exist sig. differences among QIs pairs
kruskal.test(rq2$OverallAgreement, rq2$q1vsq2)# p-value < 4e-13 - significantly different
kruskal.test(rq2$kTestEst, rq2$q1vsq2)#p-value = 1e-08 - significantly different

#rq2.2 - Fisher's exact test
rq2_2_Fisher <- data.frame()
for (q1q2 in unique(dataDiffStructure$qi1vsqi2)) {
  q1q2Data <- subset(dataDiffStructure, dataDiffStructure$qi1vsqi2 == q1q2)
  fisherTestRes <- fisher.test(q1q2Data$Problem, q1q2Data$Agree, workspace = 2e+09)
  rq2_2_Fisher <- rbind(rq2_2_Fisher, data.frame(QIs = q1q2, fisherP = fisherTestRes$p.value))
}
rq2_2_Fisher$significant <- ifelse(rq2_2_Fisher$fisherP < 0.05, "TRUE", "FALSE")
write.table(rq2_2_Fisher, file = "results/rq2_2_Fisher.txt", sep = "\t", quote = FALSE, row.names = FALSE)



#####################################


#RQ3.1 (28*6=168 rows of pairs of QIs (28) and algs (6))
#given an alg A and a pair of quality indicators QI1 and QI2, compute the number
#of comparisons of A with the other algs (5*11, i.e., other 5 algs in 11 case studies)
#in which QI1 and QI2 agree
rq3_1 <- data.frame()
rq3_1a_Kendall_Alg <- data.frame()
for (a in ALGs) {
  dataA <- subset(data, data$A == a | data$B == a)
  #numExpsPerA <- NROW(dataA)
  for (i in 1:(length(QIs) - 1)) {
    for (j in (i + 1):length(QIs)) {
      testsForQIiAndQIj <- cbind(Alg = a, agreeTests(dataA, QIs[i], QIs[j]))
      rq3_1 <- rbind(rq3_1, testsForQIiAndQIj)
    }
  }
  rq3_1Alg <- subset(rq3_1, rq3_1$Alg == a)

  #rq3.1a
  #for checking the distribution
  shapOA <- shapiro.test(rq3_1Alg$OverallAgreement)
  shapK <- shapiro.test(rq3_1Alg$kTestEst)
  kendallOAvsK <- Kendall(rq3_1Alg$OverallAgreement, rq3_1Alg$kTestEst)
  rq3_1a_Kendall_Alg <-
    rbind(
      rq3_1a_Kendall_Alg,
      data.frame(
        Alg = a,
        OAvsK_tau = kendallOAvsK$tau,
        OAvsK_p = kendallOAvsK$sl,
        shapOA_p = shapOA$p.value,
        shapK_p = shapK$p.value
      )
    )
}
rq3_1 <- rq3_1[order(-rq3_1$OverallAgreement),]
write.table(rq3_1, file = "results/rq3_1.txt", sep = "\t", quote = FALSE, row.names = FALSE)

#rq3_1a
#rq3.1a: Kendall's test for each algorithm
rq3_1a_Kendall_Alg$normalOA <- ifelse(rq3_1a_Kendall_Alg$shapOA_p < 0.05, "FALSE", "TRUE")
rq3_1a_Kendall_Alg$normalK <- ifelse(rq3_1a_Kendall_Alg$shapK_p < 0.05, "FALSE", "TRUE")
rq3_1a_Kendall_Alg$correlated <- ifelse(rq3_1a_Kendall_Alg$OAvsK_p < 0.05, "TRUE", "FALSE")
write.table(rq3_1a_Kendall_Alg, file = "results/rq3_1a_Kendall_Alg.txt", sep = "\t", quote = FALSE, row.names = FALSE)

#rq3_1a - Kruskal-Wallis for checking whether there exist sig. differences among algorithms
kruskal.test(rq3_1$OverallAgreement, rq3_1$Alg) #p-value = 0.2 - not significantly different
kruskal.test(rq3_1$kTestEst, rq3_1$Alg) #p-value = 0.009 - significantly different

#rq3_1a - "Mann-Whitney U test" and "Vargha and Delaney's A12" to assess the differences
#we print the table only for Kappa (the only significant)
rq3_1a_UtestA12_Alg_OA <- data.frame()
rq3_1a_UtestA12_Alg_Kappa <- data.frame()
for (a1 in ALGs) {
  p1data <- subset(rq3_1, rq3_1$Alg == a1)
  for (a2 in ALGs) {
    if (a1 != a2) {
      p2data <- subset(rq3_1, rq3_1$Alg == a2)
      rq3_1a_UtestA12_Alg_OA <- rbind(rq3_1a_UtestA12_Alg_OA, catVDAwilcox(p1data$OverallAgreement, p2data$OverallAgreement, a1, a2))
      rq3_1a_UtestA12_Alg_Kappa <- rbind(rq3_1a_UtestA12_Alg_Kappa, catVDAwilcox(p1data$kTestEst, p2data$kTestEst, a1, a2))
    }
  }
}
rq3_1a_UtestA12_Alg_OA_table <- table(rq3_1a_UtestA12_Alg_OA[, c(1, 3)])#this is not significant for kruskal test (and so not shown in the paper)
rq3_1a_UtestA12_Alg_Kappa_table <- table(rq3_1a_UtestA12_Alg_Kappa[, c(1, 3)])#only this is significant for kruskal test
write.table(rq3_1a_UtestA12_Alg_Kappa_table, file = "results/rq3_1a_UtestA12_Alg_Kappa_table.txt", sep = "\t", quote = FALSE, row.names = TRUE)

#rq3_1a
#Table with counting for Bowker's test (counted if below 0.05)
#Table with 28 rows (pairs of quality indicators) and a column saying how many algs have p-value of Bowker's test below 0.05.
#We use table rq3_1 (single algs)
temp_rq3_1 <- rq3_1
temp_rq3_1$countSigBowker <- ifelse(temp_rq3_1$bowkerP < 0.05,"SIG", "NOT_SIG")
temp_rq3_1$countSigBowker[is.na(temp_rq3_1$countSigBowker)] <- "NA"
rq3_1a_aggrSigBowker <- as.data.frame.matrix(table(temp_rq3_1[,c(2,10)]))
rq3_1a_aggrSigBowker$percSig <- (rq3_1a_aggrSigBowker[,c("SIG")]/(rq3_1a_aggrSigBowker[,c("SIG")] + rq3_1a_aggrSigBowker[,c("NOT_SIG")]))*100
rq3_1a_aggrSigBowker$percNA <- (rq3_1a_aggrSigBowker[,c("NA")]/(rq3_1a_aggrSigBowker[,c("NA")] + rq3_1a_aggrSigBowker[,c("SIG")] + rq3_1a_aggrSigBowker[,c("NOT_SIG")]))*100
write.table(rq3_1a_aggrSigBowker, file = "results/rq3_1a_aggrSigBowker.txt", sep = "\t", quote = FALSE, row.names = TRUE)


#rq3.1b
#Kendall for checking whether the two measures are consistent
Kendall(rq3_1$OverallAgreement, rq3_1$kTestEst)#tau = 0.775, 2-sided pvalue < 2e-16
#rq3_1b - comparison of each pair of QIs
pdf(file = paste0("plots/rq3_1b_OA_QIs", suffix, ".pdf"), width = w, height = h)
par(cex.lab=1.2, cex.axis=1.2)
par(mar = c(5.8, 4, 0.1, 0.1))
boxplot(OverallAgreement ~ q1vsq2, data = rq3_1, las = 2, ylab = "OA across MOSAs",
  at = rank(tapply(rq3_1$OverallAgreement, rq3_1$q1vsq2, median), ties.method = "first"), ylim = c(0, 1)
)
#title(main = "rq3.1b OverallAgreement~pair of QIs")
dev.off()
pdf(file = paste0("plots/rq3_1b_Kappa_QIs", suffix, ".pdf"), width = w, height = h)
par(cex.lab=1.2, cex.axis=1.2)
par(mar = c(5.8, 4, 0.1, 0.1))
boxplot(kTestEst ~ q1vsq2, data = rq3_1, las = 2, ylab = "Kappa across MOSAs",
  at = rank(tapply(rq3_1$kTestEst, rq3_1$q1vsq2, median), ties.method = "first"), ylim = c(-0.5, 1), yaxt='n'
)
axis(2, at=seq(from = -0.5, to = 1, by = 0.25), las=2)
#title(main = "rq3.1b kTestEst~pair of QIs")
dev.off()
#rq3_1b - Kruskal-Wallis for checking whether there exist sig. differences among QIs pairs
kruskal.test(rq3_1$OverallAgreement, rq3_1$q1vsq2)# p-value < 2e-16 - significantly different
kruskal.test(rq3_1$kTestEst, rq3_1$q1vsq2)#p-value = 5e-12 - significantly different

#rq3.1b - Fisher's exact test
rq3_1_b_Fisher <- data.frame()
dataDiffStructureFor_rq3_1_b <- dataDiffStructure
temp <- dataDiffStructure
colnames(temp) <- c("qi1vsqi2", "qi1", "qi2", "Problem", "pairAlgs", "AlgB", "AlgA", "resQI1", "resQI2", "Agree")
temp <- temp[ , c("qi1vsqi2", "qi1", "qi2", "Problem", "pairAlgs", "AlgA", "AlgB", "resQI1", "resQI2", "Agree")]
dataDiffStructureFor_rq3_1_b <- rbind(dataDiffStructureFor_rq3_1_b, temp)
for (q1q2 in unique(dataDiffStructureFor_rq3_1_b$qi1vsqi2)) {
  q1q2Data <- subset(dataDiffStructureFor_rq3_1_b, dataDiffStructureFor_rq3_1_b$qi1vsqi2 == q1q2)
  fisherTestRes <- fisher.test(q1q2Data$AlgA, q1q2Data$Agree, workspace = 2e+09)
  rq3_1_b_Fisher <- rbind(rq3_1_b_Fisher, data.frame(QIs = q1q2, fisherP = fisherTestRes$p.value))
}
rq3_1_b_Fisher$significant <- ifelse(rq3_1_b_Fisher$fisherP < 0.05, "TRUE", "FALSE")
write.table(rq3_1_b_Fisher, file = "results/rq3_1_b_Fisher.txt", sep = "\t", quote = FALSE, row.names = FALSE)






#RQ3.2 (28*15=420 rows)
#pairs of algs and pairs of QIs
rq3_2 <- data.frame()
for (ca in unique(data$pairAlgs)) {
  dataCA <- subset(data, data$pairAlgs == ca)
  for (i in 1:(length(QIs) - 1)) {
    for (j in (i + 1):length(QIs)) {
      testsForQIiAndQIj <- cbind(pairAlgs = ca, agreeTests(dataCA, QIs[i], QIs[j]))
      rq3_2 <- rbind(rq3_2, testsForQIiAndQIj)
    }
  }
}
write.table(rq3_2, file = "results/rq3_2.txt", sep = "\t", quote = FALSE, row.names = FALSE)

#Kendall for checking whether the two measures are consistent
Kendall(rq3_2$OverallAgreement, rq3_2$kTestEst)#tau = 0.477, 2-sided pvalue < 2e-16

#rq3_2 - comparison of each pair of QIs
pdf(file = paste0("plots/rq3_2_OA_QIs", suffix, ".pdf"), width = w, height = h)
par(cex.lab=1.2, cex.axis=1.2)
par(mar = c(5.8, 4, 0.1, 0.1))
boxplot(OverallAgreement ~ q1vsq2, data = rq3_2, las = 2, ylab = "OA across pairs of MOSAs",
  at = rank(tapply(rq3_2$OverallAgreement, rq3_2$q1vsq2, median), ties.method = "first"), ylim = c(0, 1)
)
#title(main = "RQ3.2 OverallAgreement~pair of QIs (pairs of MOSAs)")
dev.off()
pdf(file = paste0("plots/rq3_2_Kappa_QIs", suffix, ".pdf"), width = w, height = h)
par(cex.lab=1.2, cex.axis=1.2)
par(mar = c(5.8, 4, 0.1, 0.1))
boxplot(kTestEst ~ q1vsq2, data = rq3_2, las = 2, ylab = "Kappa across pairs of MOSAs",
  at = rank(tapply(rq3_2$kTestEst, rq3_2$q1vsq2, median), ties.method = "first"), ylim = c(-0.5, 1), yaxt='n'
)
axis(2, at=seq(from = -0.5, to = 1, by = 0.25), las=2)
#title(main = "RQ3.2 kTestEst~pair of QIs (pairs of MOSAs)")
dev.off()
#rq3_2 - Kruskall-Wallis for checking whether there exist sig. differences among QIs pairs
kruskal.test(rq3_2$OverallAgreement, rq3_2$q1vsq2)# p-value < 2e-16 - significantly different
kruskal.test(rq3_2$kTestEst, rq3_2$q1vsq2)#p-value = 5e-13 - significantly different

rq3_2_Fisher <- data.frame()
for (q1q2 in unique(dataDiffStructure$qi1vsqi2)) {
  q1q2Data <- subset(dataDiffStructure, dataDiffStructure$qi1vsqi2 == q1q2)
  fisherTestRes <- fisher.test(q1q2Data$pairAlgs, q1q2Data$Agree, workspace = 2e+09)
  rq3_2_Fisher <- rbind(rq3_2_Fisher, data.frame(QIs = q1q2, fisherP = fisherTestRes$p.value))
}
rq3_2_Fisher$significant <- ifelse(rq3_2_Fisher$fisherP < 0.05, "TRUE", "FALSE")
write.table(rq3_2_Fisher, file = "results/rq3_2_Fisher.txt", sep = "\t", quote = FALSE, row.names = FALSE)

#############################################
