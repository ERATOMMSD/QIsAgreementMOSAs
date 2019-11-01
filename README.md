# Insights on Quality Indicators in Search-Based Software Engineering: An Empirical Evaluation

## Description
Search-Based Software Engineering (SBSE) researchers who apply multi-objective search algorithms (MOSAs) assess the quality of solutions produced by MOSAs with quality indicators (QIs). Given the fact that SBSE researchers typically apply MOSAs from the evolutionary computation domain to address software engineering problems, they have limited knowledge about QIs and their relations with SBSE problems and MOSAs. To this end, we conducted an extensive empirical evaluation to provide insights on commonly used QIs in SBSE by studying agreements among them when comparing MOSAs, with and without considering differences of SBSE problems and MOSAs. In addition, by defining a systematic process based on three common ways of comparing MOSAs, we produced additional observations based on the results of our empirical evaluation. These observations can be used by SBSE researchers to gain a better understanding of the commonly used QIs in SBSE and their agreements.

## Process of the performed analyses
![Process](https://github.com/ERATOMMSD/QIsAgreementMOSAs/blob/master/statisticalTests/StatTestsProcess.png)

## Repository content
This repository contains: 
* Raw data about agreement results (result of step A2)
* R script for statistical analysis of the raw agreement data (for performing steps A3-A8)
* Results of statistical analysis
* Java code for producing *Single Representative* (SR) and *Minimum Representative Set* (MRS) for:
  * *All*: without any specification of MOSA or SBSE problem characteristics
  * *SingleAlgorithm*: for one specific MOSA
  * *PairAlgorithms*: for a given pair of MOSAs
  
## Tool
* In order to perform new statistical analyses on new data, a user can run the R script [computeAgreementQIs.R](https://github.com/ERATOMMSD/QIsAgreementMOSAs/blob/master/statisticalTests/computeAgreementQIs.R). Input data must be added to this 
[csv file](https://github.com/ERATOMMSD/QIsAgreementMOSAs/blob/master/statisticalTests/inputData/inputData.csv) using

The observations can be generated using the tool GenObs provided as jar file: [GenObs.jar](https://github.com/ERATOMMSD/QIsAgreementMOSAs/tree/master/code/generatorReprSets/GenObs.jar)

## People
* Shaukat Ali https://www.simula.no/people/shaukat
* Paolo Arcaini http://group-mmm.org/~arcaini/
* Dipesh Pradhan https://www.simula.no/people/dipeshpr
* Safdar Aqeel Safdar https://www.simula.no/people/safdar
* Tao Yue https://www.simula.no/people/tao
