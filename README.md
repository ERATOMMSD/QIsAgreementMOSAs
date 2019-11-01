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
[csv file](https://github.com/ERATOMMSD/QIsAgreementMOSAs/blob/master/statisticalTests/inputData/inputData.csv) using the format
   ```
   CaseStudy;A;B;HV;IGD;EP;GD;GS;ED;PFS;C
   CaseStudyName;MOSA1;MOSA2;B;B;B;A;A;A;A;ND
   ```
   where the first line is the header of the file, and the second line is an example of entry; *CaseStudyName* is the name of the search problem, *MOSA1* and *MOSA2* the names of the two compared MOSAs (*A* and *B* in the header), and the remaining columns tell whether a given quality indicator (reported in the table header) prefers *A* (i.e., *MOSA1*), *B* (i.e., *MOSA2*), or there is no difference (*ND*) for the indicator. 

* The observations can be generated using the tool GenObs provided as jar file: [GenObs.jar](https://github.com/ERATOMMSD/QIsAgreementMOSAs/blob/master/code/generatorReprSets/GenObs.jar?raw=true)
  * The tool must me invoked as follows:
     ```
     java -jar GenObs.jar TypeOfMOSA TypeOfObservation
     - TypeOfMOSA: all, single, or pairs
     - TypeOfObservation: SR, or MRS
     ```

## How to contribute to the project
Our observations are based on 11 search problems, taken from different domains.
Should the paper be accepted, we will make them publicly available (in any case, they can be already produced with the tool).

We will also actively update the observations when more data are available and analyzed.
In order to enlarge the evaluation, we encourage researchers to send us data from their experiments, so that we can add it to our repository and so obtain a more comprehensive evaluation. In order to do this, you should send us a link to a csv file having the structure described in the Tool section at the address arcaini@nii.ac.jp
Note that, at the current stage, we do not provide the computation of all the quality indicators and so you should compute the values of the quality indicators of your interest (i.e., step A1), and assess which MOSA is preferred by each of them (i.e., step A2). Should you not have such information, we still encourage you to contact us describing which kind of data you have.


## People
* Shaukat Ali https://www.simula.no/people/shaukat
* Paolo Arcaini http://group-mmm.org/~arcaini/
* Dipesh Pradhan https://www.simula.no/people/dipeshpr
* Safdar Aqeel Safdar https://www.simula.no/people/safdar
* Tao Yue https://www.simula.no/people/tao
