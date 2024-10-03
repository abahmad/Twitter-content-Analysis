## Installs all the libraries that are needed
#if(!require(foreign)){install.packages('foreign')} # for opening SPSS files
if(!require(readxl)){install.packages('readxl')} # for opening Excel XSLX files
if(!require(psych)){install.packages('psych')} # for Cohen's Kappa
if(!require(rel)){install.packages('rel')} # for Gwet's AC
if(!require(irr)){install.packages('irr')} # for Percentage Agreement
#library(foreign)
library(readxl)
library(psych)
library(rel)
library(irr)



setwd("C:/Users/abuba/Desktop/thesis/R/Reliability/RTPMO/PRETEST")
getwd()

## Reads the dataset for the calculation of the inter-coder-reliabilities
dataset <- read_xlsx("intercoder.xlsx") # opens data file in XLSX format


#dataset <- read.csv2("intercoder.csv", header = TRUE) # opens data file in CSV format



Informative<-subset(dataset, select = c(Informative, Informative1))
RHE<-subset(dataset, select = c(Rhetoric, Rhetoric1))
EMC<-subset(dataset, select = c(Emotionally_charged, Emotionally_charged1))
POlD<-subset(dataset, select = c(Political_Discussion, Political_Discussion1))
DSI<-subset(dataset, select = c(Disinformation, Disinformation1))
DSC<-subset(dataset, select = c(Discredit, Discredit1))
FA<-subset(dataset, select = c(False_analogy, False_analogy1))
BG<-subset(dataset, select = c(Blame_game, Blame_game1))
ADC<-subset(dataset, select = c(advocacy_campaign, advocacy_campaign1))
MP<-subset(dataset, select = c(Military_provocations, Military_provocations1))
DAP<-subset(dataset, select = c(Domestic_approach, Domestic_approach1))
IAP<-subset(dataset, select = c(International_approach, International_approach1))
VM<-subset(dataset, select = c(Voter_mobilization, Voter_mobilization1))
ELE<-subset(dataset, select = c(Election_as_an_event, Election_as_an_event1))
EPUL<-subset(dataset, select = c(Linking_election_with_Pulwama, Linking_election_with_Pulwama1))
ENT<-subset(dataset, select = c(Entice, Entice1))
ENG<-subset(dataset, select = c(Engage, Engage1))
ELV<-subset(dataset, select = c(Elevate, Elevate1))
EXPL<-subset(dataset, select = c(Exploit, Exploit1))







agree(Informative) #1
agree(RHE) #2
agree(EMC) #3
agree(POlD) #4
agree(DSI) #5
agree(DSC) #6
agree(FA) #7
agree(BG) #9
agree(ADC) #10
agree(MP) #11
agree(DAP) #12
agree(IAP) #13
agree(VM) #14
agree(ELE) #15
agree(EPUL) #16
agree(ENT) #17
agree(ENG) #18
agree(ELV) #19
agree(EXPL) #20

sink("result.doc")
kappa2(Informative) #1
kappa2(RHE) #2
kappa2(EMC) #3
kappa2(POlD) #4
kappa2(DSI) #5
kappa2(DSC) #6
kappa2(FA) #7
kappa2(BG) #9
kappa2(ADC) #10
kappa2(MP) #11
kappa2(DAP) #12
kappa2(IAP) #13
kappa2(VM) #14
kappa2(ELE) #15
kappa2(EPUL) #16
kappa2(ENT) #17
kappa2(ENG) #18
kappa2(ELV) #19
kappa2(EXPL) #20
sink()

#gac(Informative) #1
#gac(RHE) #2
#gac(EMC) #3
#gac(POlD) #4
#gac(DSI) #5
#gac(DSC) #6
#gac(FA) #7
#gac(BG) #9
#gac(ADC) #10
#gac(MP) #11
#gac(DAP) #12
#gac(IAP) #13
#gac(VM) #14
#gac(ELE) #15
#gac(EPUL) #16
#gac(ENT) #17
#gac(ENG) #18
#gac(ELV) #19
#gac(EXPL) #20

