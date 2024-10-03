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
if(!require(tidyverse)){install.packages('tidyverse')} # for opening SPSS files
if(!require(xlsx)){install.packages('xlsx')} # for opening SPSS files
## Installs all the libraries that are needed
if(!require(foreign)){install.packages('foreign')} # for opening SPSS files
if(!require(readxl)){install.packages('readxl')} # for opening Excel XSLX files
if(!require(MASS)){install.packages('MASS')} # for Chi Square
if(!require(DescTools)){install.packages('DescTools')}# for Cramers V
if(!require(gmodels)){install.packages('gmodels')}# for Crosstabulation
if(!require(expss)){install.packages('expss')} # for labels
#library(foreign)
library(readxl)
library(MASS)
library(DescTools)
library(gmodels)
library(expss)
if(!require(tidyverse)){install.packages('tidyverse')}
if(!require(readxl)){install.packages('readxl')}
if(!require(DescTools)){install.packages('DescTools')}
if(!require(sjPlot)){install.packages('sjPlot')}
library(tidyverse)
library(readxl)
library(DescTools)
library(sjPlot)


#Change the directory between "" to run the file

#Setting the Directory
setwd("C:/Users/abuba/Desktop/coding")
getwd()

## Reads the dataset for the calculation of the inter-coder-reliabilities
dataset <- read_xlsx("Final Coding Sheet.xlsx") # opens data file in XLSX format

#Changing the Name of the Dataset
mydata<- dataset

#Preparing Data for Cross Tabulation and CHI SQ test

### Research Q1
# To what extent do the tweets of Indian public diplomacy stakeholders reflect
# their approach in context with the Pulwama event, i.e., political discussion, 
# informative, rhetoric, and emotionally charged?) 

## label the variables' value labels
## not neccessary, but we can get more readable frequencies tables

#Getting the frequencies of all the coded comments in their particular categories.

## To Exclude NA values change "always" to "No"

val_lab(mydata$Account) = make_labels("
                                        1: @narendramod 
                                        2: @PMOIndia 
                                        3: @SushmaSwaraj 
                                        4: @MEAIndia 
                                        5: @nsitharaman
                                        6: @adgpi
                                      ")

val_lab(mydata$`Political Discussion`) = make_labels("
                                                    0: no use 
                                                    1: negative
                                                    2: neutral 
                                                    3: positive 
                                                    99: cannot distinguish
                                                     ")
PD_total <- table (mydata$`Political Discussion` , useNA = "always")
PD_percent <- round(prop.table(PD_total)*100, 1)
PD_cumpercent <- cumsum (PD_percent)
cbind (PD_total, PD_percent, PD_cumpercent)


#Checking the number of cases in every Stakeholder's case 
AccountxPD <- sjt.xtab(mydata$Account, mydata$`Political Discussion`,
                       statistics = "fisher",
                       show.exp = FALSE,
                       show.row.prc = TRUE,
                       show.col.prc = FALSE) #Calculating Crosstabs

AccountxPD


val_lab(mydata$Informative) = make_labels("
                                        0 No use of informative content
                                        1 Use of informative content 
                                         ")

In_total <- table (mydata$Informative , useNA = "always")
In_percent <- round(prop.table(In_total)*100, 1)
In_cumpercent <- cumsum (In_percent)
cbind (In_total, In_percent, In_cumpercent)

#Checking the number of cases in every stakeholder case (Informative)
AccountxInformative <- sjt.xtab(mydata$Account, mydata$Informative,
                                statistics = "fisher",
                                show.exp = FALSE,
                                show.row.prc = TRUE,
                                show.col.prc = FALSE) #Calculating Crosstabs
#To print result
AccountxInformative


val_lab(mydata$Rhetoric) = make_labels("
                                        0 No use of rhetoric content 
                                        1 Use of Rhetoric content
                                        ")

RH_total <- table (mydata$Rhetoric , useNA = "always")
RH_percent <- round(prop.table(RH_total)*100, 1)
RH_cumpercent <- cumsum (RH_percent)
cbind (RH_total, RH_percent, RH_cumpercent)


#Checking the number of cases in every stakeholder's case (Rhetoric) 
AccountxRhetoric <- sjt.xtab(mydata$Account, mydata$Rhetoric,
                             statistics = "fisher",
                             show.exp = FALSE,
                             show.row.prc = TRUE,
                             show.col.prc = FALSE) #Calculating Crosstabs

AccountxRhetoric




val_lab(mydata$`Emotionally-charged`) = make_labels("
                                        0 No use of emotions 
                                        1 Expressing Anger 
                                        2 Expressing frustration 
                                        3 Expressing disappointment 
                                        4 Expressing Sadness                
                                                    ")

EM_total <- table (mydata$`Emotionally-charged`, useNA = "always")
EM_percent <- round(prop.table(EM_total)*100, 1)
EM_cumpercent <- cumsum (EM_percent)
cbind (EM_total, EM_percent, EM_cumpercent)


AccountxEM <- sjt.xtab(mydata$Account, mydata$`Emotionally-charged`,
                       statistics = "fisher",
                       show.exp = FALSE,
                       show.row.prc = TRUE,
                       show.col.prc = FALSE) #Calculating Crosstabs

AccountxEM


AccountxMP <- sjt.xtab(mydata$`Military provocations`, mydata$`Emotionally-charged`,
                       statistics = "fisher",
                       show.exp = FALSE,
                       show.row.prc = TRUE,
                       show.col.prc = FALSE) #Calculating Crosstabs

AccountxMP



### Research Q2  (Propaganda)
#	To what extent the content of Twitter messages of public diplomacy stakeholders
# implied propaganda tactics, i.e., disinformation, discrediting, false analogy, and 
# used blame game for targeting the political opponent and enemy?


val_lab(mydata$Disinformation) = make_labels("
                                        0 No Disinfo 
                                        1 Disinfo")


DIS_total <- table (mydata$Disinformation, useNA = "always")
DIS_percent <- round(prop.table(DIS_total)*100, 1)
DIS_cumpercent <- cumsum (DIS_percent)
cbind (DIS_total, DIS_percent, DIS_cumpercent)


AccountxDIS <- sjt.xtab(mydata$Account, mydata$Disinformation,
                       statistics = "fisher",
                       show.exp = FALSE,
                       show.row.prc = TRUE,
                       show.col.prc = FALSE) #Calculating Crosstabs

AccountxDIS


###Percentage of discredit tweets

val_lab(mydata$Discredit) = make_labels("
                                        0 No Discredit 
                                        1 Discredit")

DC_total <- table (mydata$Discredit, useNA = "always")
DC_percent <- round(prop.table(DC_total)*100, 1)
DC_cumpercent <- cumsum (DC_percent)
cbind (DC_total, DC_percent, DC_cumpercent)


AccountxDC <- sjt.xtab(mydata$Account, mydata$Discredit,
                       statistics = "fisher",
                       show.exp = FALSE,
                       show.row.prc = TRUE,
                       show.col.prc = FALSE) #Calculating Crosstabs

AccountxDC


val_lab(mydata$`False analogy`) = make_labels("
                                        0 No linkage of Pulwama with past event  
                                        1 linkage of Pulwama with past event
                                        ")

FA_total <- table (mydata$`False analogy`, useNA = "always")
FA_percent <- round(prop.table(FA_total)*100, 1)
FA_cumpercent <- cumsum (FA_percent)
cbind (FA_total, FA_percent, FA_cumpercent)


AccountxFA <- sjt.xtab(mydata$Account, mydata$`False analogy`,
                       statistics = "fisher",
                       show.exp = FALSE,
                       show.row.prc = TRUE,
                       show.col.prc = FALSE) #Calculating Crosstabs

AccountxFA

val_lab(mydata$`Blame game`) = make_labels("
                                        0 No Blame game  
                                        1 Blame game
                                        ")

BG_total <- table (mydata$`Blame game`, useNA = "always")
BG_percent <- round(prop.table(BG_total)*100, 1)
BG_cumpercent <- cumsum (BG_percent)
cbind (BG_total, BG_percent, BG_cumpercent)


AccountxBG <- sjt.xtab(mydata$Account, mydata$`Blame game`,
                       statistics = "fisher",
                       show.exp = FALSE,
                       show.row.prc = TRUE,
                       show.col.prc = FALSE) #Calculating Crosstabs

AccountxBG


### Research Q3  (Public diplomacy)

#	To what extent do public diplomacy stakeholders disseminate their foreign 
# policy agenda in context with the Pulwama event both at domestic and 
# international platforms? 

val_lab(mydata$`advocacy campaign`) = make_labels("
                                        0 No Advocacy  
                                        1 Advocacy
                                        ")

AC_total <- table (mydata$`advocacy campaign`, useNA = "always")
AC_percent <- round(prop.table(AC_total)*100, 1)
AC_cumpercent <- cumsum (AC_percent)
cbind (AC_total, AC_percent, AC_cumpercent)


AccountxAC <- sjt.xtab(mydata$Account, mydata$`advocacy campaign`,
                       statistics = "fisher",
                       show.exp = FALSE,
                       show.row.prc = TRUE,
                       show.col.prc = FALSE) #Calculating Crosstabs

AccountxAC



val_lab(mydata$`Military provocations`) = make_labels("
                                        0 No provocations  
                                        1 provocations
                                        ")

MP_total <- table (mydata$`Military provocations`, useNA = "always")
MP_percent <- round(prop.table(MP_total)*100, 1)
MP_cumpercent <- cumsum (MP_percent)
cbind (MP_total, MP_percent, MP_cumpercent)


AccountxMP <- sjt.xtab(mydata$Account, mydata$`Military provocations`,
                       statistics = "fisher",
                       show.exp = FALSE,
                       show.row.prc = TRUE,
                       show.col.prc = FALSE) #Calculating Crosstabs

AccountxMP


val_lab(mydata$`Domestic approach`) = make_labels("
                                        0 No Domestic approach  
                                        1 Domestic approach
                                        ")

DA_total <- table (mydata$`Domestic approach`, useNA = "always")
DA_percent <- round(prop.table(DA_total)*100, 1)
DA_cumpercent <- cumsum (DA_percent)
cbind (DA_total, DA_percent, DA_cumpercent)


AccountxDA <- sjt.xtab(mydata$Account, mydata$`Domestic approach`,
                       statistics = "fisher",
                       show.exp = FALSE,
                       show.row.prc = TRUE,
                       show.col.prc = FALSE) #Calculating Crosstabs

AccountxDA



val_lab(mydata$`International approach`) = make_labels("
                                        0 No International approach  
                                        1 International approach
                                        ")

IA_total <- table (mydata$`International approach`, useNA = "always")
IA_percent <- round(prop.table(IA_total)*100, 1)
IA_cumpercent <- cumsum (IA_percent)
cbind (IA_total, IA_percent, IA_cumpercent)


AccountxIA <- sjt.xtab(mydata$Account, mydata$`International approach`,
                       statistics = "fisher",
                       show.exp = FALSE,
                       show.row.prc = TRUE,
                       show.col.prc = FALSE) #Calculating Crosstabs

AccountxIA



### Rsearch Q4 (Elections 2019)  
#	To what extent Pulwama event was used for voter mobilization by public diplomacy 
# stakeholders during election campaigns?



val_lab(mydata$`Voter mobilization`) = make_labels("
                                        0 No Motivation for Vote   
                                        1 Motivation for Vote
                                        ")

VM_total <- table (mydata$`Voter mobilization`, useNA = "always")
VM_percent <- round(prop.table(VM_total)*100, 1)
VM_cumpercent <- cumsum (VM_percent)
cbind (VM_total, VM_percent, VM_cumpercent)


AccountxVM <- sjt.xtab(mydata$Account, mydata$`Voter mobilization`,
                       statistics = "fisher",
                       show.exp = FALSE,
                       show.row.prc = TRUE,
                       show.col.prc = FALSE) #Calculating Crosstabs

AccountxVM


val_lab(mydata$`Election as an event`) = make_labels("
                                        0 No Electoral discussion    
                                        1 Electoral discussion 
                                        ")

ED_total <- table (mydata$`Election as an event`, useNA = "always")
ED_percent <- round(prop.table(ED_total)*100, 1)
ED_cumpercent <- cumsum (ED_percent)
cbind (ED_total, ED_percent, ED_cumpercent)


AccountxED <- sjt.xtab(mydata$Account, mydata$`Election as an event`,
                       statistics = "fisher",
                       show.exp = FALSE,
                       show.row.prc = TRUE,
                       show.col.prc = FALSE) #Calculating Crosstabs

AccountxED


###To what extent audience engagement varies depending on the content of messages by Indian public diplomacy stakeholders ?

MydataPD3<- mydata[mydata$`Political Discussion` == '0',]


PD4 <- MydataPD3[c(14:17)]
sum(PD4$Favourites)
sum(PD4$Replies)
sum(PD4$Retweets)
mean(PD4$Favourites)
mean(PD4$Retweets)
mean(PD4$Replies)


MydataPD<- mydata[mydata$`Political Discussion` == '1',]


PD1 <- MydataPD[c(14:17)]
sum(PD1$Favourites)
sum(PD1$Replies)
sum(PD1$Retweets)
mean(PD1$Favourites)
mean(PD1$Retweets)
mean(PD1$Replies)


MydataPD1<- mydata[mydata$`Political Discussion` == '2',]


PD2 <- MydataPD1[c(14:17)]
sum(PD2$Favourites)
sum(PD2$Replies)
sum(PD2$Retweets)
mean(PD2$Favourites)
mean(PD2$Retweets)
mean(PD2$Replies)


MydataPD2<- mydata[mydata$`Political Discussion` == '3',]


PD3 <- MydataPD2[c(14:17)]
sum(PD3$Favourites)
sum(PD3$Replies)
sum(PD3$Retweets)
mean(PD3$Favourites)
mean(PD3$Retweets)
mean(PD3$Replies)



Mydatainfo0<- mydata[mydata$Informative == '0',]


info0 <- Mydatainfo0[c(14:16,18)]
sum(info0$Favourites)
sum(info0$Replies)
sum(info0$Retweets)
mean(info0$Favourites)
mean(info0$Retweets)
mean(info0$Replies)



Mydatainfo1<- mydata[mydata$Informative == '1',]


info1 <- Mydatainfo1[c(14:16,18)]
sum(info1$Favourites)
sum(info1$Replies)
sum(info1$Retweets)

mean(info1$Favourites)
mean(info1$Retweets)
mean(info1$Replies)



MydataRhetoric0<- mydata[mydata$Rhetoric == '0',]


Rhetoric0 <- MydataRhetoric0[c(14:16,19)]

mean(Rhetoric0$Favourites)
mean(Rhetoric0$Retweets)
mean(Rhetoric0$Replies)



MydataRhetoric1<- mydata[mydata$Rhetoric == '1',]


Rhetoric1 <- MydataRhetoric1[c(14:16,19)]

mean(Rhetoric1$Favourites)
mean(Rhetoric1$Retweets)
mean(Rhetoric1$Replies)



MydataEMC0<- mydata[mydata$`Emotionally-charged` == '0',]


EMC0 <- MydataEMC0[c(14:16,20)]

mean(EMC0$Favourites)
mean(EMC0$Retweets)
mean(EMC0$Replies)



MydataEMC1<- mydata[mydata$`Emotionally-charged` == '1',]


EMC1 <- MydataEMC1[c(14:16,20)]

mean(EMC1$Favourites)
mean(EMC1$Retweets)
mean(EMC1$Replies)


MydataEMC2<- mydata[mydata$`Emotionally-charged` == '2',]


EMC2 <- MydataEMC2[c(14:16,20)]

mean(EMC2$Favourites)
mean(EMC2$Retweets)
mean(EMC2$Replies)



MydataEMC3<- mydata[mydata$`Emotionally-charged` == '3',]


EMC3 <- MydataEMC3[c(14:16,20)]

mean(EMC3$Favourites)
mean(EMC3$Retweets)
mean(EMC3$Replies)


MydataEMC4<- mydata[mydata$`Emotionally-charged` == '4',]


EMC4 <- MydataEMC4[c(14:16,20)]

mean(EMC4$Favourites)
mean(EMC4$Retweets)
mean(EMC4$Replies)


MydataDisinf0<- mydata[mydata$Disinformation == '0',]


Disinf0 <- MydataDisinf0[c(14:16,21)]

mean(Disinf0$Favourites)
mean(Disinf0$Retweets)
mean(Disinf0$Replies)



MydataDisinf1<- mydata[mydata$Disinformation == '1',]


Disinf1 <- MydataDisinf1[c(14:16,21)]

mean(Disinf1$Favourites)
mean(Disinf1$Retweets)
mean(Disinf1$Replies)


MydataDisc0<- mydata[mydata$Discredit == '0',]


Disc0 <- MydataDisc0[c(14:16,22)]

mean(Disc0$Favourites)
mean(Disc0$Retweets)
mean(Disc0$Replies)



MydataDisc1<- mydata[mydata$Discredit == '1',]


Disc1 <- MydataDisc1[c(14:16,22)]

mean(Disc1$Favourites)
mean(Disc1$Retweets)
mean(Disc1$Replies)



MydataFA0<- mydata[mydata$`False analogy` == '0',]


FA0 <- MydataFA0[c(14:16,23)]

mean(FA0$Favourites)
mean(FA0$Retweets)
mean(FA0$Replies)



MydataFA1<- mydata[mydata$`False analogy` == '1',]


FA1 <- MydataFA1[c(14:16,23)]

mean(FA1$Favourites)
mean(FA1$Retweets)
mean(FA1$Replies)


MydataBG0<- mydata[mydata$`Blame game` == '0',]


BG0 <- MydataBG0[c(14:16,24)]

mean(BG0$Favourites)
mean(BG0$Retweets)
mean(BG0$Replies)



MydataBG1<- mydata[mydata$`Blame game` == '1',]


BG1 <- MydataBG1[c(14:16,24)]

mean(BG1$Favourites)
mean(BG1$Retweets)
mean(BG1$Replies)


MydataAC0<- mydata[mydata$`advocacy campaign` == '0',]


AC0 <- MydataAC0[c(14:16,25)]

mean(AC0$Favourites)
mean(AC0$Retweets)
mean(AC0$Replies)



MydataAC1<- mydata[mydata$`advocacy campaign` == '1',]


AC1 <- MydataAC1[c(14:16,25)]

mean(AC1$Favourites)
mean(AC1$Retweets)
mean(AC1$Replies)


MydataMP0<- mydata[mydata$`Military provocations` == '0',]


MP0 <- MydataMP0[c(14:16,26)]

mean(MP0$Favourites)
mean(MP0$Retweets)
mean(MP0$Replies)



MydataMP1<- mydata[mydata$`Military provocations` == '1',]


MP1 <- MydataMP1[c(14:16,26)]

mean(MP1$Favourites)
mean(MP1$Retweets)
mean(MP1$Replies)



MydataDA0<- mydata[mydata$`Domestic approach` == '0',]


DA0 <- MydataDA0[c(14:16,27)]

mean(DA0$Favourites)
mean(DA0$Retweets)
mean(DA0$Replies)



MydataDA1<- mydata[mydata$`Domestic approach`== '1',]


DA1 <- MydataDA1[c(14:16,27)]

mean(DA1$Favourites)
mean(DA1$Retweets)
mean(DA1$Replies)


MydataIntlA0<- mydata[mydata$`International approach` == '0',]


IntlA0 <- MydataIntlA0[c(14:16,28)]

mean(IntlA0$Favourites)
mean(IntlA0$Retweets)
mean(IntlA0$Replies)



MydataIntlA1<- mydata[mydata$`International approach` == '1',]


IntlA1 <- MydataIntlA1[c(14:16,28)]

mean(IntlA1$Favourites)
mean(IntlA1$Retweets)
mean(IntlA1$Replies)


MydataVM0<- mydata[mydata$`Voter mobilization`== '0',]


VM0 <- MydataVM0[c(14:16,29)]

mean(VM0$Favourites)
mean(VM0$Retweets)
mean(VM0$Replies)



MydataVM1<- mydata[mydata$`Voter mobilization`== '1',]


VM1 <- MydataVM1[c(14:16,29)]

mean(VM1$Favourites)
mean(VM1$Retweets)
mean(VM1$Replies)


MydataEA0<- mydata[mydata$`Election as an event`== '0',]


EA0 <- MydataEA0[c(14:16,30)]

mean(EA0$Favourites)
mean(EA0$Retweets)
mean(EA0$Replies)



MydataEA1<- mydata[mydata$`Election as an event` == '1',]


EA1 <- MydataEA1[c(14:16,28)]

mean(EA1$Favourites)
mean(EA1$Retweets)
mean(EA1$Replies)
