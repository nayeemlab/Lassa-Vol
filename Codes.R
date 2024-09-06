library(dplyr)
library("FactoMineR")
library("factoextra")
library(extrafont)
library(ggplot2)
library(pastecs)
library(corrplot)
library(ppcor)
library(factoextra)
library(psych)
library(GPArotation)
library(Hmisc)
library(dplyr)
library(ape)
library(psych)
library(psychometric)

setwd('E:\\ResearchProject\\Najmul Bhai\\Lassa')
LsData <- read.csv("Merge.csv")

library(pastecs)
LsData$urea..mg.dl.
LsData$urea..mg.dl.Cat[LsData$urea..mg.dl. < 100]  = 0
LsData$urea..mg.dl.Cat[LsData$urea..mg.dl. >= 100] = 1

LsData$urea..mg.dl.Cat <- factor(LsData$urea..mg.dl.Cat,levels=c(0,1),labels = c('Moderate','Severe'))
LsData$urea..mg.dl.Cat


LsData$SEX
stat.desc(LsData$AGE..Years.)
describe.by(LsData$AGE..Years., LsData$Case)
describe.by(LsData$Selenium..ng.ml., LsData$Case)
describe.by(LsData$CRP...mg.l., LsData$Case)

options(scipen = 999)
model <- glm(urea..mg.dl.Cat ~ SEX + AGE..Years. + Selenium..ng.ml. + CRP...mg.l.,family = "binomial", data = LsData)

summary(model)
round(exp(cbind(coef(model), confint(model))),2)
