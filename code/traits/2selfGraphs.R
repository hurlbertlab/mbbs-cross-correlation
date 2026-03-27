# Relevant Graphs
library(ggplot2)
library(tidyverse)
library(patchwork)
library(ggeffects)
library(gtsummary)
library(beepr)

mbbs <- read.csv("data/traits/mbbsTraitsAndCorr.csv")|> 
  filter(corr !=1)
cbc <- read.csv("data/traits/CBCTraitsAndCorr.csv")|> 
  filter(corr !=1)

mbbsMatrix <- read.csv("data/corrMatrices/subsets/mBBSSubset.csv")
cbcMatrix <- read.csv("data/corrMatrices/subsets/CBCSubset.csv")

cbcHeat <- read.csv("data/CBCHistoricData/CBCMERGEDDeltaY.csv") |> select(-year)
mbbsHeat <- read.csv("data/mbbs/mbbsDeltaYWide.csv")|> select(-year)

# Yay GLM!
listOfData <- list(mbbs, cbc)
listOfMatrices <- list(mbbsMatrix, cbcMatrix)
listOfHeat <- list( mbbsHeat, mbbsHeat)
seasons <- c("mBBS", "Christmas Bird Count")
intersections <- intersect(names(cbcHeat), names(mbbsHeat))

literature <- c("dif_mass", "dif_mig", "dif_trophicLvl", "dif_HB")
importantRF <- c("dif_wingLen", "dif_KippsDist", "dif_mass", "dif_Secondary")
randomInterests <- c("dif_mig", "dif_mass", "dif_HB", "dif_clutchMax")
newVars <- c("dif_mass","dif_migDist", "dif_arth", "dif_trophicLvl")
interest <- newVars
survey <- mbbs
formula <- paste("corr ~", paste(interest, collapse = " + "))

glm_model <- glm(formula, data = survey, family="gaussian")
summary(glm_model)

p1 <- as.data.frame(ggpredict(glm_model, terms = interest[1]))
p2 <- as.data.frame(ggpredict(glm_model, terms = interest[2]))
p3 <- as.data.frame(ggpredict(glm_model, terms = interest[3]))
p4 <- as.data.frame(ggpredict(glm_model, terms = interest[4]))

q1 <- ggplot(p1, aes(x = x, y = predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15) +
  ylim(min(survey$corr), max(survey$corr))

q2 <- ggplot(p2, aes(x = x, y = predicted)) +
  geom_line(linetype = 2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15) +
  ylim(min(survey$corr), max(survey$corr))

q3 <- ggplot(p3, aes(x = x, y = predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15) +
  ylim(min(survey$corr), max(survey$corr))

q4 <- ggplot(p4, aes(x = x, y = predicted)) +
  geom_line(linetype = 2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15) +
  ylim(min(survey$corr), max(survey$corr))

(q1+q2)/(q3+q4)
