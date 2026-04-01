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

#Yay GLM
glm_Models <- function(interest, interestNames, survey, name){
  for(j in seq(length(interest))){
    formula <- paste("corr ~", paste(interest[[j]], collapse = " + "))
    glm_model <- glm(formula, data = survey, family="gaussian")
    #TRUE if p-value is less than 0.05
    modelCoefficients <- c(summary(glm_model)$coefficients[,4] < 0.05)
    variableInfluencePlots <- c()
    for(i in seq(length(interest[[j]]))){
      variableInfluencePlots[[i]] <- ggplot(
        as.data.frame(ggpredict(glm_model, terms = interest[[j]][i]))
        , aes(x = x, y = predicted)) + 
        geom_point(data = survey, aes(x = .data[[interest[[j]][i]]], y = corr),
                   colour = "purple", alpha = 0.1) +
        geom_line(linetype = ifelse(modelCoefficients[i+1] == FALSE, 2, 1),
                  colour = "black") +
        #geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.4) +
        ylim(min(survey$corr), max(survey$corr)) +
        labs(x = interestNames[[j]][i], y = ifelse(i%%2 == 1, "Correlation Value", ""))
    }
    if(length(variableInfluencePlots) == 4){
      png(filename = paste0("figures/variableInfluence/", as.character(name), as.character(j), ".png"), 
          res = 300, units = "in", width = 6, height = 5)
      combinedPlot <- (variableInfluencePlots[[1]]+variableInfluencePlots[[2]])/(variableInfluencePlots[[3]]+variableInfluencePlots[[4]])
      plot(combinedPlot + plot_annotation(
        title = paste0(c("Generalized-Linear Model - Traits for", name), collapse = " "),
        subtitle = "1999-2025"
      ))
      dev.off()
    }else{
      png(filename = paste0("figures/variableInfluence/", as.character(name), as.character(j), ".png"), 
          res = 300, units = "in", width = 6, height = 5)
      combinedPlot <- (variableInfluencePlots[[1]]+variableInfluencePlots[[2]])/(variableInfluencePlots[[3]])
      plot(combinedPlot + plot_annotation(
        title = paste0(c("Generalized-Linear Model - Traits for", name), collapse = " "),
        subtitle = "1999-2025"
      ))
      dev.off()
    }
  }
}


other <- c("dif_mass", "dif_mig", "dif_ESI")
otherNames <- c("Mass Difference", "Identical Migration Style", "Ecological Specialization Index Difference")
habitatSimilarity <- c("dif_habitat", "dif_habDen", "dif_primHab", "dif_HB")
habitatSimilarityNames <- c("Identical Habitat", "Identical Habitat Density", "Identical Primary Habitat", "Habitat Breadth Difference")
diet <- c("dif_primDiet", "dif_trophicLvl", "dif_trophicNic", "dif_DB")
dietNames <- c("Identical Primary Diet", "Identical Trophic Level", "Identical Trophic Niche", "Diet Breadth Difference")

allVariables <- list(other, habitatSimilarity, diet)
#allVariables <- list(other)
#allNames <- list(otherNames)
allNames <- list(otherNames, habitatSimilarityNames, dietNames)

glm_Models(interest = allVariables, interestNames = allNames, survey = mbbs, name = "mBBS")
glm_Models(interest = allVariables, interestNames = allNames, survey = cbc, name = "CBC")

#formula <- paste("corr ~", paste(allVariables[[1]], collapse = " + "))
#glm_model <- glm(formula, data = mbbs, family="gaussian")
#modelCoefficients <- c(summary(glm_model)$coefficients[,4] < 0.05)
#ggplot(
#  as.data.frame(ggpredict(glm_model, terms = allVariables[[1]][1]))
#  , aes(x = x, y = predicted)) + 
#  geom_point(data = mbbs, aes(x = allVariables[[1]][1], y = corr)) +
#  geom_line(colour = "purple",linetype = ifelse(modelCoefficients[1+1] == FALSE, 2, 1)) +
#  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, colour = "purple") +
#  ylim(min(mbbs$corr), max(mbbs$corr)) +
#  labs(x = allNames[[1]][1], y = ifelse(1%%2 == 1, "Correlation Value", ""))

