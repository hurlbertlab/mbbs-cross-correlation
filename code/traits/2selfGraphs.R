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

resMbbs <- read.csv("data/traits/residents/mbbsResidentTraitsAndCorr.csv") |>
  filter(corr != 1)
resCBC <- read.csv("data/traits/residents/CBCResidentTraitsAndCorr.csv") |>
  filter(corr != 1)

#Yay GLM
glm_Models <- function(interest, interestNames, survey, name){
  toExport <- data.frame("variable_name" = c(), "coefficient" = c(), "p_value" = c())
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
                   colour = "cornflowerblue", alpha = 0.1) +
        geom_line(linetype = ifelse(modelCoefficients[i+1] == FALSE, 2, 1),
                  colour = "black") +
        #geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.4) +
        ylim(min(survey$corr), max(survey$corr)) +
        labs(x = interestNames[[j]][i], y = ifelse(i%%2 == 1, "Correlation Value", ""))
      toExport <- rbind(toExport, data.frame("variable_name" = c(interestNames[[j]][i]),
                                             "coefficient" = c(summary(glm_model)$coefficients[i+1]),
                                             "p_value" = c(summary(glm_model)$coefficients[,4][i+1])))
    }
    if(length(variableInfluencePlots) == 4){
      png(filename = paste0("figures/variableInfluence/", as.character(name), as.character(j), ".png"), 
          res = 300, units = "in", width = 6, height = 5)
      combinedPlot <- (variableInfluencePlots[[1]]+variableInfluencePlots[[2]])/(variableInfluencePlots[[3]]+variableInfluencePlots[[4]])
      plot(combinedPlot)
     # plot(combinedPlot + plot_annotation(
    #    title = paste0(c("Generalized-Linear Model - Traits for", name), collapse = " "),
    #    subtitle = "1999-2025"
    #  ))
      dev.off()
    }else{
      png(filename = paste0("figures/variableInfluence/", as.character(name), as.character(j), ".png"), 
          res = 300, units = "in", width = 6, height = 5)
      combinedPlot <- (variableInfluencePlots[[1]]+variableInfluencePlots[[2]])/(variableInfluencePlots[[3]])
      plot(combinedPlot)
     # plot(combinedPlot + plot_annotation(
    #    title = paste0(c("Relationship between Trait Correlation and ", name), collapse = " "),
    #    subtitle = "1999-2025"
    #  ))
      dev.off()
    }
  }
  write.csv(toExport, paste0(c("data/variableInfluences/", name, "Values.csv"), collapse = ""))
  return(summary(glm_model))
}

set1 <- c("dif_mass", "dif_mig","dif_trophicLvl")
set2 <- c("dif_primHab", "dif_habDen", "dif_DB")
set1Names <- c("Absolute Mass Difference", "Absolute Migration Difference", "Identical Trophic Level")
set2Names <- c("Identical Primary Habitat", "Absolute Habitat Density Difference", "Absolute Diet Breadth Difference")
allVariables <- list(set1, set2)
allNames <- list(set1Names, set2Names)

mbbsSummary <- glm_Models(interest = allVariables, interestNames = allNames, survey = mbbs, name = "mBBS")
#cbcSummary <- glm_Models(interest = allVariables, interestNames = allNames, survey = cbc, name = "CBC")
mbbsResSummary <- glm_Models(interest = allVariables, interestNames = allNames, survey = resMbbs, name = "mBBSRes")
#cbcResSummary <- glm_Models(interest = allVariables, interestNames = allNames, survey = resCBC, name = "CBCRes")
#write.csv(mbbsSummary, paste0(c("data/variableInfluences/", name, "Values.csv"), collapse = ""))
#mbbsSummary


## Scatterplot!
rMbbsCorrs <- resMbbs |>
  select("sp1", "sp2", "corr") |>
  mutate("mBBSCorr" = corr) |>
  select(-corr)

rCBCCorrs <- resCBC |>
  select("sp1", "sp2", "corr") |>
  mutate("CBCCorr" = corr) |>
  select(-corr)

rMbbsAndCBC <- left_join(rMbbsCorrs, rCBCCorrs)

# Investigate number of points above vs. below
rMbbsAndCBC$Above <- rMbbsAndCBC$CBCCorr > rMbbsAndCBC$mBBSCorr
above <- sum(rMbbsAndCBC$Above == TRUE)
below <- sum(rMbbsAndCBC$Above == FALSE)

# Create the scatterplot with color based on mBBSCorr values
png(filename = "figures/correlationScatter.png", units = "in", width = 7, height = 4,
    res = 300)
ggplot(data = rMbbsAndCBC, aes(x = mBBSCorr, y = CBCCorr, color = CBCCorr > mBBSCorr)) +
  geom_point() +
  labs(title = "Scatterplot of mBBS Correlation Values vs CBC Correlation Values",
       x = "mBBS Correlation Values",
       y = "CBC Correlation Values",
       color = "Relation to 1-1 Line") + 
  scale_color_hue(labels = c("FALSE" = "Below (N = 255)", "TRUE" = "Above (N = 241)")) +
  geom_abline(intercept = 0, slope = 1) +
  theme_minimal()
dev.off()
