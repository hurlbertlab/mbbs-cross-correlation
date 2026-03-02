#General distributions
library(ggplot2)
library(tidyverse)
library(patchwork)
library(randomForest)
library(ggplot2)
library(dplyr)
library(ggeffects)
library(sjPlot)
library(gt)
library(DHARMa)
library(gtsummary)
library(beepr)

#Read in data
spring <- read.csv("data/traits/springTraitsAndCorr.csv") |> 
  filter(corr !=1)
mbbs <- read.csv("data/traits/mbbsTraitsAndCorr.csv")|> 
  filter(corr !=1)
cbc <- read.csv("data/traits/CBCTraitsAndCorr.csv")|> 
  filter(corr !=1)
resident <- read.csv("data/traits/ResidentTraitsAndCorr.csv")|> 
  filter(corr !=1)



# To collapse --> Cmd+Opt+L
#Density of correlation values by survey
#Spring
p1 <- ggplot(spring, aes(x=corr)) + 
  geom_histogram(aes(y=after_stat(density)), colour="black", fill="white",
                 binwidth = .05) +
  labs(x="Correlation Value",
       y = "Density",
       title = "Spring") +
  geom_density(alpha=.2, fill="#FF6666") +
  scale_x_continuous(breaks = seq(-1, 1, 0.1))
#mbbs
p2 <- ggplot(mbbs, aes(x=corr)) + 
  geom_histogram(aes(y=after_stat(density)), colour="black", fill="white",
                 binwidth = .05) +
  labs(x="Correlation Value",
       y = "",
       title = "mBBS") +
  geom_density(alpha=.2, fill="#FF6666") +
  scale_x_continuous(breaks = seq(-1, 1, 0.1))
#CBC
p3 <- ggplot(cbc, aes(x=corr)) + 
  geom_histogram(aes(y=after_stat(density)), colour="black", fill="white",
                 binwidth = .05) +
  labs(x="Correlation Value",
       y = "Density",
       title = "CBC") +
  geom_density(alpha=.2, fill="#FF6666") +
  scale_x_continuous(breaks = seq(-1, 1, 0.1))

p4 <- ggplot(resident, aes(x=corr)) + 
  geom_histogram(aes(y=after_stat(density)), colour="black", fill="white",
                 binwidth = .05) +
  labs(x="Correlation Value",
       y = "",
       title = "Resident (Spring-CBC)") +
  geom_density(alpha=.2, fill="#FF6666") +
  scale_x_continuous(breaks = seq(-1, 1, 0.1))

#Final product
combined <- p1 + p2 + p3 + p4
hisPlot <- combined + 
  plot_annotation(
    title = 'Yearly Abundance Change Correlation Values Distribution by Survey',
    subtitle = '1999 - 2025',
  )

png(filename = paste("figures/genGraphs/densityPlotCorrValues.png", sep = ""), 
    width = 1000, height = 400)
hisPlot
dev.off()

# Random Forest Models!

randomfor <- function(file, iterationNum, topValueNums){
  # Select columns that start with 'dif_' and the target variable
  variables <- file |>
    select(starts_with("dif_"))
  target <- file$corr
  
  iterations <- seq(1:iterationNum)
  runningImportance <- data.frame(row.names = c("var", "importance"))
  
  for (i in iterations){
    rf_model <- randomForest(variables, target, importance = TRUE)
    importance <- importance(rf_model)
    importance_df <- data.frame(var = rownames(importance), importance = importance[, 1])
    
    if(i != 1){
      runningImportance$importance <- runningImportance$importance + importance_df$importance
    }else{
      runningImportance <- importance_df
    }
  }
  
  runningImportance$mean <- runningImportance$importance/iterationNum
  
  sorted <- runningImportance |>
    arrange(desc(mean)) |>
    head(topValueNums)
  
  return(sorted)
}

springRandFor <- randomfor(spring, 10, 10)
mbbsRandFor <- randomfor(mbbs, 10, 10)
cbcRandFor <- randomfor(cbc, 10, 10)
residentRandFor <- randomfor(resident, 10, 10)

# Plot variable importance
p1 <- ggplot(springRandFor, aes(x = reorder(var, mean), y = mean)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Spring", x = "Variables", y = "Importance") +
  theme_minimal()
p2 <- ggplot(mbbsRandFor, aes(x = reorder(var, mean), y = mean)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "mBBS", x = "", y = "Importance") +
  theme_minimal()
p3 <- ggplot(cbcRandFor, aes(x = reorder(var, mean), y = mean)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "CBC", x = "Variables", y = "Importance") +
  theme_minimal()
p4 <- ggplot(residentRandFor, aes(x = reorder(var, mean), y = mean)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Residents (Spring-CBC)", x = "", y = "Importance") +
  theme_minimal()


combined <- p1 + p2 + p3 + p4
randForCombo <- combined + 
  plot_annotation(
    title = 'Variable Importance from Random Forest (10 Iterations) by Survey',
    subtitle = '1999 - 2025',
  )
png(filename = paste("figures/genGraphs/randomForestImportance.png", sep = ""), 
    width = 800, height = 500)
randForCombo
dev.off()

# Yay GLMs!
literature <- c("dif_mass", "dif_mig", "dif_trophicLvl", "dif_clutchMax")
importantRF <- c("dif_wingLen", "dif_KippsDist", "dif_mass", "dif_Secondary")
randomInterests <- c("dif_wingLen", "dif_mass", "dif_beakLenCulm", "dif_beakLenNare")
interest <- literature
formula <- paste("corr ~", paste(interest, collapse = " + "))

glm_model <- glm(formula , data = mbbs, family="gaussian")
summary(glm_model)

png(filename = paste("figures/genGraphs/mbbsGLMLiterature.png", sep = ""), 
    width = 800, height = 800)
par(mfrow = c(2, 2))
plot(glm_model, which = 1, cex.lab = 1.5, cex.main = 2, cex.axis = 1.2)
plot(glm_model, which = 2, cex.lab = 1.5, cex.main = 2, cex.axis = 1.2)
plot(glm_model, which = 3, cex.lab = 1.5, cex.main = 2, cex.axis = 1.2)
plot(glm_model, which = 4, cex.lab = 1.5, cex.main = 2, cex.axis = 1.2)
mtext("mBBS Literature GLM", side=3, line=-1.5, outer=TRUE, cex=1.75)
mtext(formula, side=3, line=-2.4, outer=TRUE, cex=1)
par(mfrow = c(1,1))
dev.off()

png(filename = paste("figures/genGraphs/mbbsGLMCoefficientPlot.png", sep = ""), 
    width = 500, height = 350)
plot_model(glm_model, show.values = TRUE, value.offset = .3) + 
  labs(title = "GLM Coefficient Plot for Correlation Values - mBBS")
dev.off()

p1 <- plot_model(glm_model, type = "pred", terms = interest[1], title = "")# + labs(x = "Difference in Mass", y = "Correlation")
p2 <- plot_model(glm_model, type = "pred", terms = interest[2], title = "")# + labs(x = "Equivilency of Migration Pattern", y = "")
p3 <- plot_model(glm_model, type = "pred", terms = interest[3], title = "")#+ labs(x = "Equivilancy of Trophic Level", y = "Correlation")
p4 <- plot_model(glm_model, type = "pred", terms = interest[4], title = "")#+ labs(x = "Difference in Maximum Clutch Size", y = "")


combined <- (p1+p2)/(p3+p4)
newCombined <- combined + 
  plot_annotation(
    #title = 'Predicted correlation values - differences in mass, migration, trophic level, and clutch maximum',
    subtitle = '1999 - 2025',
  )
newCombined
png(filename = paste("figures/genGraphs/mbbsPredictedCorrValues.png", sep = ""), 
    width = 600, height = 500)
newCombined
dev.off()

# Display extreme high values and extreme lows for each correlation matrix

getMaxMin <- function(fileName, surveyName){
  maxMbbs <- fileName |> 
    filter(corr != 1) |>
    filter(corr == max(corr)) |>
    select(c("sp1", "sp2", "corr")) |>
    mutate("Survey" = surveyName)
  
  minMbbs <- fileName |>
    filter(corr == min(corr)) |>
    select(c("sp1", "sp2", "corr")) |>
    mutate("Survey" = surveyName)

  joined <- rbind(maxMbbs, minMbbs)
  return(joined)
}

allJoined <- rbind(getMaxMin(spring, "Spring"), getMaxMin(mbbs, "mBBS"),
                   getMaxMin(cbc, "CBC"), getMaxMin(resident, "Residents"))


allJoined <- allJoined |>
  rename("Species A" = sp1, "Species B" = sp2, 
         "Correlation" = corr)|>
  gt() |> 
  tab_header(
    title = "Minimum and Maximum Correlation Values",
    subtitle = "1999-2025"
  ) |>
  fmt_number(
    columns = everything(),
    decimals = 4
  )

allJoined


# Test literally everything!
allNames <- names(mbbs |> select(-c(sp1, sp2, corr, X)))
allNames

allCombos <- combn(allNames, 4)
allCombos <- as.data.frame(t(allCombos))

emptyDataframe <- data.frame("formula" = c(), "num" = c())

for (i in 1:nrow(allCombos)){
  interest <- allCombos[i,]
  formula2 <- paste("corr ~", paste(interest, collapse = " + "))
  
  glm_model <- glm(formula2 , data = mbbs, family="gaussian")
  temp <- data.frame(confint(glm_model))
  
  lower <- round(temp$X2.5.., 3)
  upper <- round(temp$X97.5.., 3)
  #Checks to see how many variables cross 0
  temp$diff <- (lower < 0 & upper > 0)
  #True values = 1, so ideally less than 2
  
  if(sum(temp$diff) <= 1){
    formulaDataframe <- data.frame("formula" = c(formula2), "num" = c(sum(temp$diff)))
    emptyDataframe <- rbind(emptyDataframe, formulaDataframe)
    }
}

beepr::beep()

ownThoughts <- c("dif_mig", "dif_habDen", "dif_mass", "dif_tarsusLen")
ownThoughtsVerbal <- c("Equivalency of Migration", "Difference in Habitat Density",
                       "Equivalency of Habitat Density", "Difference in Tarsus Length")
literature <- c("dif_mass", "dif_mig", "dif_trophicLvl", "dif_clutchMax")
literatureVerbal <- c("Difference in Mass", "Equivalency of Migration",
                      "Equivalency of Trophic Level", "Difference in Clutch Maximum")
interestTitles <- ownThoughtsVerbal
interest <- ownThoughts
formula <- paste("corr ~", paste(interest, collapse = " + "))
glm_model <- glm(formula, 
                 data = mbbs, family="gaussian")
#summary(glm_model)
#png(filename = paste("figures/genGraphs/GLMEvalLiterature.png", sep = ""), width = 800, height = 500)
tbl_regression(glm_model, estimate_fun = label_style_number(digits = 3))
#dev.off()

confidence <- data.frame(confint(glm_model))

#Checks to see how many variables cross 0
confidence$diff <- (confidence$X2.5.. <= 0 & confidence$X97.5..>= 0)
confidence
plot_model(glm_model, show.values = TRUE, value.offset = .3) + 
  labs(title = "GLM Coefficient Plot for Correlation Values - mBBS")


p1 <- plot_model(glm_model, type = "pred", terms = interest[1], title = "") + labs(x = interestTitles[1], y = "Correlation")
p2 <- plot_model(glm_model, type = "pred", terms = interest[2], title = "") + labs(x = interestTitles[2], y = "")
p3 <- plot_model(glm_model, type = "pred", terms = interest[3], title = "")+ labs(x = interestTitles[3], y = "Correlation")
p4 <- plot_model(glm_model, type = "pred", terms = interest[4], title = "")+ labs(x = interestTitles[4], y = "")

combined <- (p1+p2)/(p3+p4)
png(filename = paste("figures/genGraphs/GLMEval.png", sep = ""), width = 800, height = 500)
combined
dev.off()

png(filename = paste("figures/genGraphs/GLMEval2.png", sep = ""), width = 800, height = 500)
par(mfrow = c(2, 2))
plot(glm_model, which = 1, cex.lab = 1.5, cex.main = 2, cex.axis = 1.2)
plot(glm_model, which = 2, cex.lab = 1.5, cex.main = 2, cex.axis = 1.2)
plot(glm_model, which = 3, cex.lab = 1.5, cex.main = 2, cex.axis = 1.2)
plot(glm_model, which = 4, cex.lab = 1.5, cex.main = 2, cex.axis = 1.2)
mtext("mBBS Non-Literature GLM", side=3, line=-1.5, outer=TRUE, cex=1.75)
mtext(formula, side=3, line=-2.4, outer=TRUE, cex=1)
par(mfrow = c(1,1))
dev.off()


#DHARMa checks!
simr <- simulateResiduals(glm_model, n=1000)
plot(simr)
#Points should follow the red line
testUniformity(simr)
#Normal distrbution
testDispersion(simr)

