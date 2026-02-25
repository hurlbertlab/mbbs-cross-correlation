#General distributions
library(ggplot2)
library(tidyverse)
library(patchwork)
library(randomForest)
library(ggplot2)
library(dplyr)
library(ggeffects)
library(sjPlot)

#Read in data
spring <- read.csv("data/traits/springTraitsAndCorr.csv")
mbbs <- read.csv("data/traits/mbbsTraitsAndCorr.csv")
cbc <- read.csv("data/traits/CBCTraitsAndCorr.csv")
resident <- read.csv("data/traits/ResidentTraitsAndCorr.csv")


# Remove 1s from corr plots
springNo1s <- spring |>
  filter(corr != 1)
mbbsNo1s <- mbbs |>
  filter(corr != 1)
cbcNo1s <- cbc |>
  filter(corr != 1)
residentNo1s <- resident |>
  filter(corr != 1)

# To collapse --> Cmd+Opt+L
#Density of correlation values by survey
#Spring
p1 <- ggplot(springNo1s, aes(x=corr)) + 
  geom_histogram(aes(y=after_stat(density)), colour="black", fill="white",
                 binwidth = .05) +
  labs(x="Correlation Value",
       y = "Density",
       title = "Spring") +
  geom_density(alpha=.2, fill="#FF6666") +
  scale_x_continuous(breaks = seq(-1, 1, 0.1))
#mbbs
p2 <- ggplot(mbbsNo1s, aes(x=corr)) + 
  geom_histogram(aes(y=after_stat(density)), colour="black", fill="white",
                 binwidth = .05) +
  labs(x="Correlation Value",
       y = "Density",
       title = "mBBS") +
  geom_density(alpha=.2, fill="#FF6666") +
  scale_x_continuous(breaks = seq(-1, 1, 0.1))
#CBC
p3 <- ggplot(cbcNo1s, aes(x=corr)) + 
  geom_histogram(aes(y=after_stat(density)), colour="black", fill="white",
                 binwidth = .05) +
  labs(x="Correlation Value",
       y = "Density",
       title = "CBC") +
  geom_density(alpha=.2, fill="#FF6666") +
  scale_x_continuous(breaks = seq(-1, 1, 0.1))

p4 <- ggplot(residentNo1s, aes(x=corr)) + 
  geom_histogram(aes(y=after_stat(density)), colour="black", fill="white",
                 binwidth = .05) +
  labs(x="Correlation Value",
       y = "Density",
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
  labs(title = "mBBS", x = "Variables", y = "Importance") +
  theme_minimal()
p3 <- ggplot(cbcRandFor, aes(x = reorder(var, mean), y = mean)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "CBC", x = "Variables", y = "Importance") +
  theme_minimal()
p4 <- ggplot(residentRandFor, aes(x = reorder(var, mean), y = mean)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Residents", x = "Variables", y = "Importance") +
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

plot_model(glm_model, vline.color = "red")
plot_model(glm_model, show.values = TRUE, value.offset = .3) + labs(title = "GLM Coefficient Plot for Correlation Values")


p1 <- plot_model(glm_model, type = "pred", terms = interest[1], title = "")
p2 <- plot_model(glm_model, type = "pred", terms = interest[2], title = "")
p3 <- plot_model(glm_model, type = "pred", terms = interest[3], title = "")
p4 <- plot_model(glm_model, type = "pred", terms = interest[4], title = "")

combined <- (p1+p2)/(p3+p4)
newCombined <- combined + 
  plot_annotation(
    title = 'Predicted correlation values - differences in mass, migration, trophic level, and clutch maximum',
    subtitle = '1999 - 2025',
  )
newCombined

