# 04/16/2026
# Anneliese Pinnell

#Create a three panel with (mBBS)
#two highly correlated species: Grey Catbird, Ovenbird
#two near zero: Blue Jay and American Crow
#and one negative: Tufted Titmouse and Eastern Bluebird

library(tidyverse)
library(ggplot2)
library(patchwork)

mbbs <- read.csv("data/mbbs/mbbsDeltaYLong.csv")

# Grouped
positive <- mbbs |>
  filter(common_name == "Gray Catbird" | common_name == "Ovenbird") |>
  pivot_wider(
    names_from = common_name,
    values_from = yoy_change
  ) |>
  mutate(catbirdRank = rank(`Gray Catbird`)) |>
  mutate(ovenbirdRank = rank(`Ovenbird`))

nearZero <- mbbs |>
  filter(common_name == "Blue Jay" | common_name == "American Crow") |>
  pivot_wider(
    names_from = common_name,
    values_from = yoy_change
  ) |>
  mutate(blueRank = rank(`Blue Jay`)) |>
  mutate(americanRank = rank(`American Crow`))

negative <- mbbs |>
  filter(common_name == "Tufted Titmouse" | common_name == "Eastern Bluebird") |>
  pivot_wider(
    names_from = common_name,
    values_from = yoy_change
  ) |>
  mutate(tuftedRank = rank(`Tufted Titmouse`)) |>
  mutate(easternRank = rank(`Eastern Bluebird`))

png(filename = "figures/exampleRho.png", width = 7, height = 2.25, units = "in", 
    res = 300)
par(mfrow = c(1, 3))
# Catbird vs. Ovenbird - Positive
model <- lm(positive$ovenbirdRank ~ positive$catbirdRank)
slope_val <- round(coef(model)[2], 3)
subtitle <- paste("Slope:", slope_val)
plot(positive$catbirdRank, positive$ovenbirdRank, 
     main=paste("Positive Spearman's rho\n", "rho:", slope_val),
     xlab="Gray Catbird", ylab="Ovenbird", 
     pch=19, col="blue")
abline(model, col = "red", lwd = 2)


# Blue Jay vs. American Crow - near zero
model <- lm(nearZero$blueRank ~ nearZero$americanRank)
slope_val <- round(coef(model)[2], 3)
plot(nearZero$blueRank, nearZero$americanRank, 
     main=paste("Near Zero Spearman's rho\n", "rho:", slope_val),
     xlab="Blue Jay", ylab="American Crow", 
     pch=19, col="blue")
abline(model, col = "red", lwd = 2)

# Tufted Titmouse vs. Eastern Bluebird - negative
model <- lm(negative$tuftedRank ~ negative$easternRank)
slope_val <- round(coef(model)[2], 3)
plot(negative$tuftedRank, negative$easternRank, 
     main=paste("Negative Spearman's rho\n", "rho:", slope_val),,
     xlab="Tufted Titmouse", ylab="Eastern Bluebird", 
     pch=19, col="blue")
abline(model, col = "red", lwd = 2)

dev.off()
