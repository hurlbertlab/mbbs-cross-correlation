# Last updated: 02/18/2026 - Anneliese Pinnell

# The goal of this code is to create differences

library(tidyverse)
library(ggplot2)
library(ggeffects)
library(sjPlot)
library(dplyr)

#Load in data
mbbsTraits <- read.csv("data/traits/mbbsTraits.csv") %>%
  #create an easier to use common name column
  mutate(common_name = English.Name..BirdLife...IOC...Clements.AviList.) %>%
  #pick just the columns of interest
  dplyr::select(common_name, Unsexed.MinMass, Average.Mass, Elevational.Range)
  #dplyr::select(common_name, Average.Mass, Primary.Diet, Primary.Habitat, Migration, BrS1, BrS2, Prod1, Prod2)

mbbsCorr <- read.csv("data/corrMatrices/spring_delta_y_corr_matrix.csv")

#Filter to just chickadee and titmouse
together <- mbbsTraits |>
  filter(common_name %in% c("Carolina Chickadee", "Tufted Titmouse", "Eastern Bluebird")) 

#Get the number of unique species
n_species <- length(unique(together$common_name))
#Get the unique species
species_list <- unique(together$common_name)

sp1 <- rep(species_list, each = n_species)
sp2 <- rep(species_list, times = n_species)

calculated <- data.frame(sp1 = sp1, sp2 = sp2) |>
  left_join(mbbsTraits, by = c("sp1" = "common_name")) |>
  left_join(mbbsTraits, by = c("sp2" = "common_name")) |>
  mutate(dif_weight = abs(Average.Mass.x - Average.Mass.y)) |>
  mutate(dif_elevaRange = abs(Elevational.Range.x - Elevational.Range.y)) |>
  mutate(dif_umin_mass = abs(Unsexed.MinMass.x - Unsexed.MinMass.y))

calculated$sp1 <- gsub(" ", ".", calculated$sp1)
calculated$sp2 <- gsub(" ", ".", calculated$sp2)

calculated <- calculated[!duplicated(apply(calculated[,1:2], 1, function(row) paste(sort(row), collapse=""))),]

corrValues <- c()
for (i in 1:length(calculated$sp1)){
  addedValue <- mbbsCorr |> filter(X == calculated$sp1[i]) |> select(calculated$sp2[i])
  corrValues <- c(corrValues, addedValue)
}

calculated$corr <- as.numeric(corrValues)

#glm stats model
#with the correlation ~ differences in traits
glm_model <- glm(corr ~ dif_weight + dif_elevaRange + dif_umin_mass, data = calculated, family="gaussian")
summary(glm_model)

#plot_model(glm_model, vline.color = "red")
#plot_model(glm_model, show.values = TRUE, value.offset = .3)
#plot_model(glm_model, type = "pred", terms = "dif_weight")
