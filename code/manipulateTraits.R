# Last updated: 02/18/2026 - Anneliese Pinnell

# The goal of this code is to create more correlation heatmaps for species
# of interest

library(tidyverse)

#Load in data
mbbsTraits <- read.csv("data/traits/mbbsTraits.csv") %>%
  #create an easier to use common name column
  mutate(common_name = English.Name..BirdLife...IOC...Clements.AviList.) %>%
  #pick just the columns of interest
  dplyr::select(common_name, Average.Mass, Primary.Diet, Primary.Habitat, Migration, BrS1, BrS2, Prod1, Prod2)

#Numeric traits only
numericTraits <- mbbsTraits |>
  select(common_name, where(is.numeric))

#Filter to just chickadee and titmouse
together <- numericTraits |>
  filter(common_name %in% c("Carolina Chickadee", "Tufted Titmouse")) 

# you need a dataset where every species is listed n(unique_species) times in sp1, and then in sp2 you need the unique list of every species, repeated n(unqiue_species) times. Then, you can left_join in the traits based on sp1 and then sp2, and then use mutate to create the dataset

n_species <- length(unique(together$common_name))
species_list <- unique(together$common_name)

sp1 <- rep(species_list, each = n_species)
sp2 <- rep(species_list, times = n_species)

temp <- data.frame(sp1 = sp1, sp2 = sp2) |>
  left_join(mbbsTraits, by = c("sp1" = "common_name")) |>
  left_join(mbbsTraits, by = c("sp2" = "common_name")) |>
  mutate(dif_weight = abs(Average.Mass.x - Average.Mass.y))
  


#glm stats model
#with the correlation ~ differences in traits

# Commenting out, no need to create a cor matrix between these.
# # create a cor matrix for trait Average.Mass
# trait_df <-
#   together |>
#   select(common_name, Average.Mass, Migration) |>
#   pivot_wider(names_from = common_name,
#               values_from = Average.Mass)
# cor(trait_df)
# 
# df_wider <- together %>%
#   pivot_wider(
#     names_from = `common_name`,  # Use the English Name as new column names
#     values_from = everything()   # This means all other columns should be included
#   )

#Make cor matrix
#cor_matrix <- cor(together, method = "spearman")

