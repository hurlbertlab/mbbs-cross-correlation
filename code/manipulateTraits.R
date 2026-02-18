# Last updated: 02/18/2026 - Anneliese Pinnell

# The goal of this code is to create more correlation heatmaps for species
# of interest

library(tidyverse)

#Load in data
mbbsTraits <- read.csv("data/traits/mbbsTraits.csv")

#Numeric traits only
numericTraits <- mbbsTraits |>
  select(where(is.numeric))

numericTraits$"common_name" <- mbbsTraits$English.Name..BirdLife...IOC...Clements.AviList.

chick <- numericTraits |> filter(common_name == "Carolina Chickadee")
tuft <- numericTraits |> filter(common_name == "Tufted Titmouse")

#chick <- chick[,-ncol(chick)]
#tuft <- tuft[,-ncol(tuft)]

#Join species
together <- rbind(chick, tuft)

df_wider <- together %>%
  pivot_wider(
    names_from = `common_name`,  # Use the English Name as new column names
    values_from = everything()   # This means all other columns should be included
  )

#Make cor matrix
#cor_matrix <- cor(together, method = "spearman")

