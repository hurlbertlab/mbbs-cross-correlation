# Created 02/12/2026
# Last updated 02/12/2026 by Anneliese Pinnell

# This file maps pearson vs. spearman correlations for species and population
# fluctuations

library(tidyverse)
library(ggplot2)

mbbs <- read.csv("data/mbbs/mbbsMerged.csv")

#Chickadee - Titmouse
chickTuft <- mbbs |>
  filter(common_name %in% c("Carolina Chickadee", "Tufted Titmouse")) |>
  group_by(common_name, year) |>
  mutate(count = sum(count)) |>
  unique()

#General population trend
ggplot(chickTuft, aes(x=year, y=count, color = common_name)) + geom_point()

# Sample data
chick <- chickTuft |> filter(common_name == "Carolina Chickadee")
tuft <- chickTuft |> filter(common_name == "Tufted Titmouse")

chick <- chick$count
tuft <- tuft$count

# Correlation scatterplot
plot(rank(chick), rank(tuft), main="Spearman Correlation", 
     xlab="Carolina Chickadee", ylab="Tufted Titmouse")

# Pearson
plot(chick, tuft, main="Pearson Correlation")

