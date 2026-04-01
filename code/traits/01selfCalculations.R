# Created 03/24/2026
# Anneliese Pinnell

# The purpose of this file to calculate migration distance from furthest point
# North or South to Chapel Hill
# Calculate range similarity (how much overlap)
# Add in Ivara Diet!

library(tidyverse)
library(tmap)
library(sf)

#Load in and get species codes
allBirds <- c(list.dirs(path = "data/ranges/2023", full.names = FALSE, recursive = FALSE))

format <- c("data/ranges/2023/", "/ranges/", "_range_raw_9km_2023.gpkg")
paste0(c(format[1], allBirds[1], format[2], allBirds[1], format[3]), collapse = "")
rangeInfo <- data.frame()
for (item in allBirds){
  format <- c("data/ranges/2023/", "/ranges/", "_range_raw_9km_2023.gpkg")
  range <- st_read(paste0(c(format[1], item, format[2], item, format[3]), collapse = ""))
  nonBreedingRes <- range |>
    filter(season == "nonbreeding" | season == "resident")
  breedingRange <- range |>
    filter(season == "breeding" | season == "resident")
  nonCentroid <- st_centroid(nonBreedingRes)
  breedingCentroid <- st_centroid(breedingRange)
  distanceKM <- as.numeric(st_distance(nonCentroid, breedingCentroid))/1000
  addRow <- data.frame("nameCode" = c(item), "common_name" = c(range$common_name[1]),
                       "migDistanceKM" = c(distanceKM), "nonBreedPoly" = c(nonBreedingRes))
  rangeInfo <- rbind(rangeInfo, addRow)
}

# Load in Ivara's Data
diet <- read.csv("data/IvaraDiet/fraction_diet_arthropods.csv") |>
  select(c(common_name, Final_Fraction_Diet_Wt))

joined <- left_join(rangeInfo, diet)
