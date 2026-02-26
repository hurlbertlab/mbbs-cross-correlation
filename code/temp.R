# Created 11/20/2025
# Last updated 1/29/2025 by Anneliese Pinnell
#Added CBC and Spring data
#Removed Pct change
#Added delta y long export

#Converts long data to wide form
#Generates sums for each year for each species

library(dplyr)
library(tidyr)
library(purrr)


fileName <- "data/CBCHistoricData/CBCMerged.csv"
longName <- "data/CBCHistoricData/CBCMergedLong.csv"
wideName <- "data/CBCHistoricData/CBCMergedWide.csv"
deltaYName <- "data/CBCHistoricData/CBCMergedDeltaY.csv"
deltaYLong <-"data/CBCHistoricData/CSBSDeltaYLong.csv" 
minimum <- 0.2

#Read in data
bird_info = read.csv(fileName)

#need to hash out species included/excluded basis
#Exclude hawks and owls, waterbirds, and categories that are not species-specific.
excluded_species <- c("Red-shouldered Hawk", "Killdeer", "Great Blue Heron", "Canada Goose", "Turkey Vulture", "Black Vulture", "crow sp.","duck sp.","hawk sp.","passerine sp.", "swallow sp.","waterfowl sp.","woodpecker sp.", "Summer/Scarlet Tanager", "Sharp-shinned/Cooper's Hawk", "Mute Swan", "Mississippi Kite", "Mallard", "Green Heron","Great Horned Owl", "Great Egret", "Eastern Screech-Owl", "Double-crested Cormorant", "Cooper's Hawk" , "Sharp-shinned Hawk", "Broad-winged Hawk", "Belted Kingfisher", "Barred Owl", "American/Fish Crow", "Accipitrine hawk sp.", "Yellow-crowned Night Heron", "Wood Duck", "Osprey", "Bald Eagle", "Red-tailed Hawk")

#Only includes data from 1999 and on
bird_info <- bird_info |>
  filter(year >= 1999)

##Removes low number birds based off of means
#Calculates means
bird_means <- bird_info %>%
  group_by(common_name) %>%
  summarise(mean_value = mean(count, na.rm = TRUE))

#Determines birds to keep
birds_to_keep <- bird_means %>%
  filter(mean_value > minimum) %>%
  filter(!common_name %in% excluded_species) %>%
  pull(common_name)

#Filters main data to only have kept birds
only_kept_birds <- bird_info %>%
  filter(common_name %in% birds_to_keep) %>%
  slice(1:n())  #resets index

#Sums species to have a count for each year
individual_species <- only_kept_birds %>%
  group_by(common_name, year) %>%
  summarise(count = sum(count), .groups = "drop")

#Long form data
write.csv(individual_species, longName, row.names = FALSE)

#Pivots data to wide form
df_pivot <- individual_species %>%
  pivot_wider(names_from = common_name, values_from = count, values_fill = 0)

#To csv file
write.csv(df_pivot, wideName, row.names = FALSE)

#read in long_form data
long_form <- read.csv(longName)

#Nests data for common_name
nestForDY <- long_form |>
  group_by(common_name, year) |>
  mutate(count = sum(count)) |>
  unique() |>
  group_by(common_name) |>
  nest()

#Calculates delta Y through nests 
nestForDY$data <- map(nestForDY$data, ~ {
  .x %>%
    mutate(
      count = as.numeric(count),
      previous_count = lag(count),
      yoy_change = count - previous_count
    ) |>
    replace_na(list(yoy_change = 0))
})

#unnests data
delta_y_change <- nestForDY |>
  unnest(data)

#Removes irrelevant columns
delta_y_change$previous_count <- NULL
delta_y_change$count <- NULL

delta_y_change <- delta_y_change |>
  filter(year != 1999)
  
  
#CHANGES MADE HERE
write.csv(delta_y_change, deltaYLong, row.names = FALSE)

#Pivots data to wide form
df_pivot <- delta_y_change %>%
  pivot_wider(names_from = common_name, values_from = yoy_change, values_fill = 0)

#To csv file
write.csv(df_pivot, deltaYName, row.names = FALSE)





