# Created 11/20/2025
# Last updated 11/20/2025 by Anneliese Pinnell

#Converts long for mBBS data to wide form
#Generates sums for each year for each species

library(dplyr)
library(tidyr)

makeWideLong <- function(fileName, longName, wideName, deltaYName, pctChangeName, minimum){
  #Read in data
  bird_info = read.csv(fileName)
  
  #need to hash out species included/excluded basis
  #Exclude hawks and owls, waterbirds, and categories that are not species-specific.
  excluded_species <- c("Red-shouldered Hawk", "Killdeer", "Great Blue Heron", "Canada Goose", "Turkey Vulture", "Black Vulture", "crow sp.","duck sp.","hawk sp.","passerine sp.", "swallow sp.","waterfowl sp.","woodpecker sp.", "Summer/Scarlet Tanager", "Sharp-shinned/Cooper's Hawk", "Mute Swan", "Mississippi Kite", "Mallard", "Green Heron","Great Horned Owl", "Great Egret", "Eastern Screech-Owl", "Double-crested Cormorant", "Cooper's Hawk" , "Sharp-shinned Hawk", "Broad-winged Hawk", "Belted Kingfisher", "Barred Owl", "American/Fish Crow", "Accipitrine hawk sp.", "Yellow-crowned Night Heron", "Wood Duck", "Osprey", "Bald Eagle", "Red-tailed Hawk")
  
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
  
  #Import long form
  long_form = read.csv(longName)
  
  #Determines delta y change per year
  delta_y_change <- long_form %>%
    mutate(
      previous_count = dplyr::lag(as.numeric(count)),
      yoy_change = as.numeric(count) - as.numeric(previous_count)
    ) %>%
    replace_na(list(yoy_change = 0))
  
  #Removes irrelevant columns
  delta_y_change$previous_count <- NULL
  delta_y_change$count <- NULL
  
  #Pivots data to wide form
  df_pivot <- delta_y_change %>%
    pivot_wider(names_from = common_name, values_from = yoy_change, values_fill = 0)
  
  #To csv file
  write.csv(df_pivot, deltaYName, row.names = FALSE)

  #Creates % Change
  pct_change <- long_form |>
    group_by(common_name) |>
    mutate(PercentChange = ((count-lag(count))/lag(count))*100) |>
    ungroup()
  
  # Replace all NaNs with 0
  pct_change$PercentChange[is.na(pct_change$PercentChange)] <- 0
  # Replace all infinite values with 100
  pct_change$PercentChange[is.infinite(pct_change$PercentChange)] <- 100
  
  #Pivots data to wide form
  pivotPctChange <- pct_change %>%
    pivot_wider(names_from = common_name, values_from = PercentChange, values_fill = 0)
  write.csv(pivotPctChange, pctChangeName, row.names = FALSE)

}

#Data file name, long name, wide name, delta y name

#CBC
makeWideLong("data/CBCHistoricData/CBCMerged.csv", "data/CBCHistoricData/CBCMergedLong.csv",
             "data/CBCHistoricData/CBCMergedWide.csv", "data/CBCHistoricData/CBCMergedDeltaY.csv", 
             "data/CBCHistoricData/CBCPctChange.csv", 0.1)

#mBBS
makeWideLong("data/mbbs/mbbs_route_counts.csv", "data/mbbs/mbbsLong.csv",
             "data/mbbs/mbbsWide.csv", "data/mbbs/mbbsDeltaYWide.csv", 
             "data/mbbs/mbbsPctChange.csv", 1)

#Spring
makeWideLong("data/Spring/SpringMerged.csv", "data/Spring/SpringLong.csv",
             "data/Spring/SpringWide.csv", "data/Spring/SpringDeltaYWide.csv", 
             "data/Spring/SpringPctChange.csv", 15)

