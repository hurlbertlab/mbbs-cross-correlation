# Created 11/20/2025
# Last updated 1/29/2025 by Anneliese Pinnell
  #Added CBC and Spring data
  #Removed Pct change
  #Added delta y long export
  #Added filtering to just difference post 1999

#Converts long data to wide form
#Generates sums for each year for each species

library(dplyr)
library(tidyr)
library(stringr)
library(testthat)
library(tidyverse)

makeWideLong <- function(fileName, longName, wideName, deltaYName, deltaYLong, minimum){
  #Read in data
  bird_info = read.csv(fileName)
  
  #need to hash out species included/excluded basis
  #Exclude hawks and owls, waterbirds, and categories that are not species-specific.
  #Self added: Ring-billed Gull
  excluded_species <- c("Red-shouldered Hawk", "Killdeer", "Great Blue Heron", "Canada Goose", "Turkey Vulture", "Black Vulture", "crow sp.","duck sp.","hawk sp.","passerine sp.", "swallow sp.","waterfowl sp.","woodpecker sp.", "Summer/Scarlet Tanager", "Sharp-shinned/Cooper's Hawk", "Mute Swan", "Mississippi Kite", "Mallard", "Green Heron","Great Horned Owl", "Great Egret", "Eastern Screech-Owl", "Double-crested Cormorant", "Cooper's Hawk" , "Sharp-shinned Hawk", "Broad-winged Hawk", "Belted Kingfisher", "Barred Owl", "American/Fish Crow", "Accipitrine hawk sp.", "Yellow-crowned Night Heron", "Wood Duck", "Osprey", "Bald Eagle", "Red-tailed Hawk",
                        "Ring-billed Gull", "Herring Gull", "American Coot", "Bonaparte's Gull", "Ruddy Duck", "Hooded Merganser", "Horned Grebe", "Bufflehead", "Ring-necked Duck", "Rock Pigeon (Feral Pigeon)", "Greater Yellowlegs", "Common Loon", "American Bittern", "American Black Duck", "Lesser Scaup", "Northern Harrier", "Green-winged Teal", "gull sp.", "Pied-billed Grebe", "Tundra Swan", "Loggerhead Shrike", "sparrow sp.", "Gadwall", "Spotted Sandpiper", "Accipiter sp.", "blackbird sp.", "Merlin", "Lesser Yellowlegs", "Greater Scaup", "Canvasback", "Northern Shoveler", "Common Goldeneye", "American Wigeon", "Wilson's Snipe", "Wild Turkey", "Redhead", "American Kestrel", "Northern Pintail", "Red-breasted Merganser", "Greater/Lesser Scaup", "Hairy Woodpecker", "Evening Grosbeak")
  
  #Only includes data from 1999 and on
  bird_info <- bird_info |>
    filter(year >= 1999)
  
  ##Removes low number birds based off of means
  #MBBS data has already been filtered to species that meet a minimum bound when we made the dataset, so just filter out unreliable species for CBC and Spring
  if (!str_detect(fileName, "bbs")) { 
  #Calculates means
  bird_means <- bird_info |>
    group_by(common_name) |>
    summarise(mean_value = mean(count, na.rm = TRUE))
  
  #Determines birds to keep
  birds_to_keep <- bird_means |>
    filter(mean_value > minimum) |>
    filter(!common_name %in% excluded_species) |>
    pull(common_name)
  
  #Filters main data to only have kept birds
  only_kept_birds <- bird_info |>
    filter(common_name %in% birds_to_keep) |>
    slice(1:n())  #resets index
  } else {
    #Determines birds to keep
    birds_to_keep <- bird_info |>
      filter(!common_name %in% excluded_species)
    only_kept_birds <- birds_to_keep
  }
  
  #Sums species to have a count for each year
  individual_species <- only_kept_birds |>
    group_by(common_name, year) |>
    summarise(count = sum(count), .groups = "drop")
  
  #Long form data
  write.csv(individual_species, longName, row.names = FALSE)
  
  #Pivots data to wide form
  df_pivot <- individual_species |>
    pivot_wider(names_from = common_name, values_from = count, values_fill = 0)
  
  #To csv file
  write.csv(df_pivot, wideName, row.names = FALSE)
  
  #long_form data
  long_form <- individual_species
  
  #Nests data for common_name
  nestForDY <- long_form |>
    group_by(common_name, year) |>
    mutate(count = sum(count)) |>
    unique() |>
    group_by(common_name) |>
    nest()
  
  #Calculates delta y
  nestForDY$data <- map(nestForDY$data, ~ {
    diffVec <- c(0, diff(.x$count))
    .x |>
      mutate(yoy_change = diffVec)
  })
  
  #unnests data
  delta_y_change <- nestForDY |>
    unnest(data)
  
  #Removes irrelevant columns
  delta_y_change$previous_count <- NULL
  delta_y_change$count <- NULL
  
  delta_y_change <- delta_y_change |>
    filter(year != 1999)
  
  write.csv(delta_y_change, deltaYLong, row.names = FALSE)
  
  #Pivots data to wide form
  df_pivot <- delta_y_change |>
    pivot_wider(names_from = common_name, values_from = yoy_change, values_fill = 0)
  
  #To csv file
  write.csv(df_pivot, deltaYName, row.names = FALSE)

}

#Data file name, long name, wide name, delta y name

#CBC
makeWideLong(fileName = "data/CBCHistoricData/CBCMerged.csv",
             longName = "data/CBCHistoricData/CBCMergedLong.csv",
             wideName = "data/CBCHistoricData/CBCMergedWide.csv", 
             deltaYName = "data/CBCHistoricData/CBCMergedDeltaY.csv",
             deltaYLong = "data/CBCHistoricData/CBCDeltaYLong.csv", 
             minimum = 0)

#mBBS
makeWideLong(fileName = "data/mbbs/mbbsMerged.csv", 
             longName = "data/mbbs/mbbsLong.csv",
             wideName = "data/mbbs/mbbsWide.csv", 
             deltaYName = "data/mbbs/mbbsDeltaYWide.csv", 
             deltaYLong = "data/mbbs/mbbsDeltaYLong.csv",
             minimum = 1)

# Testing
makeWideLong(fileName = "data/testingData/2createWideLongTest.csv",
             longName = "data/testingData/2CWLLong.csv",
             wideName = "data/testingData/2CWLWide.csv",
             deltaYName = "data/testingData/2CWLDYWide.csv",
             deltaYLong = "data/testingData/2CWLDYLong.csv",
             minimum = 1)

testCWLWide <- read.csv("data/testingData/2CWLWide.csv")
expectedCWLWide <- read.csv("data/testingData/2CWLWideExpected.csv")

testCWLLong <- read.csv("data/testingData/2CWLLong.csv")
expectedCWLLong <- read.csv("data/testingData/2CWLLongExpected.csv")

testCWLDYWide <- read.csv("data/testingData/2CWLDYWide.csv")
expectedCWLDYWide <- read.csv("data/testingData/2CWLDYWideExpected.csv")

testCWLDYLong <- read.csv("data/testingData/2CWLDYLong.csv")
expectedCWLDYLong <- read.csv("data/testingData/2CWLDYLongExpected.csv")

if(test_that("expect equal outputs",{
  expect_equal(testCWLWide, expectedCWLWide, tolerance = 0.0001)
  expect_equal(testCWLLong, expectedCWLLong, tolerance = 0.0001)
  expect_equal(testCWLDYWide, expectedCWLDYWide, tolerance = 0.0001)
  expect_equal(testCWLDYLong, expectedCWLDYLong, tolerance = 0.0001)
})){
  beepr::beep(4)
}else{
  beepr::beep(9)
}

