# Created 01/29/2026
# Last updated 03/25/2026 by Anneliese Pinnell

# The purpose of this file is to merge mBBS data and the predicted route counts
# required columns = common_name, count, and year
# Added - set mBBS to be by effort hour (3 min x 20 stops = 60 mins --> 34 routes)

library(dplyr)
library(tidyr)
library(testthat)

options(warn = -1)

#function to filter the mbbs to just the species that we want
filter_to_min_sightings <- function(mbbs, min_sightings_per_route = 9, min_num_routes = 5) {
  
  #filter mbbs so we only have records where count is not 0
  mbbs <- mbbs |>
    dplyr::filter(count > 0)
  
  #set up for for loop
  occurances <- mbbs |> ungroup() |> count(common_name, route) |> arrange(n)
  allspecies <- unique(mbbs$common_name)
  temp_occurances <- occurances |> filter(common_name == "Northern Bobwhite") #temp for use in for loop
  temp_num <- n_distinct(temp_occurances$route) #for use in for loop, really this is also the nrow(temp_occurances) but that's ok.
  
  #for loop to filter species that haven't been seen enough, the minimum number of times on a minimum number of routes
  for (s in 1:length(allspecies)) {
    
    temp_occurances <- occurances |> filter(common_name == allspecies[s])
    temp_num <- n_distinct(temp_occurances$route)
    
    if(temp_num >= min_num_routes) { #this species has been seen on the minimum number of routes
      #check that the species has been seen the minimum number of TIMES on those routes
      #so, count the n values over min_sightings_per_route
      temp_num <- sum(temp_occurances$n >= min_sightings_per_route)
      if(temp_num >= min_num_routes) {
        #do nothing, the species meet the minimum sighting requirements and should stay in the route
      } else {
        #the species does not meet the minimum sighting requirements and should be removed from analysis
        mbbs <- mbbs |> filter(common_name != temp_occurances$common_name[1]) #remove species from datatable
      }
      
    } else { #this species hasn't been seen on the minimum number of routes required for analysis
      mbbs <- mbbs |> filter(common_name != temp_occurances$common_name[1]) #remove species from datatable
    }
  }
  
  #beepr::beep()
  return(mbbs)
  
}

mbbs <- read.csv("data/mbbs/mbbs_route_counts.csv") |>
  filter_to_min_sightings(min_sightings_per_route = 9, min_num_routes = 5) |>
  dplyr::select(year, common_name, count)

predicted <- read.csv("data/mbbs/predictedRouteValues.csv") |>
  #trim predicted sp to just ones of interest
  filter(common_name %in% unique(mbbs$common_name)) |>
  dplyr::select(year, common_name, count) 

mergedDF <- rbind(mbbs, predicted)

# Divide all values by effort hours
# Each route has 20, 3 min surveys (60min), so divide by # routes
mergedDF <- mergedDF |>
  mutate(count = count/34)
  
write.csv(mergedDF, file = "data/mbbs/mbbsMerged.csv", row.names = FALSE)

# Testing!
testFile1 <- read.csv("data/testingData/1combineMBBSTest1.csv") |>
                   filter_to_min_sightings(min_sightings_per_route = 1, min_num_routes = 1) |>
                   dplyr::select(year, common_name, count)

testFile2 <- read.csv("data/testingData/1combineMBBSTest2.csv") |>
                   filter_to_min_sightings(min_sightings_per_route = 1, min_num_routes = 1) |>
                   dplyr::select(year, common_name, count)

merged <- rbind(testFile2, testFile1)
expectedFile <- read.csv("data/testingData/1combineMBBSExpected.csv")


if(test_that("testing output matches expected output", 
             expect_equal(merged, expectedFile, tolerance = 0.0001))){
  beepr::beep(4)
}else{
  beepr::beep(9)
}
