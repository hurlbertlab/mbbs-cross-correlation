# Created 01/29/2026
# Last updated 01/29/2026 by Anneliese Pinnell

# The purpose of this file is to merge mBBS data and the predicted route counts
# required columns = common_name, count, and year

library(dplyr)
library(tidyr)

#function to filter the mbbs to just the species that we want
filter_to_min_sightings <- function(mbbs, min_sightings_per_route = 9, min_num_routes = 5) {
  
  #filter mbbs so we only have records where count is not 0
  mbbs <- mbbs %>%
    dplyr::filter(count > 0)
  
  #set up for for loop
  occurances <- mbbs %>% ungroup() %>% count(common_name, route) %>% arrange(n)
  allspecies <- unique(mbbs$common_name)
  temp_occurances <- occurances %>% filter(common_name == "Northern Bobwhite") #temp for use in for loop
  temp_num <- n_distinct(temp_occurances$route) #for use in for loop, really this is also the nrow(temp_occurances) but that's ok.
  
  #for loop to filter species that haven't been seen enough, the minimum number of times on a minimum number of routes
  for (s in 1:length(allspecies)) {
    
    temp_occurances <- occurances %>% filter(common_name == allspecies[s])
    temp_num <- n_distinct(temp_occurances$route)
    
    if(temp_num >= min_num_routes) { #this species has been seen on the minimum number of routes
      #check that the species has been seen the minimum number of TIMES on those routes
      #so, count the n values over min_sightings_per_route
      temp_num <- sum(temp_occurances$n >= min_sightings_per_route)
      if(temp_num >= min_num_routes) {
        #do nothing, the species meet the minimum sighting requirements and should stay in the route
      } else {
        #the species does not meet the minimum sighting requirements and should be removed from analysis
        mbbs <- mbbs %>% filter(common_name != temp_occurances$common_name[1]) #remove species from datatable
      }
      
    } else { #this species hasn't been seen on the minimum number of routes required for analysis
      mbbs <- mbbs %>% filter(common_name != temp_occurances$common_name[1]) #remove species from datatable
    }
  }
  
  #beepr::beep()
  return(mbbs)
  
}

mbbs <- read.csv("data/mbbs/mbbs_route_counts.csv") %>%
  filter_to_min_sightings(min_sightings_per_route = 9, min_num_routes = 5) %>%
  dplyr::select(year, common_name, count)
predicted <- read.csv("data/mbbs/predictedRouteValues.csv") %>%
  #trim predicted sp to just ones of interest
  filter(common_name %in% unique(mbbs$common_name)) %>%
  dplyr::select(year, common_name, count)

mergedDF <- rbind(mbbs, predicted)

write.csv(mergedDF, file = "data/mbbs/mbbsMerged.csv", row.names = FALSE)
