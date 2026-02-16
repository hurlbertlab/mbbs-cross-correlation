# Created 01/29/2026
# Last updated 01/29/2026 by Anneliese Pinnell

#This file uses a linear model to predict bird populations for missing years by
# route. These are exported seperately from main data. Outputs 'done' when 
# finished running. Exports as "data/mbbs/predictedRouteValues.csv"

require(dplyr)
require(tidyr)
require(ggplot2)
require(tibble)
require(purrr)
require(broom)

mbbs <- read.csv("data/mbbs/mbbs_route_counts.csv")

uniqueRouteYears <- unique(mbbs[c("year", "route")])
# DELETE uniqueRoutes <- unique(mbbs["route"])

## Creates T/F matrix to determine if data is missing for the route that year
#Years as rows, routes as columns
result <- uniqueRouteYears|>
  mutate(temp = TRUE)|>
  #Years as rows, routes as columns
  pivot_wider(
    names_from = route,
    values_from = temp,
    values_fill = FALSE # Missing routes are FALSE
  )

#Assigns index to be years instead of r assigned indexes
result <- result |> 
  column_to_rownames(var = "year")

#List of all possible years for mBBS
years <- as.character(1999:2025)

#Empty dataframe for missing data to be added to
allAdded <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(allAdded) <- c("route", "year", "common_name", 
                               "count", "slope", "intercept")

#Iterates through year --> routeName --> individual species
#Creates linear model for each species and predicts accordingly
for (yearNum in years){
  for (routeName in colnames(result)){
    intersection <- result[yearNum, routeName]
    if(intersection == FALSE){
      tempGrouped <- mbbs |>
        filter(route == routeName)
      #By route
      allSpeciesLinearInRoute <- tempGrouped |>
        group_by(common_name) |>
        nest() |> #makes each species have its own list of occurrences
        mutate(model_summary = map(data, ~ {
          lm_fit <- lm(count ~ year, data = .x) #count is y, year is x
          tidy(lm_fit)  #Get tidy summary of the model instead of lm object
        })) |>
        unnest(model_summary) |>
        select(common_name, term, estimate) |>
        pivot_wider(names_from = term, values_from = estimate) |>
        rename(intercept = "(Intercept)", slope = year)

      for (species in allSpeciesLinearInRoute$common_name){
        #By Species
        slope <- allSpeciesLinearInRoute[allSpeciesLinearInRoute$common_name == species,]$slope
        intercept <- allSpeciesLinearInRoute[allSpeciesLinearInRoute$common_name == species,]$intercept

        #Calculates number of individuals
        calculatedValue <- as.integer((slope*as.integer((yearNum))+intercept)+0.5)
        calculatedValue

        #Creates a dataframe (one line) for the species with updated count
        speciesUpdated <- data.frame("route" = routeName, "year" = yearNum, "common_name" = species, "count" = calculatedValue,
                                     "slope" = slope, "intercept" = intercept)
        allAdded <- rbind(allAdded, speciesUpdated)
      }
    }
 }
}
print("Done!")

write.csv(allAdded, file = "data/mbbs/predictedRouteValues.csv", row.names = FALSE)
