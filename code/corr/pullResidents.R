# Created 02/24/2026 

# The goal of this file is to get the resident species amounts and create a 
# file for them
library(pheatmap)
library(correlation)

spring <- read.csv("data/Spring/SpringLong.csv")
mbbs <- read.csv("data/mbbs/mbbsMerged.csv")
cbc <- read.csv("data/CBCHistoricData/CBCMergedLong.csv")

mbbsSp <- unique(mbbs$common_name)

spring <- spring |>
  filter(common_name %in% mbbsSp)
cbc <- cbc |>
  filter(common_name %in% mbbsSp)

joined <- left_join(spring, cbc, by = c("common_name", "year"))
joined

joined <- joined[complete.cases(joined),] 

longFormData <- joined |>
  mutate(seasonal_change = as.numeric(count.x - count.y)) |>
  select(c("common_name", "year", "seasonal_change")) 

write.csv(longFormData, "data/residents/residentSpeciesLong.csv")

wideFormData <- longFormData |>
  pivot_wider(names_from = common_name, values_from = seasonal_change)

write.csv(wideFormData, "data/residents/residentSpeciesWide.csv", row.names = FALSE)

#Gets delta y
#read in long_form data

#Nests data for common_name
nestForDY <- longFormData |>
  group_by(common_name, year) |>
  mutate(seasonal_change = sum(seasonal_change)) |>
  unique() |>
  group_by(common_name) |>
  nest()

#Calculates delta Y through nests 
nestForDY$data <- map(nestForDY$data, ~ {
  .x %>%
    mutate(
      count = as.numeric(seasonal_change),
      previous_count = lag(seasonal_change),
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

#To csv file
write.csv(delta_y_change, "data/residents/deltaYChangeLong.csv", row.names = FALSE)
delta_y_change$seasonal_change <- NULL
wide_form_data <- pivot_wider(delta_y_change, names_from = common_name, values_from = yoy_change)
write.csv(wide_form_data, "data/residents/deltaYChangeWide.csv", row.names = FALSE)