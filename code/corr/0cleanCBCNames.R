# Created 02/16/2026
# Last updated: 02/16/2026 - Anneliese Pinnell

# The goal of this code is to create a file that stores the correct mapped
# name for species for CBC

library(tidyverse)

CBCData <- read.csv("data/CBCHistoricData/CBCMerged.csv")

#toRemove <- c("American Black Duck x Mallard (hybrid)", "Greater/Lesser Scaup", "Dark-eyed Junco x White-throated Sparrow (hybrid)", "Greater/Lesser Yellowlegs")

#Mapping of names to new names
toMap <- c("Green-winged Teal (American)" = "Green-winged Teal", 
          "Great Blue Heron (Blue form)" = "Great Blue Heron",
          "Bohemian/Cedar Waxwing" = "Cedar Waxwing",
          "Northern Flicker (Yellow-shafted)" = "Northern Flicker",
          "Wilson's/Common Snipe" = "Wilson's Snipe",
          "Purple Finch (Eastern)" = "Purple Finch",
          "Dark-eyed Junco (Slate-colored)" = "Dark-eyed Junco",
          "Song Sparrow (melodia/atlantica)" = "Song Sparrow",
          "Fox Sparrow (Red)" = "Fox Sparrow",
          "Eastern Towhee (Red-eyed)" = "Eastern Towhee",
          "Spotted/Eastern Towhee (Rufous-sided Towhee)" = "Eastern Towhee",
          "Common Grackle (Purple)" = "Common Grackle",
          "Rufous/Allen's Hummingbird" = "Rufous Hummingbird",
          "Yellow-rumped Warbler (Myrtle)" = "Yellow-rumped Warbler",
          "Pacific/Winter Wren" = "Winter Wren",
          "Barn Owl (American)" = "Barn Owl",
          "Great Blue Heron (White form)" = "Great Blue Heron",
          "Bullock's/Baltimore Oriole" = "Baltimore Oriole")
keys <- names(toMap)
uniqueNames <- data.frame("common_name" = keys)

#Create dataframe to be used for mapping
uniqueNames$correct_name <- toMap[match(uniqueNames$common_name, names(toMap))]

# Replaces only incorrect names
if ("common_name" %in% colnames(CBCData) && "common_name" %in% colnames(uniqueNames)) {
  # Replace common_name in CBCData with correct_name from uniqueNames
  CBCData2 <- CBCData %>%
    left_join(uniqueNames, by = "common_name") %>%
    mutate(common_name = ifelse(!is.na(correct_name), correct_name, common_name)) %>%
    select(-correct_name)
}

write.csv(CBCData2, "data/CBCHistoricData/CBCMerged.csv", row.names = FALSE)

# TESTING
if(test_that("testing output matches expected output", 
             expect_equal(sum(CBCData2$common_name %in% uniqueNames$common_name), 0))){
  beepr::beep(4)
}else{
  beepr::beep(9)
}


