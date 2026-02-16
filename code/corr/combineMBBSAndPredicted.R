# Created 01/29/2026
# Last updated 01/29/2026 by Anneliese Pinnell

# The purpose of this file is to merge mBBS data and the predicted route counts
# required columns = common_name, count, and year

library(dplyr)
library(tidyr)

mbbs <- read.csv("data/mbbs/mbbs_route_counts.csv")
predicted <- read.csv("data/mbbs/predictedRouteValues.csv")

selectedMBBS <- mbbs[, c("year", "common_name", "count")]
selectedPredicted <- predicted[, c("year", "common_name", "count")]

mergedDF <- rbind(selectedMBBS, selectedPredicted)

write.csv(mergedDF, file = "data/mbbs/mbbsMerged.csv", row.names = FALSE)
