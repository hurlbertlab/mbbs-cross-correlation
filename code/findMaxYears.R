# Created 02/12/2026
# Last updated 02/12/2026 by Anneliese Pinnell

# The purpose of this file is to determine if there are common peak years for
# mbbs species to explain high pearson, low spearman correlations

library(tidyverse)
library(ggplot2)

#Read in data
deltaY <- read.csv("data/mbbs/mbbsDeltaYWide.csv")

#Create empty dataframe
colNames <- c("com_name", "maxYear", "minYear", "maxNum", "minNum")
maxDF <- as.data.frame(matrix(ncol=length(colNames), nrow=0))
colnames(maxDF) <- colNames


#Skips year column
for (colName in names(deltaY)[-1]){
  spMaxYear <- deltaY$year[which.max(deltaY[[colName]])]
  spMinYear <- deltaY$year[which.min(deltaY[[colName]])]
  spMax <- max(deltaY[[colName]])
  spMin <- min(deltaY[[colName]])

  addDF <- data.frame(com_name = colName, maxYear = spMaxYear,
                      minYear = spMinYear, maxNum = spMax, 
                      minNum = spMin)
  maxDF <- rbind(maxDF, addDF)
}
png(filename = paste("figures/histogram/", "mbbsMaxYears", ".png", sep = ""), 
    width = 1000, height = 500)
hist(maxDF$maxYear, breaks = (2025-1999), main = "Maximum Year Frequency mBBS ΔY (1999-2025)", labels=TRUE)
dev.off()

png(filename = paste("figures/histogram/", "mbbsMinYears", ".png", sep = ""), 
    width = 1000, height = 500)
hist(maxDF$minYear, breaks = (2025-1999), main = "Minimum Year Frequency mBBS ΔY (1999-2025)", labels=TRUE)
dev.off()

